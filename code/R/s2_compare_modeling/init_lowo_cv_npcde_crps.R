# PURPOSE: Generate LOWO-CV results for univariate conditional density estimation
#          just based solely on the field position

library(tidyverse)
library(np)

# Load modeling data and limit to at catch --------------------------------

model_data <-
  map_dfr(1:17,
          function(week_i) {

            read_rds(paste0("data/model_data/weekly/week",
                            week_i, ".rds")) %>%
              filter(is_start_bc == 1)
          })

# Generate LOWO CV CRPS ---------------------------------------------------

# Generate the LOWO-CV CRPS values using the univariate CDE with adj_bc_x only.
# Start with a delta_yards of 0.5 (should try with 1 as well since that is
# what Lopez used here for CRPS https://www.kaggle.com/c/nfl-big-data-bowl-2020/overview/evaluation)

delta_yards <- 0.5
lowo_cv_summary <-
  map_dfr(1:17,
          function(test_week_i) {

            # Init the training data
            train_data <- model_data %>%
              filter(week_id != test_week_i)

            # train_cde <- npcdens(end_x_change ~ adj_bc_x, data = train_data,
            #                      bwmethod = "cv.ml")
            # Try with just plug-in estimator
            train_cde <- npcdens(end_x_change ~ adj_bc_x, data = train_data,
                                 bwmethod = "normal-reference")

            # Do the same for test data:
            test_data <- model_data %>%
              filter(week_id == test_week_i)

            # Generate the predictions for the test data
            test_data_cde_preds <-
              map_dfr(1:nrow(test_data),
                      function(test_i) {

                        # What's the distance from the endzone:
                        obs_max_x <- test_data$adj_bc_x[test_i]

                        # Create a maximum possible gain with padding of 2 yards
                        max_possible_gain <- round(obs_max_x) + 2

                        # Now make a grid of values given the minimum observed in the whole
                        # data in increments of half yards to start:
                        gain_predict_grid <- seq(round(min(model_data$end_x_change)),
                                                 max_possible_gain, by = delta_yards)

                        # New data input grid:
                        new_data_pred_grid <-
                          expand.grid(adj_bc_x = max_possible_gain,
                                      end_x_change = gain_predict_grid)

                        # Generate the CDE prediction:
                        test_cde_pred <-
                          predict(train_cde, newdata = new_data_pred_grid)

                        # Convert this to a long dataset where for
                        # each observation we have the predicted yards
                        # gained with its density estimate
                        tibble(pred_yards_gain = gain_predict_grid,
                               test_cde = as.numeric(test_cde_pred)) %>%
                          # Cap the pred_yards_gain so the limit is set to be
                          # the obs_max_x:
                          mutate(pred_yards_gain = pmin(pred_yards_gain, obs_max_x)) %>%
                          # Sum the rows in the padding above:
                          group_by(pred_yards_gain) %>%
                          summarize(test_cde = sum(test_cde, na.rm = TRUE)) %>%
                          ungroup() %>%
                          # Add column for predicted CDF:
                          mutate(test_cdf = cumsum(test_cde / sum(test_cde)),
                                 # Finally with the test row index and observed yards gained:
                                 test_row_i = test_i,
                                 obs_yards_gain = test_data$end_x_change[test_i])
                        })
            test_data_cde_preds %>%
              group_by(test_row_i) %>%
              summarize(obs_yards_gain = first(obs_yards_gain),
                        expected_yac = sum(pred_yards_gain *
                                             (test_cde / sum(test_cde)),
                                           na.rm = TRUE),
                        play_crps =
                          mean(#delta_yards *
                                 (test_cdf - as.numeric((pred_yards_gain -
                                                           obs_yards_gain) >= 0))^2)) %>%
              summarize(test_crps = mean(play_crps),
                        test_yac_rmse = sqrt(mean((expected_yac -
                                                     obs_yards_gain)^2))) %>%
              mutate(test_week = test_week_i)
            })

# Save these results ------------------------------------------------------

write_csv(lowo_cv_summary,
          "data/model_output/lowo_cv_results/plugin_npcde_crps_summary_w_padding.csv")



