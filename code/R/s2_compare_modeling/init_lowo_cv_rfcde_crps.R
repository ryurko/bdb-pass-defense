# PURPOSE: Generate leave-one-week-out cross-validation (LOWO-CV) results for
#          RFCDE models with different sets of covariates

library(tidyverse)
library(RFCDE)

# Load modeling data and limit to at catch --------------------------------

model_data <-
  map_dfr(1:17,
          function(week_i) {

            read_rds(paste0("data/model_data/weekly/week",
                            week_i, ".rds")) %>%
              filter(is_start_bc == 1)
          })


# Initialize different sets of candidate covariates -----------------------

# Start with bc variables:
bc_var_names <- c("adj_bc_x", "adj_bc_y", "bc_s", "bc_dis", "bc_dir_target_endzone")

# Next make a list of all possible offense and defense player information,
# just going up to three to start
def_var_name_list <-
  lapply(1:3,
         function(def_i) {
           str_subset(colnames(model_data), paste0("defense_", def_i, "_")) %>%
             str_subset("(_dist_to_bc)|(_s)|(_dis$)|(_adj_x)|(_adj_y)|(_dir_target_endzone)|(_dir_wrt_bc_diff)")
         })

# Repeat for offense:
off_var_name_list <-
  lapply(1:3,
         function(off_i) {
           str_subset(colnames(model_data), paste0("offense_", off_i, "_")) %>%
             str_subset("(_dist_to_bc)|(_s)|(_dis$)|(_adj_x)|(_adj_y)|(_dir_target_endzone)|(_dir_wrt_bc_diff)")
         })

# Now create list of candidate covariates:
candidate_var_list <-
  lapply(0:3,
         function(n_other_players) {
           if (n_other_players == 0) {
             bc_var_names
           } else {
             c(bc_var_names, unlist(def_var_name_list[1:n_other_players]),
               unlist(off_var_name_list[1:n_other_players]))
           }
         })


# Generate LOWO CV CRPS ---------------------------------------------------

# For each set of candidate covariates, generate the LOWO-CV CRPS values,
# start with a delta_yards of 0.5 (should try with 1 as well since that is
# what Lopez used here for CRPS https://www.kaggle.com/c/nfl-big-data-bowl-2020/overview/evaluation)

delta_yards <- 0.5
lowo_cv_summary <-
    map_dfr(1:length(candidate_var_list),
          function(var_list_i) {

            # Generate the results holding out each week:
            map_dfr(1:17,
                    function(test_week_i) {

                      # Init the training data
                      train_data_matrix <- model_data %>%
                        filter(week_id != test_week_i) %>%
                        dplyr::select(candidate_var_list[[var_list_i]]) %>%
                        # Convert to matrix
                        as.matrix()

                      # Get the indices of the complete cases to use
                      train_data_use_i <- complete.cases(train_data_matrix)
                      train_data_matrix <- train_data_matrix[train_data_use_i,]

                      # Train data response:
                      train_data_resp <- model_data %>%
                        filter(week_id != test_week_i) %>%
                        pull(end_x_change)
                      train_data_resp <- train_data_resp[train_data_use_i]

                      # Do the same for test data:
                      test_data_matrix <- model_data %>%
                        filter(week_id == test_week_i) %>%
                        dplyr::select(candidate_var_list[[var_list_i]]) %>%
                        # Convert to matrix
                        as.matrix()
                      test_data_use_i <- complete.cases(test_data_matrix)
                      test_data_matrix <- test_data_matrix[test_data_use_i,]


                      # Train data response:
                      test_data_resp <- model_data %>%
                        filter(week_id == test_week_i) %>%
                        pull(end_x_change)
                      test_data_resp <- test_data_resp[test_data_use_i]

                      # Fit the model:
                      train_rfcde <- RFCDE(train_data_matrix, train_data_resp)

                      # Generate the predictions for the test data
                      test_data_cde_preds <-
                        map_dfr(1:nrow(test_data_matrix),
                                function(test_i) {

                                  # what's the maximum possible distance the ball carrier can travel
                                  # and round up:
                                  max_possible_gain <- round(test_data_matrix[[test_i, "adj_bc_x"]])

                                  # Now make a grid of values given the minimum observed in the whole
                                  # data in increments of half yards to start:
                                  gain_predict_grid <- seq(round(min(model_data$end_x_change)),
                                                           max_possible_gain, by = delta_yards)

                                  # Generate the CDE prediction:
                                  test_cde_pred <- predict(train_rfcde, test_data_matrix[test_i,],
                                                           "CDE", gain_predict_grid)

                                  # Convert this to a long dataset where for
                                  # each observation we have the predicted yards
                                  # gained with its density estimate
                                  tibble(pred_yards_gain = gain_predict_grid,
                                         test_cde = as.numeric(test_cde_pred)) %>%
                                    # Add column for predicted CDF:
                                    mutate(test_cdf = cumsum(test_cde * delta_yards),
                                           # Finally with the test row index and observed yards gained:
                                           test_row_i = test_i,
                                           obs_yards_gain = test_data_resp[test_i])
                                })

                      test_data_cde_preds %>%
                        group_by(test_row_i) %>%
                        summarize(play_crps =
                                    mean(delta_yards *
                                           (test_cdf -
                                              as.numeric((pred_yards_gain -
                                                            obs_yards_gain) >= 0))^2)) %>%
                        summarize(test_crps = mean(play_crps)) %>%
                        mutate(test_week = test_week_i)


                    }) %>%
              mutate(n_close_players = var_list_i)

          })


# View the results --------------------------------------------------------

library(ggbeeswarm)
lowo_cv_summary %>%
  mutate(n_close_players = n_close_players - 1) %>%
  ggplot(aes(x = n_close_players, y = test_crps)) +
  geom_beeswarm(color = "darkblue") +
  stat_summary(fun = "mean", color = "darkorange", size = 3, geom = "point") +
  theme_bw() +
  labs(x = "Number of defense / offense players included by distance",
       y = "Holdout CRPS")

lowo_cv_summary %>%
  mutate(n_close_players = n_close_players - 1) %>%
  group_by(n_close_players) %>%
  summarize(med_crps = median(test_crps))


# Save these results ------------------------------------------------------

write_csv(lowo_cv_summary,
          "data/model_output/lowo_cv_results/rfcde_crps_summary.csv")




