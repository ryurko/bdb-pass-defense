# PURPOSE: Generate leave-one-week-out cross-validation (LOWO-CV) results for
#          RFCDE models with different levels of context info included with
#          player-level information

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



# Load weekly data corresponding to QB position at release ----------------

qb_release_data <-
  map_dfr(1:17,
          function(week_i) {

            read_rds(paste0("data/input/weekly_at_release_features/week",
                            week_i, ".rds")) %>%
              dplyr::select(gameId, playId, adj_qb_x, adj_qb_y, qb_x, qb_y)
          }) %>%
  distinct()

# Join this information and compute covariates about info with respect to
# the QB such as the ``air yards" and actual distance:
model_data <- model_data %>%
  dplyr::left_join(qb_release_data, by = c("gameId", "playId")) %>%
  mutate(adj_x_change_to_qb = adj_qb_x - adj_bc_x,
         adj_y_change_to_qb = adj_qb_y - adj_bc_y,
         bc_dist_to_qb = sqrt((bc_x - qb_x)^2 + (bc_y - qb_y)^2))

# Load play-level features to join ----------------------------------------

play_level_data <- read_csv("data/model_data/play_level_features.csv")

# Join to the modeling data and then create the variable indicating where the
# ball-carrier is with respect to the first down marker:
model_data <- model_data %>%
  dplyr::left_join(play_level_data, by = c("gameId", "playId")) %>%
  # Positive values indicate yards to first down / goal line while negative indicates
  # the ball-carrier is beyond the first down marker
  mutate(adj_bc_x_from_first_down = adj_bc_x - adj_x_first_down)


# Initialize different sets of candidate covariates -----------------------

# Start with bc variables:
bc_var_names <- c("adj_bc_x", "adj_bc_y", "bc_s", "bc_dis", "bc_dir_target_endzone",
                  "adj_x_change_to_qb", "adj_y_change_to_qb", "bc_dist_to_qb")

# Next make a list of all possible offense and defense player information,
# just going up to three to start
def_var_name_list <-
  lapply(1:4,
         function(def_i) {
           str_subset(colnames(model_data), paste0("defense_", def_i, "_")) %>%
             str_subset("(_dist_to_bc)|(_s)|(_dis$)|(_adj_x)|(_adj_y)|(_dir_target_endzone)|(_dir_wrt_bc_diff)")
         })

# Repeat for offense:
off_var_name_list <-
  lapply(1:4,
         function(off_i) {
           str_subset(colnames(model_data), paste0("offense_", off_i, "_")) %>%
             str_subset("(_dist_to_bc)|(_s)|(_dis$)|(_adj_x)|(_adj_y)|(_dir_target_endzone)|(_dir_wrt_bc_diff)")
         })

# Initial play-level context:
play_context_var_names <-
  c("adj_bc_x_from_first_down",
    setdiff(colnames(play_level_data),
            c("gameId", "playId", "adj_x_first_down", "quarter",
              "qtr_sec_remain", "game_sec_remain")))

# Now create list of candidate covariates:
candidate_var_list <-
  lapply(0:4,
         function(n_other_players) {
           if (n_other_players == 0) {
             c(bc_var_names, play_context_var_names)
           } else {
             c(bc_var_names, play_context_var_names,
               unlist(def_var_name_list[1:n_other_players]),
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

# Save these results ------------------------------------------------------

write_csv(lowo_cv_summary,
          "data/model_output/lowo_cv_results/qb_play_context_rfcde_crps_summary.csv")




