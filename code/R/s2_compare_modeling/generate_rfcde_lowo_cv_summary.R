# PURPOSE: Generate LOWO CV RFCDE results with different sets of potential
#          covariates included - as well as different transformations

library(tidyverse)
library(RFCDE)

# Load the modeling data --------------------------------------------------

model_data <-
  read_rds("data/model_data/at_catch_yac_model_data.rds")


# Initialize different sets of features -----------------------------------

# Start with bc variables:
bc_var_names <- c("adj_bc_x", "adj_bc_y", "bc_s", "bc_dis", "bc_dir_target_endzone",
                  "adj_x_change_to_qb", "adj_y_change_to_qb", "bc_dist_to_qb",
                  "adj_bc_x_from_first_down", "qb_s",
                  "adj_y_change_to_qb_absval", "bc_dir_target_endzone_absval")

# Defensive player level info
def_var_name_list <-
  # erroneous play...
  lapply(1:4,
         function(def_i) {
           str_subset(colnames(model_data), paste0("defense_", def_i, "_")) %>%
             str_subset("(_dist_to_bc)|(_s)|(_dis$)|(_adj_x)|(_adj_y)|(_dir_target_endzone)|(_dir_wrt_bc_diff)")
         })

# Repeat for offense:
off_var_name_list <- # for some reason there is up to 36 likely due to an
  # erroneous play...
  lapply(1:4,
         function(off_i) {
           str_subset(colnames(model_data), paste0("offense_", off_i, "_")) %>%
             str_subset("(_dist_to_bc)|(_s)|(_dis$)|(_adj_x)|(_adj_y)|(_dir_target_endzone)|(_dir_wrt_bc_diff)")
         })

# Initial play-level context:
play_context_var_names <-
  setdiff(colnames(model_data),
          c("week_id", "game_play_id", "end_x_change", bc_var_names,
            unlist(def_var_name_list), unlist(off_var_name_list)))

# Now create list of candidate covariates starting with play level info, then
# adding ball carrier, before adding defensive players. After adding defensive
# players, then make another list with offensive players also included
candidate_var_list_no_abs_val <-
  lapply(-1:8,
         function(n_other_players) {

           if (n_other_players == -1) {
             result <- bc_var_names
           } else if (n_other_players == 0) {
             result <- c(bc_var_names, play_context_var_names)
           } else if (n_other_players > 0 & n_other_players < 5) {
             result <-
               c(bc_var_names, play_context_var_names,
               unlist(def_var_name_list[1:n_other_players]))
           } else {
             result <-
               c(bc_var_names, play_context_var_names,
               unlist(def_var_name_list[1:(n_other_players - 4)]),
               unlist(off_var_name_list[1:(n_other_players - 4)]))
           }
           result %>%
             str_subset("_absval", negate = TRUE) %>%
             return
         })

# List using absolute value:
candidate_var_list_with_abs_val <-
  lapply(-1:8,
         function(n_other_players) {

           if (n_other_players == -1) {
             result <- bc_var_names
           } else if (n_other_players == 0) {
             result <- c(bc_var_names, play_context_var_names)
           } else if (n_other_players > 0 & n_other_players < 5) {
             result <-
               c(bc_var_names, play_context_var_names,
                 unlist(def_var_name_list[1:n_other_players]))
           } else {
             result <-
               c(bc_var_names, play_context_var_names,
                 unlist(def_var_name_list[1:(n_other_players - 4)]),
                 unlist(off_var_name_list[1:(n_other_players - 4)]))
           }
           result %>%
             str_subset("adj_y_change$", negate = TRUE) %>%
             str_subset("adj_y_change_to_qb$", negate = TRUE) %>%
             str_subset("dir_target_endzone$", negate = TRUE) %>%
             return
         })



# Generate LOWO CV CRPS ---------------------------------------------------


# Start without absolute value variables ----------------------------------

# For each set of candidate covariates, generate the LOWO-CV CRPS values,
# start with a delta_yards of 0.5 (should try with 1 as well since that is
# what Lopez used here for CRPS https://www.kaggle.com/c/nfl-big-data-bowl-2020/overview/evaluation)

delta_yards <- 0.5
lowo_cv_summary_no_abs <-
  map_dfr(1:length(candidate_var_list_no_abs_val),
          function(var_list_i) {

            # Generate the results holding out each week:
            map_dfr(1:17,
                    function(test_week_i) {

                      # Init the training data
                      train_data_matrix <- model_data %>%
                        filter(week_id != test_week_i) %>%
                        dplyr::select(candidate_var_list_no_abs_val[[var_list_i]]) %>%
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
                        dplyr::select(candidate_var_list_no_abs_val[[var_list_i]]) %>%
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
                                    mutate(test_cdf = cumsum(test_cde / sum(test_cde)),
                                           # Finally with the test row index and observed yards gained:
                                           test_row_i = test_i,
                                           obs_yards_gain = test_data_resp[test_i])
                                })

                      test_data_cde_preds %>%
                        group_by(test_row_i) %>%
                        summarize(obs_yards_gain = first(obs_yards_gain),
                                  expected_yac = sum(pred_yards_gain *
                                                       (test_cde / sum(test_cde)),
                                                     na.rm = TRUE),
                                  play_crps =
                                    mean(#delta_yards *
                                      (test_cdf -
                                         as.numeric((pred_yards_gain -
                                                       obs_yards_gain) >= 0))^2)) %>%
                        summarize(test_crps = mean(play_crps),
                                  test_yac_rmse = sqrt(mean((expected_yac -
                                                               obs_yards_gain)^2))) %>%
                        mutate(test_week = test_week_i)


                    }) %>%
              mutate(n_close_players = var_list_i)

          })

write_csv(lowo_cv_summary_no_abs,
          "data/model_output/lowo_cv_results/rfcde_crps_summary_no_abs_val.csv")


# Repeat with absolute value variables ------------------------------------


delta_yards <- 0.5
lowo_cv_summary_with_abs <-
  map_dfr(1:length(candidate_var_list_with_abs_val),
          function(var_list_i) {

            # Generate the results holding out each week:
            map_dfr(1:17,
                    function(test_week_i) {

                      # Init the training data
                      train_data_matrix <- model_data %>%
                        filter(week_id != test_week_i) %>%
                        dplyr::select(candidate_var_list_with_abs_val[[var_list_i]]) %>%
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
                        dplyr::select(candidate_var_list_with_abs_val[[var_list_i]]) %>%
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
                                    mutate(test_cdf = cumsum(test_cde / sum(test_cde)),
                                           # Finally with the test row index and observed yards gained:
                                           test_row_i = test_i,
                                           obs_yards_gain = test_data_resp[test_i])
                                })

                      test_data_cde_preds %>%
                        group_by(test_row_i) %>%
                        summarize(obs_yards_gain = first(obs_yards_gain),
                                  expected_yac = sum(pred_yards_gain *
                                                       (test_cde / sum(test_cde)),
                                                     na.rm = TRUE),
                                  play_crps =
                                    mean(#delta_yards *
                                      (test_cdf -
                                         as.numeric((pred_yards_gain -
                                                       obs_yards_gain) >= 0))^2)) %>%
                        summarize(test_crps = mean(play_crps),
                                  test_yac_rmse = sqrt(mean((expected_yac -
                                                               obs_yards_gain)^2))) %>%
                        mutate(test_week = test_week_i)


                    }) %>%
              mutate(n_close_players = var_list_i)

          })

write_csv(lowo_cv_summary_with_abs,
          "data/model_output/lowo_cv_results/rfcde_crps_summary_with_abs_val.csv")









