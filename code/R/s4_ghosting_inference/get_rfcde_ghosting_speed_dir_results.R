# PURPOSE: Generate comparisons of player performance based on ghosting
#          coordinates as well as summaries of variable importance across
#          the LOWO RFCDE models.

library(tidyverse)
library(RFCDE)

# Load model data ---------------------------------------------------------

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

# Create the final list of variables to use:
yac_model_vars <- c(bc_var_names, play_context_var_names,
                    unlist(def_var_name_list[1:2])) %>%
  str_subset("adj_y_change$", negate = TRUE) %>%
  str_subset("adj_y_change_to_qb$", negate = TRUE) %>%
  str_subset("dir_target_endzone$", negate = TRUE)


# Generate LOWO RFCDE model list ------------------------------------------

lowo_rfcde_models <- lapply(1:17,
                            function(week_i) {
                              # Init the training data
                              train_data_matrix <- model_data %>%
                                filter(week_id != week_i) %>%
                                dplyr::select(yac_model_vars) %>%
                                # Convert to matrix
                                as.matrix()

                              # Get the indices of the complete cases to use
                              train_data_use_i <- complete.cases(train_data_matrix)
                              train_data_matrix <- train_data_matrix[train_data_use_i,]

                              # Train data response:
                              train_data_resp <- model_data %>%
                                filter(week_id != week_i) %>%
                                pull(end_x_change)
                              train_data_resp <- train_data_resp[train_data_use_i]

                              # Fit the model:
                              train_rfcde <- RFCDE(train_data_matrix, train_data_resp)
                              return(train_rfcde)

                            })




# Generate initial YAC distributions  -------------------------------------

delta_yards <- 0.5
init_rfcde_pred_yac_data <-
  map_dfr(1:17,
          function(week_i) {

            # Get the matrix for the holdout week:
            week_data <- model_data %>%
              filter(week_id == week_i)
            week_matrix <- week_data %>%
              dplyr::select(yac_model_vars) %>%
              # Convert to matrix
              as.matrix()

            # Get the indices of the complete cases to use
            week_use_i <- complete.cases(week_matrix)
            week_matrix <- week_matrix[week_use_i,]
            week_data <- week_data[week_use_i,]

            map_dfr(1:nrow(week_matrix),
                    function(play_i) {

                      # what's the maximum possible distance the ball carrier can travel
                      # and round up:
                      max_possible_gain <- round(week_matrix[[play_i, "adj_bc_x"]])

                      # Now make a grid of values given the minimum observed in the whole
                      # data in increments of half yards to start:
                      gain_predict_grid <- seq(round(min(model_data$end_x_change)),
                                               max_possible_gain, by = delta_yards)

                      # Generate the CDE prediction:
                      cde_pred <- predict(lowo_rfcde_models[[week_i]],
                                          week_matrix[play_i,],
                                          "CDE", gain_predict_grid)

                      # Convert this to a long dataset where for
                      # each observation we have the predicted yards
                      # gained with its density estimate
                      tibble(pred_yards_gain = gain_predict_grid,
                             play_cde = as.numeric(cde_pred)) %>%
                        # Add column for predicted CDF:
                        mutate(play_cdf = cumsum(play_cde / sum(play_cde)),
                               # Finally with the test row index and observed yards gained:
                               game_play_id = week_data$game_play_id[play_i],
                               obs_yards_gain = week_data$end_x_change[play_i])
                    }) %>%
              mutate(week_id = week_i)

          })

# Compute basic summaries of distributions --------------------------------

# For each play, compute some basic summaries such as the probability of a
# first down, touchdown, positive yardage, also the expected yards gained:
pred_yac_distr_summary <- init_rfcde_pred_yac_data %>%
  # First join the first down and ball carrier position values:
  dplyr::left_join(dplyr::select(model_data, game_play_id, adj_bc_x,
                                 adj_bc_x_from_first_down),
                   by = "game_play_id") %>%
  # Create indicator variables denoting if the value exceeds the two markers:
  mutate(reach_td = as.numeric(pred_yards_gain >= adj_bc_x),
         reach_first_down = as.numeric(pred_yards_gain >= adj_bc_x_from_first_down)) %>%
  group_by(game_play_id) %>%
  summarize(expected_yac = sum(pred_yards_gain * (play_cde / sum(play_cde)), na.rm = TRUE),
            prob_td = ifelse(any(reach_td == 0),
                             1 - max(play_cdf[which(reach_td == 0)]),
                             1),
            prob_first_down = ifelse(any(reach_first_down == 0),
                                     1 - max(play_cdf[which(reach_first_down == 0)]),
                                     1),
            prob_positive_yac = 1 - play_cdf[which(pred_yards_gain == 0)]) %>%
  # Convert the negative probs due to rounding
  mutate(prob_td = pmax(prob_td, 0),
         prob_first_down = pmax(prob_first_down, 0),
         prob_positive_yac = pmax(prob_positive_yac, 0)) %>%
  # Join the yards from first down marker back to include a variable denoting
  # if the first down was already achieved based on the reception:
  dplyr::left_join(dplyr::select(model_data, game_play_id, adj_bc_x_from_first_down),
                   by = "game_play_id") %>%
  mutate(is_first_down_at_catch = as.numeric(adj_bc_x_from_first_down <= 0)) %>%
  dplyr::select(-adj_bc_x_from_first_down)


# Load ghosting and player information ------------------------------------

# Load the ghosting data
ghosting_data <-
  read_csv("data/ghosting_output/ghosts_at_catch_speed_dir.csv")

# Next load the original weekly data information containing info on the players
# involved - to join to this ghosting data
def_player_data <-
  map_dfr(1:17,
          function(week_i) {

            read_rds(paste0("data/model_data/weekly/week",
                            week_i, ".rds")) %>%
              filter(is_start_bc == 1)
          }) %>%
  dplyr::select(game_play_id,
                paste0("defense_", 1:4, "_displayName"),
                paste0("defense_", 1:4, "_nflId"))

ghosting_data <- ghosting_data %>%
  inner_join(def_player_data, by = "game_play_id")

# Convert this to a longer dataset:
long_ghosting_data <-
  map_dfr(1:4,
          function(player_rank_i) {
            player_rank_data <- ghosting_data %>%
              dplyr::select(game_play_id,
                            # Then the necessary defense player columns to change
                            paste0("defense_", player_rank_i, "_",
                                   c("displayName", "nflId", "x_ghost", "y_ghost")))
            colnames(player_rank_data) <- str_remove(colnames(player_rank_data),
                                                     paste0("defense_", player_rank_i,
                                                            "_"))
            player_rank_data %>%
              mutate(old_rank = player_rank_i) %>%
              mutate(new_adj_x = 110 - x_ghost,
                     # Next the y so that 0 indicates middle of field  (from QB POV)
                     # while > 0 indicates left side and < 0 indicates right side
                     new_adj_y = y_ghost - (160 / 6))
          }) %>%
  # Remove rows with missing player IDs
  filter(!is.na(nflId))


# Convert wide model data to long for updating ----------------------------

long_def_model_data <-
  map_dfr(1:4,
          function(player_rank_i) {
            player_rank_data <- model_data %>%
              dplyr::select(game_play_id, week_id, adj_bc_x, adj_bc_y,
                            # Then the necessary defense player columns to change
                            unlist(def_var_name_list[player_rank_i]))
            colnames(player_rank_data) <- str_remove(colnames(player_rank_data),
                                                     paste0("defense_", player_rank_i,
                                                            "_"))
            player_rank_data %>%
              mutate(old_rank = player_rank_i) %>%
              dplyr::rename(old_adj_x = adj_x, old_adj_y = adj_y)
          }) %>%
  # Only keep plays in ghost data
  filter(game_play_id %in% unique(long_ghosting_data$game_play_id))

# Generating ghosting differences for every player ------------------------

# Proceed through each week and then each player to generate new predictions
# for comparison based on the ghosting data using LOWO RFCDE models
ghost_yac_distr_summary <-
  map_dfr(1:17,
          function(week_i) {

            #week_i <- 12
            #print(paste0("STARTING WEEK ", week_i))

            # First init the week data only using its plays and players
            week_plays <- long_def_model_data %>%
              filter(week_id == week_i) %>%
              pull(game_play_id) %>% unique()

            week_long_def_model_data <- long_def_model_data %>%
              filter(week_id == week_i)

            week_ghosting_data <- long_ghosting_data %>%
              filter(game_play_id %in% week_plays)

            # Get the week candidate players
            candidate_player_ids <- unique(week_ghosting_data$nflId)

            # Generate the ghost results for each player in this week
            week_ghost_summary <-
              map_dfr(candidate_player_ids,
                      function(player_id) {

                        #player_id <- 2552385
                        #print(player_id)

                        # Filter to the plays with this player:
                        candidate_plays <- week_ghosting_data %>%
                          filter(nflId == player_id) %>%
                          pull(game_play_id) %>% unique()
                        player_name <- week_ghosting_data %>%
                          filter(nflId == player_id) %>%
                          pull(displayName) %>% unique()

                        long_plays_data <- week_long_def_model_data %>%
                          filter(game_play_id %in% candidate_plays)

                        # Now join the new positions for the player for each play:
                        wide_plays_data <- long_plays_data %>%
                          dplyr::left_join(week_ghosting_data %>%
                                             filter(nflId == player_id) %>%
                                             dplyr::select(game_play_id, old_rank,
                                                           new_adj_x, new_adj_y),
                                           by = c("game_play_id", "old_rank")) %>%
                          # now update the variables from before:
                          mutate(adj_x = ifelse(is.na(new_adj_x), old_adj_x, new_adj_x),
                                 adj_y = ifelse(is.na(new_adj_y), old_adj_y, new_adj_y),
                                 dist_to_bc = ifelse(is.na(new_adj_x), dist_to_bc,
                                                     sqrt((adj_x - adj_bc_x)^2 +
                                                            (adj_y - adj_bc_y)^2)),
                                 adj_x_change = adj_bc_x - adj_x,
                                 adj_y_change = adj_bc_y - adj_y,
                                 adj_y_change_absval = abs(adj_y_change),
                                 # Keep the direction with respect to target endzone as the same
                                 # for now - NOTE may want to change this later!
                                 # Compute the angle between the ball-carrier and the player:
                                 angle_with_bc = (atan2(adj_y_change, -adj_x_change) * 180) / pi,
                                 # Now compute the direction with respect to the ball-carrier as
                                 # simply the absolute minimum difference - this will be the minimum
                                 # across a few possible scenarios to deal 0 to 360 limits
                                 dir_wrt_bc_diff = pmin(
                                   pmin(abs(angle_with_bc - dir_target_endzone),
                                        abs(angle_with_bc - (dir_target_endzone - 360))),
                                   abs(angle_with_bc - (dir_target_endzone + 360)))) %>%
                          # Drop the ones I no longer need:
                          dplyr::select(-old_adj_x, -old_adj_y, -angle_with_bc, -old_rank,
                                        -adj_bc_x, -adj_bc_y, -new_adj_x, -new_adj_y,
                                        -week_id) %>%
                          # Now group by the play to sort the players
                          group_by(game_play_id) %>%
                          # Sort by closest distance:
                          arrange(dist_to_bc) %>%
                          # Make a column denoting this order:
                          mutate(player_dist_bc_rank = 1:n()) %>%
                          # Finally ungroup:
                          ungroup() %>%
                          # Convert back to a wide dataset
                          mutate(player_type = paste0("defense_", player_dist_bc_rank)) %>%
                          dplyr::select(-player_dist_bc_rank) %>%
                          # Now convert to wider dataset using the player_type as the name:
                          pivot_wider(names_from = player_type,
                                      values_from = dist_to_bc:adj_y,
                                      names_glue = "{player_type}_{.value}")

                        # Now want to join these updated covariates back with the old ones
                        # to then get a new dataset to generate predictions for:
                        new_model_data <- model_data %>%
                          filter(game_play_id %in% candidate_plays) %>%
                          dplyr::select(game_play_id,
                                        setdiff(colnames(model_data),
                                                colnames(wide_plays_data))) %>%
                          inner_join(wide_plays_data, by = "game_play_id")

                        # Now proceed to create the matrix for generating predictions for
                        new_data_matrix <- new_model_data %>%
                          # Use the above set of model variables
                          dplyr::select(yac_model_vars) %>%
                          # Convert to matrix
                          as.matrix()
                        # Get the indices of the complete cases to use
                        new_data_use_i <- complete.cases(new_data_matrix)
                        new_model_data <- new_model_data[new_data_use_i,]

                        if (sum(as.numeric(new_data_use_i)) == 1) {
                          new_data_matrix <- as.matrix(t(new_data_matrix[new_data_use_i,]))
                        } else {
                          new_data_matrix <- new_data_matrix[new_data_use_i,]
                        }

                        # New data response:
                        new_data_resp <- new_model_data %>%
                          pull(end_x_change)
                        new_data_resp <- new_data_resp[new_data_use_i]

                        # Generate the new RFCDE distribution predictions for each play:

                        if (nrow(new_data_matrix) > 0) {
                          delta_yards <- 0.5
                          new_rfcde_pred_yac <-
                            map_dfr(1:nrow(new_data_matrix),
                                    function(play_i) {

                                      # what's the maximum possible distance the ball carrier can travel
                                      # and round up:
                                      max_possible_gain <- round(new_data_matrix[[play_i, "adj_bc_x"]])

                                      # Now make a grid of values given the minimum observed in the whole
                                      # data in increments of half yards to start:
                                      gain_predict_grid <- seq(round(min(model_data$end_x_change)),
                                                               max_possible_gain, by = delta_yards)

                                      # Generate the CDE prediction:
                                      cde_pred <- predict(lowo_rfcde_models[[week_i]],
                                                          new_data_matrix[play_i,],
                                                          "CDE", gain_predict_grid)

                                      # Convert this to a long dataset where for
                                      # each observation we have the predicted yards
                                      # gained with its density estimate
                                      tibble(pred_yards_gain = gain_predict_grid,
                                             play_cde = as.numeric(cde_pred)) %>%
                                        # Add column for predicted CDF:
                                        mutate(play_cdf = cumsum(play_cde / sum(play_cde)),
                                               # Finally with the test row index and observed yards gained:
                                               game_play_id = new_model_data$game_play_id[play_i],
                                               obs_yards_gain = new_data_resp[play_i])
                                    })

                          # Summarize these:
                          new_rfcde_distr_summary <- new_rfcde_pred_yac %>%
                            # First join the first down and ball carrier position values:
                            dplyr::left_join(dplyr::select(new_model_data, game_play_id, adj_bc_x,
                                                           adj_bc_x_from_first_down),
                                             by = "game_play_id") %>%
                            # Create indicator variables denoting if the value exceeds the two markers:
                            mutate(reach_td = as.numeric(pred_yards_gain >= adj_bc_x),
                                   reach_first_down = as.numeric(pred_yards_gain >= adj_bc_x_from_first_down)) %>%
                            group_by(game_play_id) %>%
                            summarize(new_expected_yac = sum(pred_yards_gain * (play_cde / sum(play_cde)), na.rm = TRUE),
                                      new_prob_td = ifelse(any(reach_td == 0),
                                                           1 - max(play_cdf[which(reach_td == 0)]),
                                                           1),
                                      new_prob_first_down = ifelse(any(reach_first_down == 0),
                                                                   1 - max(play_cdf[which(reach_first_down == 0)]),
                                                                   1),
                                      new_prob_positive_yac = 1 - play_cdf[which(pred_yards_gain == 0)]) %>%
                            # Convert the negative probs due to rounding
                            mutate(new_prob_td = pmax(new_prob_td, 0),
                                   new_prob_first_down = pmax(new_prob_first_down, 0),
                                   new_prob_positive_yac = pmax(new_prob_positive_yac, 0))

                          # Now join the old results over to then compute the differences
                          # between, returning a dataset for the player where each row corresponds
                          # to a single play and the columns indicate the change in values
                          # based on their actual positions versus ghost positions:
                          result <- new_rfcde_distr_summary %>%
                            inner_join(pred_yac_distr_summary, by = "game_play_id") %>%
                            mutate(delta_expected_yac = expected_yac - new_expected_yac,
                                   delta_prob_td = prob_td - new_prob_td,
                                   delta_prob_first_down = prob_first_down - new_prob_first_down,
                                   delta_prob_positive_yac = prob_positive_yac - new_prob_positive_yac) %>%
                            mutate(nfl_id = player_id, player_display_name = player_name)

                        } else {
                          result <- pred_yac_distr_summary %>%
                            filter(game_play_id %in% candidate_plays) %>%
                            mutate(nfl_id = player_id, player_display_name = player_name)
                        }

                        return(result)

                      }) %>%
              mutate(week_id = week_i)

          })

# Save the output:
write_rds(ghost_yac_distr_summary,
          #"data/ghosting_output/player_ghost_speed_dir_yac_distr_summary.rds")
          "data/ghosting_output/player_ghost_speed_dir_yac_distr_summary_upd.rds")



