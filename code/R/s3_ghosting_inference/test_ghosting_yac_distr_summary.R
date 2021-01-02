# PURPOSE: Test process for generating comparisons of player performance based
#          on ghosting coordinates from Kostas

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


# Fit model using all data  -----------------------------------------------

# Fit model using all of the training data (this may not be the correct decision
# need to think about this more...)

# Will use up to 2 closest def / off players to start
train_data_matrix <- model_data %>%
  dplyr::select(candidate_var_list[[3]]) %>%
  # Convert to matrix
  as.matrix()
# Get the indices of the complete cases to use
train_data_use_i <- complete.cases(train_data_matrix)
train_data_matrix <- train_data_matrix[train_data_use_i,]

# Train data response:
train_data_resp <- model_data %>%
  pull(end_x_change)
train_data_resp <- train_data_resp[train_data_use_i]

# Fit the model:
init_rfcde_fit <- RFCDE(train_data_matrix, train_data_resp)

# Generate YAC distribution for every play --------------------------------

# Given this model, I can start by generating the YAC distribution for each play:
delta_yards <- 0.5
init_rfcde_pred_yac <-
  map_dfr(1:nrow(train_data_matrix),
          function(play_i) {

            # what's the maximum possible distance the ball carrier can travel
            # and round up:
            max_possible_gain <- round(train_data_matrix[[play_i, "adj_bc_x"]])

            # Now make a grid of values given the minimum observed in the whole
            # data in increments of half yards to start:
            gain_predict_grid <- seq(round(min(model_data$end_x_change)),
                                     max_possible_gain, by = delta_yards)

            # Generate the CDE prediction:
            cde_pred <- predict(init_rfcde_fit, train_data_matrix[play_i,],
                                "CDE", gain_predict_grid)

            # Convert this to a long dataset where for
            # each observation we have the predicted yards
            # gained with its density estimate
            tibble(pred_yards_gain = gain_predict_grid,
                   play_cde = as.numeric(cde_pred)) %>%
              # Add column for predicted CDF:
              mutate(play_cdf = cumsum(play_cde / sum(play_cde)),
                     # Finally with the test row index and observed yards gained:
                     game_play_id = model_data$game_play_id[train_data_use_i][play_i],
                     obs_yards_gain = train_data_resp[play_i])
          })


# Compute basic summaries of distributions --------------------------------

# For each play, compute some basic summaries such as the probability of a
# first down, touchdown, positive yardage, also the expected yards gained:
init_rfcde_distr_summary <- init_rfcde_pred_yac %>%
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
         prob_positive_yac = pmax(prob_positive_yac, 0))


# Load the ghosting data --------------------------------------------------

ghosting_data <-
  read_csv("data/ghosting_output/at_catch_ghosts.csv")

# What I really want to do here is repeat this process for every play: I go through
# every player with ghosting coordinates provided by Kostas, and generate the
# new design matrix of player level information for that player - so it preserves
# any changes in the order of distance of players, generate the new RFCDE prediction
# summaries, join the old and compute the differences. The point will be to
# tally up at the end how players affect these summaries across all plays they
# are involved in.


# First step is to convert the wide model data to a long dataset for then replacing
# and updating the distances for the various players - just do this for the
# defense player variables:
long_def_model_data <-
  map_dfr(1:4,
          function(player_rank_i) {
            player_rank_data <- model_data %>%
              dplyr::select(game_play_id, bc_x, bc_y, adj_bc_x, adj_bc_y,
                            playDirection,
                            # Then the necessary defense player columns to change
                            unlist(def_var_name_list[player_rank_i]),
                            paste0("defense_", player_rank_i, "_",
                                            c("displayName", "nflId", "position", "x", "y")))
            colnames(player_rank_data) <- str_remove(colnames(player_rank_data),
                                                     paste0("defense_", player_rank_i,
                                                            "_"))
            player_rank_data %>%
              mutate(old_rank = player_rank_i) %>%
              dplyr::rename(old_x = x, old_y = y)
          })

# Now proceed through every player in the ghosting data, updating all plays that
# they are in to then generate new predictions for comparison:
candidate_player_ids <- unique(ghosting_data$nflId)
# Only use the players in the long_def_model_data:
candidate_player_ids <- intersect(candidate_player_ids,
                                  unique(long_def_model_data$nflId))

# Make a simplified ghosting dataset to use:
adj_ghosting_data <- ghosting_data %>%
  dplyr::select(gameId, playId, nflId, playDirection, ghost_X, ghost_Y) %>%
  unite(game_play_id, c("gameId", "playId")) %>%
  filter(nflId %in% candidate_player_ids) %>%
  rename(new_x = ghost_X, new_y = ghost_Y) %>%
  mutate(new_adj_x = 110 - new_x,
         # Next the y so that 0 indicates middle of field  (from QB POV)
         # while > 0 indicates left side and < 0 indicates right side
         new_adj_y = new_y - (160 / 6)) %>%
  # Only want non-missing new coordiates:
  filter(!is.na(new_adj_x), !is.na(new_adj_y))

# Which plays overlap with the model data I have:
overlap_plays <- intersect(unique(long_def_model_data$game_play_id),
                           unique(adj_ghosting_data$game_play_id))
# Only use these plays:
long_def_model_data <- long_def_model_data %>%
  filter(game_play_id %in% overlap_plays)
adj_ghosting_data <- adj_ghosting_data %>%
  filter(game_play_id %in% overlap_plays)

init_rfcde_ghost_summary <-
  map_dfr(unique(adj_ghosting_data$nflId),
          function(player_id) {

            #player_id <- 2543739
            #print(player_id)

            # Filter to the plays with this player:
            candidate_plays <- long_def_model_data %>%
              filter(nflId == player_id) %>%
              pull(game_play_id) %>% unique()
            player_name <- long_def_model_data %>%
              filter(nflId == player_id) %>%
              pull(displayName) %>% unique()
            # player_position <- long_def_model_data %>%
            #   filter(nflId == player_id) %>%
            #   pull(position) %>% unique()

            long_plays_data <- long_def_model_data %>%
              filter(game_play_id %in% candidate_plays)

            # Now join the new positions for the player for each play:
            wide_plays_data <- long_plays_data %>%
              dplyr::left_join(dplyr::select(adj_ghosting_data, -playDirection),
                               by = c("game_play_id", "nflId")) %>%
              # now update the variables from before:
              mutate(adj_x = ifelse(is.na(new_adj_x), adj_x, new_adj_x),
                     adj_y = ifelse(is.na(new_adj_y), adj_y, new_adj_y),
                     dist_to_bc = ifelse(is.na(new_x), dist_to_bc,
                                         sqrt((new_x - bc_x)^2 + (new_y - bc_y)^2)),
                     adj_x_change = adj_bc_x - adj_x,
                     adj_y_change = adj_bc_y - adj_y,
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
              # Rename the x and y, then drop the ones I no longer need:
              dplyr::rename(x = new_x, y = new_y) %>%
              dplyr::select(-old_x, -old_y, -angle_with_bc, -old_rank,
                            -bc_x, -bc_y, -adj_bc_x, -adj_bc_y, -playDirection,
                            -new_adj_x, -new_adj_y) %>%
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
                          values_from = dist_to_bc:y,
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
              dplyr::select(candidate_var_list[[3]]) %>%
              # Convert to matrix
              as.matrix()
            # Get the indices of the complete cases to use
            new_data_use_i <- complete.cases(new_data_matrix)
            if (length(new_data_use_i) == 1) {
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
                          #play_i <- 1

                          # what's the maximum possible distance the ball carrier can travel
                          # and round up:
                          max_possible_gain <- round(new_data_matrix[[play_i, "adj_bc_x"]])

                          # Now make a grid of values given the minimum observed in the whole
                          # data in increments of half yards to start:
                          gain_predict_grid <- seq(round(min(model_data$end_x_change)),
                                                   max_possible_gain, by = delta_yards)

                          # Generate the CDE prediction:
                          cde_pred <- predict(init_rfcde_fit, new_data_matrix[play_i,],
                                              "CDE", gain_predict_grid)

                          # Convert this to a long dataset where for
                          # each observation we have the predicted yards
                          # gained with its density estimate
                          tibble(pred_yards_gain = gain_predict_grid,
                                 play_cde = as.numeric(cde_pred)) %>%
                            # Add column for predicted CDF:
                            mutate(play_cdf = cumsum(play_cde / sum(play_cde)),
                                   # Finally with the test row index and observed yards gained:
                                   game_play_id = new_model_data$game_play_id[new_data_use_i][play_i],
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
                inner_join(init_rfcde_distr_summary, by = "game_play_id") %>%
                mutate(delta_expected_yac = expected_yac - new_expected_yac,
                       delta_prob_td = prob_td - new_prob_td,
                       delta_prob_first_down = prob_first_down - new_prob_first_down,
                       delta_prob_positive_yac = prob_positive_yac - new_prob_positive_yac) %>%
                mutate(nfl_id = player_id, player_display_name = player_name)
              # ,
              # positon = player_position)

            } else {
              result <- init_rfcde_distr_summary %>%
                filter(game_play_id %in% candidate_plays) %>%
                mutate(nfl_id = player_id, player_display_name = player_name)
            }

            return(result)

          })

# Now summarize the player level:
player_ghost_summary <- init_rfcde_ghost_summary %>%
  group_by(nfl_id, player_display_name) %>%
  summarize(n_plays = n(),
            yac_better_than_ghost = sum(as.numeric(delta_expected_yac < 0)) / n_plays,
            prob_td_better_than_ghost = sum(as.numeric(delta_prob_td < 0)) / n_plays,
            prob_first_down_better_than_ghost = sum(as.numeric(delta_prob_first_down < 0)) / n_plays,
            total_yac_diff = sum(delta_expected_yac, na.rm = TRUE),
            ave_yac_diff = mean(delta_expected_yac, na.rm = TRUE),
            total_prob_td_diff = sum(delta_prob_td, na.rm = TRUE),
            ave_prob_td_diff = mean(delta_prob_td, na.rm = TRUE),
            total_prob_first_down_diff = sum(delta_prob_first_down,
                                             na.rm = TRUE),
            ave_prob_first_down_diff = mean(delta_prob_first_down,
                                            na.rm = TRUE),
            total_prob_pos_yac_diff = sum(delta_prob_positive_yac,
                                             na.rm = TRUE),
            ave_prob_pos_yac_diff = mean(delta_prob_positive_yac,
                                            na.rm = TRUE)) %>%
  ungroup()


# Display a summary of the players:
library(ggrepel)
player_ghost_summary %>%
  filter(n_plays >= 50) %>%
  # Join their positions:
  left_join(ghosting_data %>%
               dplyr::select(nflId, position) %>%
               group_by(nflId) %>%
               summarize(position = first(position)) %>%
               ungroup(), by = c("nfl_id" = "nflId")) %>%
  ggplot(aes(x = ave_yac_diff, y = ave_prob_td_diff, color = position)) +
  geom_point() +
  geom_label_repel(data = player_ghost_summary %>%
                    filter(n_plays >= 50) %>%
                    arrange(total_yac_diff) %>%
                    slice(1:10) %>%
                    # Join their positions:
                    left_join(ghosting_data %>%
                                dplyr::select(nflId, position) %>%
                                group_by(nflId) %>%
                                summarize(position = first(position)) %>%
                                ungroup(), by = c("nfl_id" = "nflId")),
                  aes(label = player_display_name),
                  min.segment.length = 0, box.padding = 0.5) +
  theme_bw() +
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred")


player_ghost_summary %>%
  filter(n_plays >= 50) %>%
  # Join their positions:
  left_join(ghosting_data %>%
              dplyr::select(nflId, position) %>%
              group_by(nflId) %>%
              summarize(position = first(position)) %>%
              ungroup(), by = c("nfl_id" = "nflId")) %>%
  ggplot(aes(x = ave_yac_diff, y = ave_prob_pos_yac_diff, color = position)) +
  geom_point() +
  geom_label_repel(data = player_ghost_summary %>%
                     filter(n_plays >= 50) %>%
                     arrange(total_yac_diff) %>%
                     slice(1:10) %>%
                     # Join their positions:
                     left_join(ghosting_data %>%
                                 dplyr::select(nflId, position) %>%
                                 group_by(nflId) %>%
                                 summarize(position = first(position)) %>%
                                 ungroup(), by = c("nfl_id" = "nflId")),
                   aes(label = player_display_name),
                   min.segment.length = 0, box.padding = 0.5) +
  theme_bw() +
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred")

player_ghost_summary %>%
  filter(n_plays >= 50) %>%
  # Join their positions:
  left_join(ghosting_data %>%
              dplyr::select(nflId, position) %>%
              group_by(nflId) %>%
              summarize(position = first(position)) %>%
              ungroup(), by = c("nfl_id" = "nflId")) %>%
  ggplot(aes(x = total_prob_td_diff, y = total_prob_first_down_diff, color = position)) +
  geom_point() +
  geom_label_repel(data = player_ghost_summary %>%
                     filter(n_plays >= 50) %>%
                     arrange(total_yac_diff) %>%
                     slice(1:10) %>%
                     # Join their positions:
                     left_join(ghosting_data %>%
                                 dplyr::select(nflId, position) %>%
                                 group_by(nflId) %>%
                                 summarize(position = first(position)) %>%
                                 ungroup(), by = c("nfl_id" = "nflId")),
                   aes(label = player_display_name),
                   min.segment.length = 0, box.padding = 0.5) +
  theme_bw() +
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred")




# Quick peak at RFCDE variable importance ---------------------------------

variable_importance(init_rfcde_fit, type = "loss") %>%
  as_tibble() %>%
  mutate(variable_name = names(variable_importance(init_rfcde_fit, type = "loss"))) %>%
  arrange(desc(value)) %>%
  slice(1:20) %>%
  mutate(variable_name = fct_reorder(variable_name, value)) %>%
  ggplot(aes(x = variable_name, y = value)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  coord_flip() +
  theme_bw()

variable_importance(init_rfcde_fit, type = "count") %>%
  as_tibble() %>%
  mutate(variable_name = names(variable_importance(init_rfcde_fit, type = "loss"))) %>%
  arrange(desc(value)) %>%
  slice(1:20) %>%
  mutate(variable_name = fct_reorder(variable_name, value)) %>%
  ggplot(aes(x = variable_name, y = value)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  coord_flip() +
  theme_bw()




