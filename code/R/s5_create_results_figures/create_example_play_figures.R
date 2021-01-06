# PURPOSE: Create figures demonstrating the change in distribution for an
#          example play with a noticeable shift for the player compared to
#          the ghosting location.

library(tidyverse)
library(RFCDE)

# Load the summary of ghosting evaluation ---------------------------------

ghost_yac_distr_summary <-
  read_rds("data/ghosting_output/player_ghost_speed_dir_yac_distr_summary.rds")

ghost_yac_distr_summary %>%
  filter(player_display_name == "Adrian Amos") %>%
  arrange(delta_prob_first_down) %>%
  dplyr::select(week_id, game_play_id, player_display_name, nfl_id,
                prob_first_down, new_prob_first_down, delta_prob_first_down)

# # A tibble: 165 x 7
#       week_id game_play_id    player_display_name  nfl_id prob_first_down new_prob_first_down delta_prob_first_down
#         <int> <chr>           <chr>                 <dbl>           <dbl>               <dbl>                 <dbl>
#     1      12 2018112200_108  Adrian Amos         2552385          0.0839               0.723                -0.639
#     2       6 2018101404_1507 Adrian Amos         2552385          0.150                0.758                -0.608
#     3       6 2018101404_362  Adrian Amos         2552385          0.204                0.766                -0.562
#     4      13 2018120207_3637 Adrian Amos         2552385          0.403                0.923                -0.520
#     5      16 2018122312_1293 Adrian Amos         2552385          0.0719               0.560                -0.489
#     6       6 2018101404_2505 Adrian Amos         2552385          0.0318               0.486                -0.454
#     7       9 2018110401_279  Adrian Amos         2552385          0.0534               0.492                -0.438
#     8      12 2018112200_3793 Adrian Amos         2552385          0.0480               0.483                -0.435
#     9       1 2018090912_2348 Adrian Amos         2552385          0.187                0.606                -0.419
#    10      14 2018120901_2960 Adrian Amos         2552385          0.0347               0.425                -0.390


# Generate the LOWO model -------------------------------------------------

model_data <-
  read_rds("data/model_data/at_catch_yac_model_data.rds")

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

# Init the training data
train_data_matrix <- model_data %>%
  filter(week_id != 12) %>%
  dplyr::select(yac_model_vars) %>%
  # Convert to matrix
  as.matrix()

# Get the indices of the complete cases to use
train_data_use_i <- complete.cases(train_data_matrix)
train_data_matrix <- train_data_matrix[train_data_use_i,]

# Train data response:
train_data_resp <- model_data %>%
  filter(week_id != 12) %>%
  pull(end_x_change)
train_data_resp <- train_data_resp[train_data_use_i]

# Fit the model:
train_rfcde <- RFCDE(train_data_matrix, train_data_resp)


# Load the initial YAC pred data ------------------------------------------

init_rfcde_pred_yac_data <-
  read_rds("data/model_output/lowo_rfcde_pred_yac_data.rds")

# Get the data for this play:
ex_play_cde <- init_rfcde_pred_yac_data %>%
  filter(game_play_id == "2018112200_108")


# Load the ghosting to generate new cde -----------------------------------

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

long_example_play_def_data <- long_def_model_data %>%
  filter(game_play_id == "2018112200_108")

# Get the ghosting coordinates for Adrian Amos for this play
ex_play_amos_data <- long_ghosting_data %>%
  filter(game_play_id == "2018112200_108",
         displayName == "Adrian Amos")

# Now join the new positions for the play
ex_play_wide_plays_data <- long_example_play_def_data %>%
              dplyr::left_join(dplyr::select(ex_play_amos_data,
                                             game_play_id, old_rank,
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
ex_play_new_model_data <- model_data %>%
  filter(game_play_id == "2018112200_108") %>%
  dplyr::select(game_play_id,
                setdiff(colnames(model_data),
                        colnames(ex_play_wide_plays_data))) %>%
              inner_join(ex_play_wide_plays_data, by = "game_play_id")

# Now proceed to create the matrix for generating predictions for
new_play_data_matrix <- ex_play_new_model_data %>%
  # Use the above set of model variables
  dplyr::select(yac_model_vars) %>%
  # Convert to matrix
  as.matrix()
#   # Get the indices of the complete cases to use
# new_play_data_matrix <- as.matrix(t(new_data_matrix[new_data_use_i,]))
#             } else {
#               new_data_matrix <- new_data_matrix[new_data_use_i,]
#             }

# New data response:
new_play_data_resp <- ex_play_new_model_data %>% pull(end_x_change)

# what's the maximum possible distance the ball carrier can travel
# and round up:
new_play_max_possible_gain <- round(new_play_data_matrix[[1, "adj_bc_x"]])

# Now make a grid of values given the minimum observed in the whole
# data in increments of half yards to start:
delta_yards <- 0.5
new_play_gain_predict_grid <- seq(round(min(model_data$end_x_change)),
                                  new_play_max_possible_gain, by = delta_yards)

# Generate the CDE prediction:
new_play_cde_pred <- predict(train_rfcde, new_play_data_matrix,
                             "CDE", new_play_gain_predict_grid)

# Convert this to a long dataset where for
# each observation we have the predicted yards
# gained with its density estimate
new_rfcde_pred_yac_data <-
  tibble(pred_yards_gain = new_play_gain_predict_grid,
       play_cde = as.numeric(new_play_cde_pred)) %>%
  # Add column for predicted CDF:
  mutate(play_cdf = cumsum(play_cde / sum(play_cde)),
         # Finally with the test row index and observed yards gained:
         game_play_id = ex_play_new_model_data$game_play_id,
         obs_yards_gain = new_play_data_resp)


# Load the actual tracking data to display --------------------------------

raw_play_tracking_data <-
  read_csv("data/input/weekly_raw_tracking/week12.csv") %>%
  filter(gameId == 2018112200, playId == 108)

# Filter to when the pass is caught:
play_at_catch_tracking_data <- raw_play_tracking_data %>%
  filter(event == "pass_outcome_caught") %>%
  # Create a column denoting special roles:
  mutate(player_role =
           case_when(nflId == 2543646 ~ "bc",
                     nflId == 2552385 ~ "def",
                     is.na(nflId) ~ "football",
                     TRUE ~ "other"))


#  velocity angle in radians
play_at_catch_tracking_data$dir_rad <- play_at_catch_tracking_data$dir * pi / 180

#  velocity components
play_at_catch_tracking_data$v_x <- sin(play_at_catch_tracking_data$dir_rad) *
  play_at_catch_tracking_data$s
play_at_catch_tracking_data$v_y <- cos(play_at_catch_tracking_data$dir_rad) *
  play_at_catch_tracking_data$s
# What are the receiver's coordinates:
receiver_tracking_data <- play_at_catch_tracking_data %>%
  filter(player_role == "bc")



amos_starting_data <- play_at_catch_tracking_data

# Create the tracking data display ----------------------------------------

# Use the Lopez code:
# General field boundaries
xmin <- 0
xmax <- 160/3
hash_right <- 38.35
hash_left <- 12
hash_width <- 3.3

# Specific boundaries for a given play
ymin <- 0
ymax <- 120
df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
df_hash <- df_hash %>% filter(y < ymax, y > ymin)

at_catch_plot <- ggplot() +
  annotate("text", y = df_hash$x[df_hash$x < 55/2],
           x = df_hash$y[df_hash$x < 55/2], label = "|", vjust = -0.3, hjust = 0.4) +
  annotate("text", y = df_hash$x[df_hash$x > 55/2],
           x = df_hash$y[df_hash$x > 55/2], label = "|", vjust = 1, hjust = 0.4) +
  annotate("segment", y = xmin,
           x = seq(max(10, ymin), min(ymax, 110), by = 5),
           yend =  xmax,
           xend = seq(max(10, ymin), min(ymax, 110), by = 5)) +
  annotate("text", y = rep(hash_left, 11), x = seq(10, 110, by = 10),
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
           angle = 0, size = 4) +
  annotate("text", y = rep((xmax - hash_left), 11), x = seq(10, 110, by = 10),
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
           angle = 180, size = 4) +
  annotate("segment", y = c(xmin, xmin, xmax, xmax),
           x = c(ymin, ymax, ymax, ymin),
           yend = c(xmin, xmax, xmax, xmin),
           xend = c(ymax, ymax, ymin, ymin), colour = "black") +
  # geom_segment(data = filter(tracking_example_rfcde, frame.id == contact_frame_id),
  #              aes(x = pred_bc_x, xend = pred_bc_x,
  #                  y = xmin, yend = xmax,
  #                  alpha = pred_bc_density),
  #              size = 2, color = "red") +
  geom_point(data = play_at_catch_tracking_data, #alpha = 0.75,
             aes(y = y, x = x, colour = player_role, fill = team, alpha = player_role,
                 group = nflId, pch = team, size = player_role)) +
  geom_segment(data = play_at_catch_tracking_data, #alpha = 0.75,
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y,
                   group = nflId, alpha = player_role),
               color = "black",
               arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("point", x = ex_play_amos_data$x_ghost, y = ex_play_amos_data$y_ghost,
           color = "gray", size = 6) +
  # geom_segment(x = ex_play_new_model_data$adj_bc_x_from_first_down + receiver_tracking_data$x,
  #              xend = ex_play_new_model_data$adj_bc_x_from_first_down + receiver_tracking_data$x,
  #              y = xmin, yend = xmax, color = "red", size = 2) +
  annotate("segment", y = xmin, yend = xmax,
          x = ex_play_new_model_data$adj_bc_x_from_first_down + receiver_tracking_data$x,
          xend = ex_play_new_model_data$adj_bc_x_from_first_down + receiver_tracking_data$x,
          color = "red", size = 1) +
  scale_size_manual(values = c(6, 6, 3, 5), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_colour_manual(values = c("black", "black", "#654321", "white"), guide = FALSE) +
  scale_fill_manual(values = c("darkorange", "#654321", "blue")) +
  scale_alpha_manual(values = c(0.75, 0.75, 0.6, 0.6), guide = FALSE) +
  #scale_alpha_continuous(range = c(0.001, .5)) +
  xlim(ymin, ymax) +
  coord_fixed() +
  theme_nothing() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


# Now make the density curve of the field at this point as well, starting with
# actual tracking data:
ex_play_cde_chart_data <- ex_play_cde %>%
  mutate(pred_x = receiver_tracking_data$x + pred_yards_gain) %>%
  dplyr::inner_join(dplyr::select(new_rfcde_pred_yac_data,
                                  pred_yards_gain, play_cde, play_cdf) %>%
                      rename(new_play_cde = play_cde,
                             new_play_cdf = play_cdf),
                    by = "pred_yards_gain")


ex_rfcde_cdf_curve <- ex_play_cde_chart_data %>%
  dplyr::select(pred_x, play_cdf, new_play_cdf) %>%
  pivot_longer(play_cdf:new_play_cdf,
               names_to = "type", values_to = "cdf") %>%
  mutate(type = fct_recode(type, Observed = "play_cdf",
                           Ghost = "new_play_cdf"),
         type = fct_rev(type)) %>%
  ggplot(aes(x = pred_x, y = cdf, color = type)) +
  geom_line() +
  geom_vline(xintercept = ex_play_new_model_data$adj_bc_x_from_first_down + receiver_tracking_data$x,
             linetype = "dashed", color = "gold") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("red", "gray")) +
  labs(x = "Yardline",
       y = "Conditional CDF estimate",
       color = "Type:") +
  scale_x_continuous(limits = c(-10, 120),
                     breaks = seq(0, 100, by = 10),
                     labels = as.character(c(seq(0, 50, by = 10), seq(40, 0, by = -10))))

# Plot difference in CDF directly?
ex_play_cde_chart_data %>%
  mutate(cdf_diff = play_cdf - new_play_cdf) %>%
  ggplot(aes(x = pred_x, y = cdf_diff)) +
  geom_line(color = "black") +
  geom_vline(xintercept = ex_play_new_model_data$adj_bc_x_from_first_down + receiver_tracking_data$x,
             linetype = "dashed", color = "gold") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #scale_color_manual(values = c("red", "gray")) +
  labs(x = "Yardline",
       y = "Difference between observed and ghost conditional CDF estimate") +
  scale_x_continuous(limits = c(-10, 120),
                     breaks = seq(0, 100, by = 10),
                     labels = as.character(c(seq(0, 50, by = 10), seq(40, 0, by = -10))))


new_rfcde_pred_yac_data %>%
  # First join the first down and ball carrier position values:
  dplyr::left_join(dplyr::select(model_data, game_play_id, adj_bc_x,
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

ex_play_cde %>%
  # First join the first down and ball carrier position values:
  dplyr::left_join(dplyr::select(model_data, game_play_id, adj_bc_x,
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


# Stack the two to make a single plot
plot_grid(contact_rfcde_plot, rfcde_density_curve,
          ncol = 1)


