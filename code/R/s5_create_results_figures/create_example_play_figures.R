# PURPOSE: Create figures demonstrating the change in distribution for an
#          example play with a noticeable shift for the player compared to
#          the ghosting location.

library(tidyverse)
library(RFCDE)
library(cowplot)

# Load the summary of ghosting evaluation ---------------------------------

ghost_yac_distr_summary <-
  read_rds("data/ghosting_output/player_ghost_full_yac_distr_summary_w_padding.rds")

ghost_yac_distr_summary %>%
  filter(player_display_name == "Kyle Fuller") %>%
  arrange(delta_prob_first_down) %>%
  dplyr::select(week_id, game_play_id, player_display_name, nfl_id,
                prob_first_down, new_prob_first_down, delta_prob_first_down,
                prob_td, new_prob_td, delta_prob_td)

# w   eek_id game_play_id    player_display_name  nfl_id prob_first_down new_prob_first_down delta_prob_first_down
#       <int> <chr>           <chr>                 <dbl>           <dbl>               <dbl>                 <dbl>
#   1       7 2018102102_2806 Kyle Fuller         2543681          0.0829               0.891                -0.808
#   2       4 2018093001_2524 Kyle Fuller         2543681          0.100                0.707                -0.607
#   3       6 2018101404_1507 Kyle Fuller         2543681          0.150                0.629                -0.479
#   4       7 2018102102_3732 Kyle Fuller         2543681          0.0587               0.516                -0.457
#   5      16 2018122312_2134 Kyle Fuller         2543681          0.0739               0.455                -0.381
#   6      11 2018111802_3629 Kyle Fuller         2543681          0.0292               0.384                -0.355
#   7      11 2018111802_2478 Kyle Fuller         2543681          0.0305               0.368                -0.338
#   8       4 2018093001_3823 Kyle Fuller         2543681          0.0623               0.384                -0.322
#   9      11 2018111802_540  Kyle Fuller         2543681          0.0356               0.342                -0.307
#  10      15 2018121603_3868 Kyle Fuller         2543681          0.0354               0.326                -0.291

# Previously looked at example for Jaire Alexander (2560952): 2018091602_4551

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
  filter(week_id != 7) %>%
  dplyr::select(yac_model_vars) %>%
  # Convert to matrix
  as.matrix()

# Get the indices of the complete cases to use
train_data_use_i <- complete.cases(train_data_matrix)
train_data_matrix <- train_data_matrix[train_data_use_i,]

# Train data response:
train_data_resp <- model_data %>%
  filter(week_id != 7) %>%
  pull(end_x_change)
train_data_resp <- train_data_resp[train_data_use_i]

# Fit the model:
train_rfcde <- RFCDE(train_data_matrix, train_data_resp)


# Load the initial YAC pred data ------------------------------------------

init_rfcde_pred_yac_data <-
  read_rds("data/model_output/lowo_rfcde_pred_yac_data_w_padding.rds")

# Get the data for this play:
ex_play_cde <- init_rfcde_pred_yac_data %>%
  filter(game_play_id == "2018102102_2806")


# Load the ghosting to generate new cde -----------------------------------

ghosting_data <-
  read_csv("data/ghosting_output/ghosts_at_catch_full_upd.csv")

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
  filter(game_play_id == "2018102102_2806")

# Get the ghosting coordinates for Kyle Fuller for this play
ex_play_fuller_data <- long_ghosting_data %>%
  filter(game_play_id == "2018102102_2806",
         displayName == "Kyle Fuller")

# Now join the new positions for the play
ex_play_wide_plays_data <- long_example_play_def_data %>%
              dplyr::left_join(dplyr::select(ex_play_fuller_data,
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
  filter(game_play_id == "2018102102_2806") %>%
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

# New data response:
new_play_data_resp <- ex_play_new_model_data %>% pull(end_x_change)

# what's the maximum possible distance the ball carrier can travel (w/ padding)
new_obs_max_x <- new_play_data_matrix[[1, "adj_bc_x"]]
new_play_max_possible_gain <- round(new_obs_max_x) + 2

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
       play_cde = as.numeric(new_play_cde_pred))  %>%
  # Cap the pred_yards_gain so the limit is set to be
  # the obs_max_x:
  mutate(pred_yards_gain = pmin(pred_yards_gain, new_obs_max_x)) %>%
  # Sum the rows in the padding above:
  group_by(pred_yards_gain) %>%
  summarize(play_cde = sum(play_cde, na.rm = TRUE)) %>%
  ungroup() %>%
  # Add column for predicted CDF:
  mutate(play_cdf = cumsum(play_cde / sum(play_cde)),
         # Finally with the test row index and observed yards gained:
         game_play_id = ex_play_new_model_data$game_play_id,
         obs_yards_gain = new_play_data_resp)


# Load the actual tracking data to display --------------------------------

raw_play_tracking_data <-
  read_csv("data/input/weekly_raw_tracking/week7.csv") %>%
  filter(gameId == 2018102102, playId == 2806)

# Filter to when the pass is caught:
play_at_catch_tracking_data <- raw_play_tracking_data %>%
  filter(event == "pass_outcome_caught") %>%
  # Create a column denoting special roles:
  mutate(player_role =
           case_when(nflId == 2530515 ~ "bc",
                     nflId == 2543681 ~ "def",
                     is.na(nflId) ~ "football",
                     TRUE ~ "other"),
         # Split between remaining offense and defense:
         # (in this play away is offense, home is defense)
         team_side = ifelse(team == "away", "offense", "defense"),
         player_role = ifelse(player_role == "other",
                              paste0(player_role, "_", team_side),
                              player_role))


#  velocity angle in radians (followed example here: https://github.com/asonty/ngs_highlights/blob/master/utils/scripts/plot_utils.R)
play_at_catch_tracking_data$dir_rad <- play_at_catch_tracking_data$dir * pi / 180

#  velocity components
play_at_catch_tracking_data$v_x <- sin(play_at_catch_tracking_data$dir_rad) *
  play_at_catch_tracking_data$s
play_at_catch_tracking_data$v_y <- cos(play_at_catch_tracking_data$dir_rad) *
  play_at_catch_tracking_data$s
# What are the receiver's coordinates:
receiver_tracking_data <- play_at_catch_tracking_data %>%
  filter(player_role == "bc")

def_tracking_data <- play_at_catch_tracking_data %>%
  filter(player_role == "def")


#fuller_starting_data <- play_at_catch_tracking_data

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

# Use the play direction to determine the first down marker location:
x_first_down_marker <- ifelse(receiver_tracking_data$playDirection == "right",
                                 receiver_tracking_data$x + ex_play_new_model_data$adj_bc_x_from_first_down,
                                 receiver_tracking_data$x - ex_play_new_model_data$adj_bc_x_from_first_down)
# Repeat for ghost coordinates:
def_ghost_x <- ifelse(receiver_tracking_data$playDirection == "right",
                      ex_play_fuller_data$x_ghost,
                      120 - ex_play_fuller_data$x_ghost)
def_ghost_y <- ifelse(receiver_tracking_data$playDirection == "right",
                      ex_play_fuller_data$y_ghost,
                      (160 / 3) - ex_play_fuller_data$y_ghost)


# Now create the plot:
at_catch_plot <- ggplot() +
  annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 160/3, alpha = 0.4,
           color = "gray", fill = "darkgreen") +
  annotate("text", y = df_hash$x[df_hash$x < 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x < 55/2], label = "|", vjust = -0.3, hjust = 0.4) +
  annotate("text", y = df_hash$x[df_hash$x > 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x > 55/2], label = "|", vjust = 1, hjust = 0.4) +
  annotate("segment", y = xmin,
           x = seq(max(10, ymin), min(ymax, 110), by = 5),
           yend =  xmax, color = "white",
           xend = seq(max(10, ymin), min(ymax, 110), by = 5), alpha = 0.75) +
  annotate("text", y = rep(hash_left, 11), x = seq(10, 110, by = 10), alpha = 0.75,
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
           angle = 0, size = 4, color = "white") +
  annotate("text", y = rep((xmax - hash_left), 11), x = seq(10, 110, by = 10),
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
           angle = 180, size = 4, alpha = 0.75, color = "white") +
  annotate("segment", y = c(xmin, xmin, xmax, xmax),
           x = c(ymin, ymax, ymax, ymin),
           yend = c(xmin, xmax, xmax, xmin),
           xend = c(ymax, ymax, ymin, ymin), colour = "white", alpha = 0.25) +
  # Line connecting ghost to player:
  annotate("segment", y = xmin, yend = xmax,
           x = x_first_down_marker,
           xend = x_first_down_marker,
           color = "gold", size = 2) +
  annotate("segment", y = def_tracking_data$y, yend = def_ghost_y,
           x = def_tracking_data$x, xend = def_ghost_x,
           color = "red", size = 2) +
  geom_point(data = play_at_catch_tracking_data, #alpha = 0.75,
             aes(y = y, x = x, colour = player_role,
                 fill = player_role, alpha = player_role,
                 group = nflId, pch = player_role, size = player_role)) +
  geom_segment(data = play_at_catch_tracking_data, #alpha = 0.75,
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y,
                   group = nflId, alpha = player_role),
               color = "black",
               alpha = 0.75,
               arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("point", x = def_ghost_x, y = def_ghost_y,
           fill = "gray25", size = 6, shape = 21, color = "black") +
  scale_size_manual(values = c(6, 6, 3, 5, 5), guide = FALSE) +
  scale_shape_manual(values = c(21, 21, 16, 21, 21), guide = FALSE) +
  scale_colour_manual(values = c("black", "black", "#654321", "white", "white"), guide = FALSE) +
  scale_fill_manual(values = c("blue", "red", "#654321", "darkorange", "blue")) +
  scale_alpha_manual(values = c(0.75, 1, 0.6, 0.6, 0.6), guide = FALSE) +
  xlim(ymin, ymax) +
  coord_fixed() +
  theme_nothing() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


# Now make the density curve of the field at this point as well, starting with
# actual tracking data:
ex_play_cde_chart_data <- ex_play_cde %>%
  # Minus since play direction is to the left
  mutate(pred_x = receiver_tracking_data$x +
           (ifelse(receiver_tracking_data$playDirection == "right", 1, -1) *
              pred_yards_gain)) %>%
  dplyr::inner_join(dplyr::select(new_rfcde_pred_yac_data,
                                  pred_yards_gain, play_cde, play_cdf) %>%
                      rename(new_play_cde = play_cde,
                             new_play_cdf = play_cdf),
                    by = "pred_yards_gain")

ex_rfcde_cde_curve <- ex_play_cde_chart_data %>%
  dplyr::select(pred_x, play_cde, new_play_cde) %>%
  pivot_longer(play_cde:new_play_cde,
               names_to = "type", values_to = "cde") %>%
  mutate(type = fct_recode(type, Observed = "play_cde",
                           Ghost = "new_play_cde"),
         type = fct_rev(type)) %>%
  ggplot() +
  geom_vline(xintercept = x_first_down_marker, color = "gold", size = 2) +
  geom_line(aes(x = pred_x, y = cde, color = type)) +
  annotate("text", label = 'Change in estimated Pr(1st down) = -0.81\ncompared to expected "ghost" coordinates',
           color = "red", x = 45, y = .25) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("red", "gray25")) +
  labs(x = "Yard line",
       y = "Conditional density estimate",
       color = "Type:") +
  scale_x_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 10),
                     labels = c("", as.character(seq(0, 50, by = 10)),
                                as.character(seq(40, 0, by = -10)), ""))

ex_rfcde_cdf_curve <- ex_play_cde_chart_data %>%
  dplyr::select(pred_x, play_cdf, new_play_cdf) %>%
  pivot_longer(play_cdf:new_play_cdf,
               names_to = "type", values_to = "cdf") %>%
  mutate(type = fct_recode(type, Observed = "play_cdf",
                           Ghost = "new_play_cdf"),
         type = fct_rev(type)) %>%
  ggplot(aes(x = pred_x, y = cdf, color = type)) +
  geom_line() +
  geom_vline(xintercept = x_first_down_marker,
             linetype = "dashed", color = "gold") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("red", "gray25")) +
  labs(x = "Yard line",
       y = "Conditional CDF estimate",
       color = "Type:") +
  scale_x_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 10),
                     labels = c("", as.character(seq(0, 50, by = 10)),
                                as.character(seq(40, 0, by = -10)), ""))


# Stack the two to make a single plot
plot_grid(at_catch_plot, ex_rfcde_cde_curve,
          ncol = 1, align = "v")

