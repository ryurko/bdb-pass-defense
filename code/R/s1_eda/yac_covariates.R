# PURPOSE: EDA of potential covariates for modeling

library(tidyverse)

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
              dplyr::select(gameId, playId, adj_qb_x, adj_qb_y,
                            # Include QB speed at release
                            qb_x, qb_y, qb_s)
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

# Initialize list of player level covariates ------------------------------

# Start with bc variables:
bc_var_names <- c("adj_bc_x", "adj_bc_y", "bc_s", "bc_dis", "bc_dir_target_endzone",
                  "adj_x_change_to_qb", "adj_y_change_to_qb", "bc_dist_to_qb",
                  "adj_bc_x_from_first_down", "qb_s")

# Next make a list of all possible offense and defense player information
def_var_name_list <- # for some reason there is up to 36 likely due to an
  # erroneous play...
  lapply(1:36,
         function(def_i) {
           str_subset(colnames(model_data), paste0("defense_", def_i, "_")) %>%
             str_subset("(_dist_to_bc)|(_s)|(_dis$)|(_adj_x)|(_adj_y)|(_dir_target_endzone)|(_dir_wrt_bc_diff)")
         })

# Repeat for offense:
off_var_name_list <- # for some reason there is up to 36 likely due to an
  # erroneous play...
  lapply(1:20,
         function(off_i) {
           str_subset(colnames(model_data), paste0("offense_", off_i, "_")) %>%
             str_subset("(_dist_to_bc)|(_s)|(_dis$)|(_adj_x)|(_adj_y)|(_dir_target_endzone)|(_dir_wrt_bc_diff)")
         })

# Initial play-level context:
play_context_var_names <-
  setdiff(colnames(play_level_data),
            c("gameId", "playId", "adj_x_first_down", "quarter",
              "qtr_sec_remain", "game_sec_remain"))


# Summarize coverage of covariates ----------------------------------------

# Basically want to view for each variable how many missing values there are:
bc_var_coverage <-
  map_dfr(bc_var_names,
          function(bc_var) {
            tibble(model_variable = bc_var,
                   n_missing = length(which(is.na(model_data[[bc_var]]))))
          }) %>%
  mutate(n = nrow(model_data),
         prop_missing = n_missing / n)
bc_var_coverage
# A tibble: 10 x 4
#   model_variable             n_missing     n prop_missing
#     <chr>                        <int> <int>        <dbl>
#   1 adj_bc_x                         0 10906      0
#   2 adj_bc_y                         0 10906      0
#   3 bc_s                             0 10906      0
#   4 bc_dis                           0 10906      0
#   5 bc_dir_target_endzone            0 10906      0
#   6 adj_x_change_to_qb             100 10906      0.00917
#   7 adj_y_change_to_qb             100 10906      0.00917
#   8 bc_dist_to_qb                  100 10906      0.00917
#   9 adj_bc_x_from_first_down         0 10906      0
#   10 qb_s                          100 10906      0.00917
# Just 100 plays where pass release was not identified for inclusion

# Not surprising here - now apply the same logic to the def and off lists:
def_var_coverage <-
  map_dfr(1:length(def_var_name_list),
          function(def_player_i) {
            map_dfr(def_var_name_list[[def_player_i]],
                    function(def_var) {
                      tibble(model_variable =
                               str_remove(def_var,
                                          paste0("defense_", def_player_i, "_")),
                             n_missing = length(which(is.na(model_data[[def_var]]))))
                    }) %>%
              mutate(player_i = def_player_i)
          }) %>%
  mutate(n = nrow(model_data),
         prop_missing = n_missing / n)

# Repeat for offense:
off_var_coverage <-
  map_dfr(1:length(off_var_name_list),
          function(off_player_i) {
            map_dfr(off_var_name_list[[off_player_i]],
                    function(off_var) {
                      tibble(model_variable =
                               str_remove(off_var,
                                          paste0("offense_", off_player_i, "_")),
                             n_missing = length(which(is.na(model_data[[off_var]]))))
                    }) %>%
              mutate(player_i = off_player_i)
          }) %>%
  mutate(n = nrow(model_data),
         prop_missing = n_missing / n)

# Now plot the prop_missing for both by player index:
def_var_coverage %>%
  ggplot(aes(x = player_i, y = prop_missing)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ model_variable, ncol = 2) +
  theme_bw()
# Based on this - should only go up to consider 7 defenders
# 1-4 are missing same 3, then 5 to 6 are only missing 4 to 7 respectively,
# followed by 7 going up to 64

off_var_coverage %>%
  ggplot(aes(x = player_i, y = prop_missing)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ model_variable, ncol = 2) +
  theme_bw()
# Only consider up to 5 closest offense teammates - all plays have info for 4


# Examine ball-carrier level features with YAC ----------------------------

model_data %>%
  dplyr::select(game_play_id, end_x_change, bc_var_names) %>%
  pivot_longer(adj_bc_x:qb_s, names_to = "feature",
               values_to = "value") %>%
  ggplot(aes(x = value, y = end_x_change)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ feature, ncol = 3, scales = "free_x") +
  labs(x = "Feature value", y = "YAC") +
  theme_bw() +
  theme(strip.background = element_blank())

# All of these variables have some indication of a relationship... including
# QB speed at release which is NOT surprising

# It may make sense to use just the absolute value of the y and direction variables
# since the relationship with YAC appears to be symmetric. I was hesitant to do
# so initially due to any potential for interactions based on the side but now
# I don't think that matters...

# View the relationships with the absolute value used instead:
model_data %>%
  mutate(adj_bc_y = abs(adj_bc_y),
         adj_y_change_to_qb = abs(adj_y_change_to_qb),
         bc_dir_target_endzone = abs(bc_dir_target_endzone)) %>%
  dplyr::select(game_play_id, end_x_change, bc_var_names) %>%
  pivot_longer(adj_bc_x:qb_s, names_to = "feature",
               values_to = "value") %>%
  ggplot(aes(x = value, y = end_x_change)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ feature, ncol = 3, scales = "free_x") +
  labs(x = "Feature value", y = "YAC") +
  theme_bw() +
  theme(strip.background = element_blank())

# Okay this is much better looking - I should just go ahead and use these instead

# Make separate figures for receiver direction to target endzone (abs val) and
# info about QB at release:
bc_dir_plot <- model_data %>%
  mutate(abs_bc_dir_target_endzone = abs(bc_dir_target_endzone)) %>%
  ggplot(aes(x = abs_bc_dir_target_endzone, y = end_x_change)) +
  geom_point(alpha = 0.1) +
  geom_vline(xintercept = 90, linetype = "dashed", color = "darkred") +
  annotate("segment", x = 100, xend = 165, y = 45, yend = 45,
           color = "darkred", size = 1, arrow = arrow()) +
  annotate("text", label = "Receiver's back is\nfacing target endzone",
           x = 130, y = 50, color = "darkred") +
  labs(x = "Absolute value of angle between receiver's direction and target endzone",
       y = "Observed YAC",
       title = "Decrease in YAC when receiver's back is facing target endzone",
       subtitle = "Vertical red dashed line denotes 90 degree angle") +
  theme_bw() +
  theme(strip.background = element_blank())

# QB speed:
qb_speed_plot <- model_data %>%
  ggplot(aes(x = qb_s, y = end_x_change)) +
  geom_point(alpha = 0.1) +
  # annotate("text", label = "Receiver's back is\nfacing target endzone",
  #          x = 130, y = 50, color = "darkred") +
  labs(x = "QB's speed in yards/second at release",
       y = "Observed YAC",
       title = "Decrease in YAC as QB's speed at release increases") +
  theme_bw() +
  theme(strip.background = element_blank())

# YAC relationships with play-context -------------------------------------

# Next examine the relationships with play-context - first without formation
# indicators

model_data %>%
  dplyr::select(game_play_id, end_x_change,
                str_subset(play_context_var_names, "is_", negate = TRUE)) %>%
  pivot_longer(posteam_score_diff:half_sec_remain, names_to = "feature",
               values_to = "value") %>%
  ggplot(aes(x = value, y = end_x_change)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ feature, ncol = 3, scales = "free_x") +
  theme_bw() +
  theme(strip.background = element_blank())

# Score differential is very interesting... plays with the most YAC occur in
# the tightest games! But we see clear distribution differences here given
# information about the types of players on the field and pass rushers. Basically
# with more DL and pass rushers, we expect more YAC


# Look at defense level relationships -------------------------------------

# Do this separately for each number of players:
model_data %>%
  dplyr::select(game_play_id, end_x_change, def_var_name_list[[1]]) %>%
  pivot_longer(def_var_name_list[[1]], names_to = "feature",
               values_to = "value") %>%
  ggplot(aes(x = value, y = end_x_change)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ feature, ncol = 3, scales = "free_x") +
  theme_bw() +
  theme(strip.background = element_blank())
# Okay clear relationships here with closest defender, but definitely need
# to consider taking absolute value with respect to y and direction variables

model_data %>%
  dplyr::select(game_play_id, end_x_change, def_var_name_list[[2]]) %>%
  pivot_longer(def_var_name_list[[2]], names_to = "feature",
               values_to = "value") %>%
  ggplot(aes(x = value, y = end_x_change)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ feature, ncol = 3, scales = "free_x") +
  theme_bw() +
  theme(strip.background = element_blank())
# Definitely much weaker than closest defender

model_data %>%
  dplyr::select(game_play_id, end_x_change, def_var_name_list[[3]]) %>%
  pivot_longer(def_var_name_list[[3]], names_to = "feature",
               values_to = "value") %>%
  ggplot(aes(x = value, y = end_x_change)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ feature, ncol = 3, scales = "free_x") +
  theme_bw() +
  theme(strip.background = element_blank())
# Again looks weaker than the previous

model_data %>%
  dplyr::select(game_play_id, end_x_change, def_var_name_list[[4]]) %>%
  pivot_longer(def_var_name_list[[4]], names_to = "feature",
               values_to = "value") %>%
  ggplot(aes(x = value, y = end_x_change)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ feature, ncol = 3, scales = "free_x") +
  theme_bw() +
  theme(strip.background = element_blank())
# Yeah now this is really looking weak

model_data %>%
  dplyr::select(game_play_id, end_x_change, def_var_name_list[[5]]) %>%
  pivot_longer(def_var_name_list[[5]], names_to = "feature",
               values_to = "value") %>%
  ggplot(aes(x = value, y = end_x_change)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ feature, ncol = 3, scales = "free_x") +
  theme_bw() +
  theme(strip.background = element_blank())
# Nothing more... don't think it's necessary to go beyond fifth closest

# Look at offense level relationships -------------------------------------

# Do this separately for each number of players:
model_data %>%
  dplyr::select(game_play_id, end_x_change, off_var_name_list[[1]]) %>%
  pivot_longer(off_var_name_list[[1]], names_to = "feature",
               values_to = "value") %>%
  ggplot(aes(x = value, y = end_x_change)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ feature, ncol = 3, scales = "free_x") +
  theme_bw() +
  theme(strip.background = element_blank())
# Im not sure there is anything meaninful here... the challenge with offense
# is not knowing who would be the appropriate defender to "assign" the offense
# player to... the simplest solution is to consider leaving in the offensive
# player variables in case there are any interactions with defense player that
# get picked up... The change in y from ball carrier looks interesting though...

model_data %>%
  dplyr::select(game_play_id, end_x_change, off_var_name_list[[2]]) %>%
  pivot_longer(off_var_name_list[[2]], names_to = "feature",
               values_to = "value") %>%
  ggplot(aes(x = value, y = end_x_change)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ feature, ncol = 3, scales = "free_x") +
  theme_bw() +
  theme(strip.background = element_blank())
# Definitely much weaker than closest defender

model_data %>%
  dplyr::select(game_play_id, end_x_change, off_var_name_list[[3]]) %>%
  pivot_longer(off_var_name_list[[3]], names_to = "feature",
               values_to = "value") %>%
  ggplot(aes(x = value, y = end_x_change)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ feature, ncol = 3, scales = "free_x") +
  theme_bw() +
  theme(strip.background = element_blank())

model_data %>%
  dplyr::select(game_play_id, end_x_change, off_var_name_list[[4]]) %>%
  pivot_longer(off_var_name_list[[4]], names_to = "feature",
               values_to = "value") %>%
  ggplot(aes(x = value, y = end_x_change)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ feature, ncol = 3, scales = "free_x") +
  theme_bw() +
  theme(strip.background = element_blank())
# The one justification for information about the other teammates is what it
# indicates with respect to the receiver play call in terms of potential
# space creation - like there is visible structure in the adj_y_change variables
# (but again it looks like I should simply use the absolute value for that)


