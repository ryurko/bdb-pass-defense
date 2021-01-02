# PURPOSE: Create a dataset with the reasonable set of considered covariates to
#          simplify construction later on

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


# Create list of covaraites -----------------------------------------------

# Start with bc variables:
bc_var_names <- c("adj_bc_x", "adj_bc_y", "bc_s", "bc_dis", "bc_dir_target_endzone",
                  "adj_x_change_to_qb", "adj_y_change_to_qb", "bc_dist_to_qb",
                  "adj_bc_x_from_first_down", "qb_s")

# Next make a list of all possible offense and defense player information
def_var_name_list <- # for some reason there is up to 36 likely due to an
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
  setdiff(colnames(play_level_data),
          c("gameId", "playId", "adj_x_first_down", "quarter",
            "qtr_sec_remain", "game_sec_remain"))


# Select the necessary columns from the above lists -----------------------

model_data_simple <- model_data %>%
  dplyr::select(week_id, game_play_id, end_x_change, bc_var_names,
                unlist(def_var_name_list), unlist(off_var_name_list),
                play_context_var_names) %>%
  # Create absolute value versions of direction and y change variables:
  mutate_at(vars(contains("dir_target_endzone")),
            .funs = list(absval = ~abs(.))) %>%
  mutate_at(vars(contains("adj_y_change")),
            .funs = list(absval = ~abs(.)))


# Save --------------------------------------------------------------------

write_rds(model_data_simple,
          "data/model_data/at_catch_yac_model_data.rds")


