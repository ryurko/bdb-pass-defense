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



# Initialize list of player level covariates ------------------------------

# Start with bc variables:
bc_var_names <- c("adj_bc_x", "adj_bc_y", "bc_s", "bc_dis", "bc_dir_target_endzone")

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

# A tibble: 5 x 4
#     model_variable        n_missing     n prop_missing
#     <chr>                     <int> <int>        <dbl>
#   1 adj_bc_x                      0 10906            0
#   2 adj_bc_y                      0 10906            0
#   3 bc_s                          0 10906            0
#   4 bc_dis                        0 10906            0
#   5 bc_dir_target_endzone         0 10906            0

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
# Only consider up to 5 closest offense teammates - all have 4

# Would not be surprising to see nearest three basically be all that matters
# for both

# Should go back at some point to review whats wrong with the 3 plays without
# any nearest defenders


