# PURPOSE: Initialize the LOWO RFCDE models based on the selected set of covariates

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

# Create the final list of variables to use:
yac_model_vars <- c(bc_var_names, play_context_var_names,
                    unlist(def_var_name_list[1:2]))



# Fit each separate LOWO model --------------------------------------------

walk(1:17,
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

          # Save:
          write_rds(train_rfcde,
                    paste0("data/model_output/lowo_rfcde_models/week",
                           week_i, "_rfcde.rds"))

        })
