# Lee Richardson
# Purpose: Consolidate all the feature engineering into a single script

base_dir <- "/home/lee/Dropbox/NFL-EP-tracking"

devtools::load_all(paste0(base_dir, "/src/goingdeepR"))
library(tidyr)
library(readr)
library(plyr)
library(dplyr)
library(neuralnet)
library(stringr)
library(data.table)

# --- Load the design matrix ---
design_matrix_path <- paste0(base_dir, "/data/initial_bc_design_matrices/rusher_bc_design_matrix_voronoi.csv")
design_matrix <- readr::read_csv(design_matrix_path) 
covariate_names <- colnames(design_matrix)[str_detect(colnames(design_matrix),
                                                      "(^bc_)|(^offense)|(^defense)|(^voronoi)")]

# --- Add the lag variables ---
model_matrix <- design_matrix %>% 
  select(c(covariate_names, field_x_change, target_x)) %>% 
  drop_na() %>%
  as.data.table

lag_names <- paste0(covariate_names, "_lag1")

## Copy pasted from this URL: https://stackoverflow.com/questions/26291988/how-to-create-a-lag-variable-within-each-group
model_matrix[, (lag_names) :=  shift(.SD), by=bc_sequence_id, .SDcols=covariate_names]

## Convert back to a tibble
setattr(model_matrix, "class", c("tbl", "tbl_df", "data.frame"))

# --- Add the distance from ball-carrier ---

## Get the column names of the x and y coordinates 
off_def_x_cols <- colnames(model_matrix)[str_detect(colnames(model_matrix),
                                                     "((^offense)|(^defense))[:digit:]{1,2}_x")]
off_def_y_cols <- colnames(model_matrix)[str_detect(colnames(model_matrix),
                                                     "((^offense)|(^defense))[:digit:]{1,2}_y")]

## Modify the off_def_x_cols so that they are computed with respect to the bc_x and the target_x:
model_matrix <- model_matrix %>% 
  # Create a bc_frame_id that just counts the current frame for a bc sequence:
  group_by(bc_sequence_id) %>%
  mutate(bc_frame_id = 1:n()) %>%
  ungroup() %>%
  # Modify the off_def_x_cols so that they are computed with respect to the bc_x and the target_x:
  mutate_at(vars(off_def_x_cols), 
            .funs = list(change = ~ifelse((target_x == 0 & 
                                             (. <= bc_x)) | 
                                            (target_x == 120 & 
                                               (. > bc_x)),
                                          abs(. - bc_x), -abs(. - bc_x)))) %>%
  mutate_at(vars(off_def_y_cols), 
            .funs = list(change = ~ifelse((target_x == 0 & 
                                             (. <= bc_y)) | 
                                            (target_x == 120 & 
                                               (. > bc_y)),
                                          abs(. - bc_y), -abs(. - bc_y))))

# --- Add the frames from end covariate ---
bc_frames_from_end <- model_matrix %>%
  group_by(bc_sequence_id) %>%
  mutate(n_frames = max(bc_frame_id)) %>%
  ungroup() %>%
  mutate(frames_from_end = n_frames - bc_frame_id) %>%
  pull(frames_from_end)
model_matrix$frames_from_end <- bc_frames_from_end

## Write out the large model matrix 
readr::write_csv(x=model_matrix, path=paste0(base_dir, "/data/initial_bc_design_matrices/rusher_bc_model_matrix.csv"))
