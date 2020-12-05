# PURPOSE: Create a model dataset that has a much "smarter" set of the
#          covariates, meaning rather than simply using all the raw x and y 
#          variables, focus on variables with respect to the target endzone. 
#          Additionally, modify the dir and voronoi variables so that they 
#          respect the target endzone as well.


#-------------------------------------------------------------------------------
library(tidyverse)
library(caret)
library(xgboost)
library(keras)
library(glmnet)
library(future)
library(furrr) # install the Github version 
use_python("/usr/local/bin/python3")
library(tensorflow)
Sys.setenv(TENSORFLOW_PYTHON = "/usr/local/bin/python3")


#-------------------------------------------------------------------------------

# Load the previously constructed model dataset:
model_dataset_no_lag <- read_csv("src/modeling/lowo_model_datasets/model_dataset_no_lag_1022.csv")

# Grab the covariates:
covariate_names <- colnames(model_dataset_no_lag)[str_detect(colnames(model_dataset_no_lag),
                                                     "(^bc_)|(^offense)|(^defense)|(^voronoi)")]
covariate_names <- covariate_names[-which(covariate_names %in% c("bc_sequence_id",
                                                                 "bc_frame_id"))]

# Now going to proceed to modify all of the covariates with x or y for bc, offense,
# or defense so that it is with respect to the target endzone. For x this means
# the variable will represent the distance from the target endzone, while for y
# it denotes how far away from center of field - positive values indicate to the
# left, while negative values denote towards the right:

x_cols <- str_subset(covariate_names,"((^offense)|(^defense)|(^bc))([:digit:]{1,2})?_x$")
y_cols <- str_subset(covariate_names,"((^offense)|(^defense)|(^bc))([:digit:]{1,2})?_y$")

model_dataset_adj_xy <- model_dataset_no_lag %>% 
  # Modify the x and y columns so that they are with respect to the target endzone:
  mutate_at(vars(x_cols), 
            .funs = list(adj = ~ifelse(target_x == 120,
                                       110 - .,
                                       . - 10))) %>%
  mutate_at(vars(y_cols),
            .funs = list(adj = ~ifelse(target_x == 120,
                                       . - 26.65,
                                       26.65 - .)))
# As an additional pre-processing step, should only consider the frames where the 
# ball carrier is not in the target endzone - this appears to be only for a small
# fraction so not a problem.

dir_cols <- str_subset(covariate_names,"((^offense)|(^defense)|(^bc))([:digit:]{1,2})?_(dir)$")
# Next make endzone direction columns:
model_dataset_adj_xy <- model_dataset_adj_xy %>% 
  mutate_at(vars(dir_cols), 
            .funs = list(target_endzone = ~ifelse(target_x == 0 &
                                                    . > 90,
                                                  270 - .,
                                                  ifelse(target_x == 0 & . <= 90,
                                                         . - 90,
                                                         ifelse(target_x == 120 & . < 270,
                                                                90 - ., 450 - .)))))

model_dataset_adj_xy %>%
  ggplot(aes(x = bc_dir_target_endzone)) +
  geom_histogram(fill = "gray", color = "black") +
  theme_bw() +
  facet_wrap(~target_x, ncol = 2)

# Next want to make direction variables for each defender with regards to the
# ball-carrier - ie are they heading in the direction of the ball-carrier?

# Compute the angle between the defender X and the ball carrier using the change
# variables:
model_dataset_bc_def_dir_cols <- map_dfr(1:nrow(model_dataset_adj_xy),
                                         function(frame_i) {
                                           frame_i_data <- model_dataset_adj_xy[frame_i,]
                                           # Create the variables for each defender
                                           map_dfc(1:11, 
                                                   function(def_i) {
                                                     def_change_x_col <- paste0("defense", def_i,
                                                                                "_x_change")
                                                     def_change_y_col <- paste0("defense", def_i,
                                                                                "_y_change")
                                                     def_dir_endzone <- paste0("defense", def_i,
                                                                               "_dir_target_endzone")

                                                     # Compute the angle between
                                                     # the defender and ball-carrier
                                                     # with respect to the target_x:
                                                     bc_def_dir <- pracma::rad2deg(atan2(frame_i_data[[def_change_y_col]],
                                                                                         frame_i_data[[def_change_x_col]]))
                                                     
                                                     # Update this angle based on its value to reflect
                                                     # the same coordinate system as the defender:
                                                     bc_def_dir <- ifelse(bc_def_dir > 0 & bc_def_dir < 180,
                                                                          bc_def_dir - 180,
                                                                          ifelse(bc_def_dir < 0 & bc_def_dir > -180,
                                                                                 bc_def_dir + 180,
                                                                                 # special cases for 0 and 180/-180:
                                                                                 abs(bc_def_dir)))
                                                     
                                                     # Now compute the value with respect to their 
                                                     # direction they are heading:
                                                     def_dir_diff <- ifelse(frame_i_data[[def_dir_endzone]] == 0,
                                                                            abs(bc_def_dir),
                                                                            ifelse(bc_def_dir == 180,
                                                                            abs(frame_i_data[[def_dir_endzone]]),
                                                                            ifelse(bc_def_dir == 0,
                                                                                   180 - abs(frame_i_data[[def_dir_endzone]]),
                                                                                   ifelse(sign(bc_def_dir) == 
                                                                                            sign(frame_i_data[[def_dir_endzone]]),
                                                                                          abs(bc_def_dir - frame_i_data[[def_dir_endzone]]),
                                                                                          ifelse(sign(bc_def_dir) == -1 & 
                                                                                                   sign(frame_i_data[[def_dir_endzone]]) == 1,
                                                                                                 pmin((360 + bc_def_dir) - 
                                                                                                        frame_i_data[[def_dir_endzone]],
                                                                                                      frame_i_data[[def_dir_endzone]] - 
                                                                                                        bc_def_dir),
                                                                                                 pmin((360 + 
                                                                                                         frame_i_data[[def_dir_endzone]]) - 
                                                                                                        bc_def_dir,
                                                                                                      bc_def_dir - 
                                                                                                        frame_i_data[[def_dir_endzone]]))))))
                                                     # Create as a dataframe:
                                                     result <- data.frame("def_dir_diff" = def_dir_diff)
                                                     colnames(result) <- paste0("defense", def_i,
                                                                                "_dir_wrt_bc_diff")
                                                     return(result)
                                                   })
                                         })


model_dataset_bc_def_dir_cols %>%
  gather(def_type, angle_diff) %>%
  ggplot(aes(x = angle_diff)) +
  geom_histogram(fill = "gray", color = "black") +
  theme_bw() +
  facet_wrap(~def_type, ncol = 3)
# EUREKA THIS SHIT FINALLY WORKED

# Join these columns:
model_dataset_adj_xy <- model_dataset_adj_xy %>%
  bind_cols(model_dataset_bc_def_dir_cols)


# Make adjusted voronoi far and close variables:
far_close_cols <- str_subset(covariate_names,"((_far)|(_close))$")
model_dataset_adj_xy <- model_dataset_adj_xy %>% 
  # Modify so that they are with respect to the target endzone:
  mutate_at(vars(far_close_cols), 
            .funs = list(adj = ~ifelse(target_x == 120,
                                       110 - .,
                                       . - 10)))

# Finally save this dataset:
write_csv(model_dataset_adj_xy, "src/modeling/lowo_model_datasets/model_dataset_no_lag_adj_xydir_1022.csv")


