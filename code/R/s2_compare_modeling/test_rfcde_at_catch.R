# PURPOSE: Test code for implementing RFCDE modeling of tracking data, with
#          functions for evaluating holdout performance using the continuous
#          ranked probability score (CRPS). This involves fitting the models,
#          generating predictions on the grid of possible values, and then
#          converting to predicted probability distribution to evaluate with
#          CRPS out of sample.

#          NOTE: This version will just focus on the snapshot at the point of
#                catch, prior to then considering all of the frames within
#                ball carrier sequences

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

# Construct initial train and test data -----------------------------------

# First create a vector of covariates to use, starting with the ball carrier
# information, along with info on the nearest defender and teammate

# Start with bc variables:
bc_var_names <- c("adj_bc_x", "adj_bc_y", "bc_s", "bc_dis", "bc_dir_target_endzone")
# Next the closest defender information:
def1_var_names <- str_subset(colnames(model_data), "defense_1_") %>%
  str_subset("(_dist_to_bc)|(_s)|(_dis$)|(_adj_x)|(_adj_y)|(_dir_target_endzone)|(_dir_wrt_bc_diff)")

# Now form the training data - use all weeks but 17:
train_data_matrix <- model_data %>%
  filter(week_id < 17) %>%
  dplyr::select(c(bc_var_names, def1_var_names)) %>%
  # Convert to matrix
  as.matrix()

# Get the indices of the complete cases to use - will have to double check
# why there are missing values...
train_data_use_i <- complete.cases(train_data_matrix)
table(train_data_use_i)
# train_data_use_i
# FALSE  TRUE
#     3 10305

# Train data response:
train_data_resp <- model_data %>%
  filter(week_id < 17) %>%
  pull(end_x_change)
train_data_resp <- train_data_resp[train_data_use_i]

# Do the same for test data:
test_data_matrix <- model_data %>%
  filter(week_id == 17) %>%
  dplyr::select(c(bc_var_names, def1_var_names)) %>%
  # Convert to matrix
  as.matrix()

test_data_use_i <- complete.cases(test_data_matrix)
table(test_data_use_i)
# test_data_use_i
# TRUE
#  598

# Train data response:
test_data_resp <- model_data %>%
  filter(week_id == 17) %>%
  pull(end_x_change)


# Test initial fitting of model -------------------------------------------

# Stick with defaults for now... Main settings that one could consider tuning
# are the number of basis functions with mtry and n_trees (although I don't
# think those will have too much of an impact)
# Time this initial fit:
start_clock <- proc.time()
init_train_rfcde <- RFCDE(train_data_matrix, train_data_resp)
proc.time() - start_clock
# user  system elapsed
# 53.730   0.178  53.967
# Well that's fast!


# Make predictions for test data ------------------------------------------

# Now the tricky part is figuring out how to make predictions for each row
# in the test data. The grid to evaluate points at will need to be adaptive to
# the actual field position. Like if they are at the 10 yard line, then they
# can only go up to 10 yards... this means that ultimately the grid should
# vary between plays - thus eliminating the possibility of generating all
# predictions at once.

test_data_cde_preds <-
  map_dfr(1:nrow(test_data_matrix),
          function(test_i) {

            # what's the maximum possible distance the ball carrier can travel
            # and round up:
            max_possible_gain <- round(test_data_matrix[[test_i, "adj_bc_x"]])

            # Now make a grid of values given the minimum observed in the whole
            # data in increments of half yards to start:
            gain_predict_grid <- seq(round(min(model_data$end_x_change)),
                                     max_possible_gain, by = 0.5)
            # Note - should make the 0.5 a variable to modify given its
            # impact on the CDF calculation...

            # Generate the CDE prediction:
            test_cde_pred <- predict(init_train_rfcde, test_data_matrix[test_i,],
                                     "CDE", gain_predict_grid)

            # It then makes sense to convert this to a long dataset where for
            # each observation we have the predicted yards gained with its
            # corresponding density estimate

            tibble(pred_yards_gain = gain_predict_grid,
                   test_cde = as.numeric(test_cde_pred)) %>%
              # Add column for predicted CDF:
              mutate(test_cdf = cumsum(test_cde * 0.5),
                     # Finally with the test row index and observed yards gained:
                     test_row_i = test_i,
                     obs_yards_gain = test_data_resp[test_i])

          })

# This looks pretty nice actually... next thing is to actually calculate the
# CRPS following last year's Big Data Bowl: https://www.kaggle.com/c/nfl-big-data-bowl-2020/overview/evaluation

test_data_crps <- test_data_cde_preds %>%
  group_by(test_row_i) %>%
  summarize(play_crps =
              mean(.5 * (test_cdf -
                     as.numeric((pred_yards_gain - obs_yards_gain) >= 0))^2))

mean(test_data_crps$play_crps)
# [1] 0.02914464

# Have to think about if this is the appropriate way to do this... each play
# has a different subset of plays - and I need to consider the step size...

