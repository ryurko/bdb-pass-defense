# PURPOSE: Fit more versions of the deep learning models

#-------------------------------------------------------------------------------
library(tidyverse)
library(caret)
library(keras)
library(future)
library(furrr) # install the Github version 
use_python("/usr/local/bin/python3")
library(tensorflow)
Sys.setenv(TENSORFLOW_PYTHON = "/usr/local/bin/python3")

#-------------------------------------------------------------------------------

# Load the previously constructed model dataset:
model_dataset_adj_xy <- read_csv("src/modeling/lowo_model_datasets/model_dataset_no_lag_adj_xydir_1022.csv")

# Create the new vector of covariate names:
adj_covariate_names <- 
  str_subset(colnames(model_dataset_adj_xy),
             "((_dis)|(_s)|(_dist_to_ball)|(_change)|(_adj)|(_target_endzone)|(_wrt_bc_diff)|(_area)|(area_in_front)|(_bubble)|(_area_in_front_bc_only))$")
adj_covariate_names <- adj_covariate_names[-which(adj_covariate_names == "field_x_change")]

# Load the folds
lowo_game_id_folds <- readRDS("src/modeling/lowo_model_datasets/lowo_game_id_folds.rds")


# ------------------------------------------------------------------------------
# FF - Three layers 5 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_threefive_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                         function(fold_i) {
                                           
                                           fold <- lowo_game_id_folds[[fold_i]]
                                           
                                           # Train the model on the training data:
                                           train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                   fold$train_game_ids)
                                           test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                  fold$test_game_ids)
                                           # Get the training data covariates
                                           train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                            adj_covariate_names])
                                           # Create scaled version:
                                           scaled_train_data_cov <- scale(train_data_cov)
                                           train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                           
                                           # Specify the neural network model using the bc-models.R code:
                                           nn_model <- keras_model_sequential() %>%
                                             layer_dense(units = 5, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 5, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 5, activation = "relu") %>%
                                             layer_dense(units = 1)
                                           
                                           nn_model %>% compile(
                                             loss = "mse",
                                             optimizer = "adam", 
                                             metrics = list("mean_squared_error"))
                                           
                                           callbacks_list <- list(
                                             callback_early_stopping(
                                               monitor = "mean_squared_error",
                                               patience = 5
                                             ), 
                                             callback_model_checkpoint(
                                               filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_threefive_models/ff_nn_howk_",
                                                                 fold_i, "_model.h5"),
                                               monitor = "val_loss",
                                               save_best_only = TRUE)
                                           )
                                           
                                           nn_model %>% fit(
                                             scaled_train_data_cov,
                                             train_data_y,
                                             epochs = 100,
                                             validation_split = 0.2,
                                             verbose = 0, 
                                             callbacks = callbacks_list
                                           )
                                           
                                           ## Load the model w/ the best parameters 
                                           nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_threefive_models/ff_nn_howk_",
                                                                                     fold_i, "_model.h5"))
                                           
                                           # Get the predictions:
                                           test_data <- model_dataset_adj_xy[test_data_i,]
                                           scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                           
                                           # Join the prediction column:
                                           test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                               scaled_test_data))
                                           
                                           ## Remove the file that stores the best model
                                           #file.remove("my_model.h5") just keep for now
                                           
                                           return(test_data)
                                         })
# Save these results:
saveRDS(ff_threefive_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_threefive_test_predictions.rds")
rm(ff_threefive_test_predictions)
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# FF - Ten layers 5 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_tenfive_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                            function(fold_i) {
                                              
                                              fold <- lowo_game_id_folds[[fold_i]]
                                              
                                              # Train the model on the training data:
                                              train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                      fold$train_game_ids)
                                              test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                     fold$test_game_ids)
                                              # Get the training data covariates
                                              train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                               adj_covariate_names])
                                              # Create scaled version:
                                              scaled_train_data_cov <- scale(train_data_cov)
                                              train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                              
                                              # Specify the neural network model using the bc-models.R code:
                                              nn_model <- keras_model_sequential() %>%
                                                layer_dense(units = 5, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 5, activation = "relu", 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 5, activation = "relu", 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 5, activation = "relu", 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 5, activation = "relu", 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 5, activation = "relu", 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 5, activation = "relu", 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 5, activation = "relu", 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 5, activation = "relu", 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 5, activation = "relu") %>%
                                                layer_dense(units = 1)
                                              
                                              nn_model %>% compile(
                                                loss = "mse",
                                                optimizer = "adam", 
                                                metrics = list("mean_squared_error"))
                                              
                                              callbacks_list <- list(
                                                callback_early_stopping(
                                                  monitor = "mean_squared_error",
                                                  patience = 5
                                                ), 
                                                callback_model_checkpoint(
                                                  filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_tenfive_models/ff_nn_howk_",
                                                                    fold_i, "_model.h5"),
                                                  monitor = "val_loss",
                                                  save_best_only = TRUE)
                                              )
                                              
                                              nn_model %>% fit(
                                                scaled_train_data_cov,
                                                train_data_y,
                                                epochs = 100,
                                                validation_split = 0.2,
                                                verbose = 0, 
                                                callbacks = callbacks_list
                                              )
                                              
                                              ## Load the model w/ the best parameters 
                                              nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_tenfive_models/ff_nn_howk_",
                                                                                        fold_i, "_model.h5"))
                                              
                                              # Get the predictions:
                                              test_data <- model_dataset_adj_xy[test_data_i,]
                                              scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                              
                                              # Join the prediction column:
                                              test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                                  scaled_test_data))
                                              
                                              ## Remove the file that stores the best model
                                              #file.remove("my_model.h5") just keep for now
                                              
                                              return(test_data)
                                            })
# Save these results:
saveRDS(ff_tenfive_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_tenfive_test_predictions.rds")
rm(ff_tenfive_test_predictions)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# FF - 5 layers 5 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_fivefive_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                          function(fold_i) {
                                            
                                            fold <- lowo_game_id_folds[[fold_i]]
                                            
                                            # Train the model on the training data:
                                            train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                    fold$train_game_ids)
                                            test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                   fold$test_game_ids)
                                            # Get the training data covariates
                                            train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                             adj_covariate_names])
                                            # Create scaled version:
                                            scaled_train_data_cov <- scale(train_data_cov)
                                            train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                            
                                            # Specify the neural network model using the bc-models.R code:
                                            nn_model <- keras_model_sequential() %>%
                                              layer_dense(units = 5, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 5, activation = "relu", 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 5, activation = "relu", 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 5, activation = "relu", 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 5, activation = "relu") %>%
                                              layer_dense(units = 1)
                                            
                                            nn_model %>% compile(
                                              loss = "mse",
                                              optimizer = "adam", 
                                              metrics = list("mean_squared_error"))
                                            
                                            callbacks_list <- list(
                                              callback_early_stopping(
                                                monitor = "mean_squared_error",
                                                patience = 5
                                              ), 
                                              callback_model_checkpoint(
                                                filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_fivefive_models/ff_nn_howk_",
                                                                  fold_i, "_model.h5"),
                                                monitor = "val_loss",
                                                save_best_only = TRUE)
                                            )
                                            
                                            nn_model %>% fit(
                                              scaled_train_data_cov,
                                              train_data_y,
                                              epochs = 100,
                                              validation_split = 0.2,
                                              verbose = 0, 
                                              callbacks = callbacks_list
                                            )
                                            
                                            ## Load the model w/ the best parameters 
                                            nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_fivefive_models/ff_nn_howk_",
                                                                                      fold_i, "_model.h5"))
                                            
                                            # Get the predictions:
                                            test_data <- model_dataset_adj_xy[test_data_i,]
                                            scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                            
                                            # Join the prediction column:
                                            test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                                scaled_test_data))
                                            
                                            ## Remove the file that stores the best model
                                            #file.remove("my_model.h5") just keep for now
                                            
                                            return(test_data)
                                          })
# Save these results:
saveRDS(ff_fivefive_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_fivefive_test_predictions.rds")
rm(ff_fivefive_test_predictions)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# FF - Three layers 10 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_threeten_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                            function(fold_i) {
                                              
                                              fold <- lowo_game_id_folds[[fold_i]]
                                              
                                              # Train the model on the training data:
                                              train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                      fold$train_game_ids)
                                              test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                     fold$test_game_ids)
                                              # Get the training data covariates
                                              train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                               adj_covariate_names])
                                              # Create scaled version:
                                              scaled_train_data_cov <- scale(train_data_cov)
                                              train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                              
                                              # Specify the neural network model using the bc-models.R code:
                                              nn_model <- keras_model_sequential() %>%
                                                layer_dense(units = 10, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 10, activation = "relu", 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 10, activation = "relu") %>%
                                                layer_dense(units = 1)
                                              
                                              nn_model %>% compile(
                                                loss = "mse",
                                                optimizer = "adam", 
                                                metrics = list("mean_squared_error"))
                                              
                                              callbacks_list <- list(
                                                callback_early_stopping(
                                                  monitor = "mean_squared_error",
                                                  patience = 5
                                                ), 
                                                callback_model_checkpoint(
                                                  filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_threeten_models/ff_nn_howk_",
                                                                    fold_i, "_model.h5"),
                                                  monitor = "val_loss",
                                                  save_best_only = TRUE)
                                              )
                                              
                                              nn_model %>% fit(
                                                scaled_train_data_cov,
                                                train_data_y,
                                                epochs = 100,
                                                validation_split = 0.2,
                                                verbose = 0, 
                                                callbacks = callbacks_list
                                              )
                                              
                                              ## Load the model w/ the best parameters 
                                              nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_threeten_models/ff_nn_howk_",
                                                                                        fold_i, "_model.h5"))
                                              
                                              # Get the predictions:
                                              test_data <- model_dataset_adj_xy[test_data_i,]
                                              scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                              
                                              # Join the prediction column:
                                              test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                                  scaled_test_data))
                                              
                                              ## Remove the file that stores the best model
                                              #file.remove("my_model.h5") just keep for now
                                              
                                              return(test_data)
                                            })
# Save these results:
saveRDS(ff_threeten_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_threeten_test_predictions.rds")
rm(ff_threeten_test_predictions)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# FF - 5 layers 10 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_fiveten_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                           function(fold_i) {
                                             
                                             fold <- lowo_game_id_folds[[fold_i]]
                                             
                                             # Train the model on the training data:
                                             train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                     fold$train_game_ids)
                                             test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                    fold$test_game_ids)
                                             # Get the training data covariates
                                             train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                              adj_covariate_names])
                                             # Create scaled version:
                                             scaled_train_data_cov <- scale(train_data_cov)
                                             train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                             
                                             # Specify the neural network model using the bc-models.R code:
                                             nn_model <- keras_model_sequential() %>%
                                               layer_dense(units = 10, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 10, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 10, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 10, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 10, activation = "relu") %>%
                                               layer_dense(units = 1)
                                             
                                             nn_model %>% compile(
                                               loss = "mse",
                                               optimizer = "adam", 
                                               metrics = list("mean_squared_error"))
                                             
                                             callbacks_list <- list(
                                               callback_early_stopping(
                                                 monitor = "mean_squared_error",
                                                 patience = 5
                                               ), 
                                               callback_model_checkpoint(
                                                 filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_fiveten_models/ff_nn_howk_",
                                                                   fold_i, "_model.h5"),
                                                 monitor = "val_loss",
                                                 save_best_only = TRUE)
                                             )
                                             
                                             nn_model %>% fit(
                                               scaled_train_data_cov,
                                               train_data_y,
                                               epochs = 100,
                                               validation_split = 0.2,
                                               verbose = 0, 
                                               callbacks = callbacks_list
                                             )
                                             
                                             ## Load the model w/ the best parameters 
                                             nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_fiveten_models/ff_nn_howk_",
                                                                                       fold_i, "_model.h5"))
                                             
                                             # Get the predictions:
                                             test_data <- model_dataset_adj_xy[test_data_i,]
                                             scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                             
                                             # Join the prediction column:
                                             test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                                 scaled_test_data))
                                             
                                             ## Remove the file that stores the best model
                                             #file.remove("my_model.h5") just keep for now
                                             
                                             return(test_data)
                                           })
# Save these results:
saveRDS(ff_fiveten_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_fiveten_test_predictions.rds")
rm(ff_fiveten_test_predictions)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# FF - Ten layers 10 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_tenten_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                          function(fold_i) {
                                            
                                            fold <- lowo_game_id_folds[[fold_i]]
                                            
                                            # Train the model on the training data:
                                            train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                    fold$train_game_ids)
                                            test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                   fold$test_game_ids)
                                            # Get the training data covariates
                                            train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                             adj_covariate_names])
                                            # Create scaled version:
                                            scaled_train_data_cov <- scale(train_data_cov)
                                            train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                            
                                            # Specify the neural network model using the bc-models.R code:
                                            nn_model <- keras_model_sequential() %>%
                                              layer_dense(units = 10, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 10, activation = "relu", 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 10, activation = "relu", 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 10, activation = "relu", 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 10, activation = "relu", 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 10, activation = "relu", 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 10, activation = "relu", 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 10, activation = "relu", 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 10, activation = "relu", 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 10, activation = "relu") %>%
                                              layer_dense(units = 1)
                                            
                                            nn_model %>% compile(
                                              loss = "mse",
                                              optimizer = "adam", 
                                              metrics = list("mean_squared_error"))
                                            
                                            callbacks_list <- list(
                                              callback_early_stopping(
                                                monitor = "mean_squared_error",
                                                patience = 5
                                              ), 
                                              callback_model_checkpoint(
                                                filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_tenten_models/ff_nn_howk_",
                                                                  fold_i, "_model.h5"),
                                                monitor = "val_loss",
                                                save_best_only = TRUE)
                                            )
                                            
                                            nn_model %>% fit(
                                              scaled_train_data_cov,
                                              train_data_y,
                                              epochs = 100,
                                              validation_split = 0.2,
                                              verbose = 0, 
                                              callbacks = callbacks_list
                                            )
                                            
                                            ## Load the model w/ the best parameters 
                                            nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_tenten_models/ff_nn_howk_",
                                                                                      fold_i, "_model.h5"))
                                            
                                            # Get the predictions:
                                            test_data <- model_dataset_adj_xy[test_data_i,]
                                            scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                            
                                            # Join the prediction column:
                                            test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                                scaled_test_data))
                                            
                                            ## Remove the file that stores the best model
                                            #file.remove("my_model.h5") just keep for now
                                            
                                            return(test_data)
                                          })
# Save these results:
saveRDS(ff_tenten_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_tenten_test_predictions.rds")
rm(ff_tenten_test_predictions)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# FF - Three layers 20 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_threetwenty_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                           function(fold_i) {
                                             
                                             fold <- lowo_game_id_folds[[fold_i]]
                                             
                                             # Train the model on the training data:
                                             train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                     fold$train_game_ids)
                                             test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                    fold$test_game_ids)
                                             # Get the training data covariates
                                             train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                              adj_covariate_names])
                                             # Create scaled version:
                                             scaled_train_data_cov <- scale(train_data_cov)
                                             train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                             
                                             # Specify the neural network model using the bc-models.R code:
                                             nn_model <- keras_model_sequential() %>%
                                               layer_dense(units = 20, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 20, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 20, activation = "relu") %>%
                                               layer_dense(units = 1)
                                             
                                             nn_model %>% compile(
                                               loss = "mse",
                                               optimizer = "adam", 
                                               metrics = list("mean_squared_error"))
                                             
                                             callbacks_list <- list(
                                               callback_early_stopping(
                                                 monitor = "mean_squared_error",
                                                 patience = 5
                                               ), 
                                               callback_model_checkpoint(
                                                 filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_threetwenty_models/ff_nn_howk_",
                                                                   fold_i, "_model.h5"),
                                                 monitor = "val_loss",
                                                 save_best_only = TRUE)
                                             )
                                             
                                             nn_model %>% fit(
                                               scaled_train_data_cov,
                                               train_data_y,
                                               epochs = 100,
                                               validation_split = 0.2,
                                               verbose = 0, 
                                               callbacks = callbacks_list
                                             )
                                             
                                             ## Load the model w/ the best parameters 
                                             nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_threetwenty_models/ff_nn_howk_",
                                                                                       fold_i, "_model.h5"))
                                             
                                             # Get the predictions:
                                             test_data <- model_dataset_adj_xy[test_data_i,]
                                             scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                             
                                             # Join the prediction column:
                                             test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                                 scaled_test_data))
                                             
                                             ## Remove the file that stores the best model
                                             #file.remove("my_model.h5") just keep for now
                                             
                                             return(test_data)
                                           })
# Save these results:
saveRDS(ff_threetwenty_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_threetwenty_test_predictions.rds")
rm(ff_threetwenty_test_predictions)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# FF - 5 layers 20 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_fivetwenty_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                          function(fold_i) {
                                            
                                            fold <- lowo_game_id_folds[[fold_i]]
                                            
                                            # Train the model on the training data:
                                            train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                    fold$train_game_ids)
                                            test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                   fold$test_game_ids)
                                            # Get the training data covariates
                                            train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                             adj_covariate_names])
                                            # Create scaled version:
                                            scaled_train_data_cov <- scale(train_data_cov)
                                            train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                            
                                            # Specify the neural network model using the bc-models.R code:
                                            nn_model <- keras_model_sequential() %>%
                                              layer_dense(units = 20, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 20, activation = "relu", 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 20, activation = "relu", 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 20, activation = "relu", 
                                                          kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                              layer_dense(units = 20, activation = "relu") %>%
                                              layer_dense(units = 1)
                                            
                                            nn_model %>% compile(
                                              loss = "mse",
                                              optimizer = "adam", 
                                              metrics = list("mean_squared_error"))
                                            
                                            callbacks_list <- list(
                                              callback_early_stopping(
                                                monitor = "mean_squared_error",
                                                patience = 5
                                              ), 
                                              callback_model_checkpoint(
                                                filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_fivetwenty_models/ff_nn_howk_",
                                                                  fold_i, "_model.h5"),
                                                monitor = "val_loss",
                                                save_best_only = TRUE)
                                            )
                                            
                                            nn_model %>% fit(
                                              scaled_train_data_cov,
                                              train_data_y,
                                              epochs = 100,
                                              validation_split = 0.2,
                                              verbose = 0, 
                                              callbacks = callbacks_list
                                            )
                                            
                                            ## Load the model w/ the best parameters 
                                            nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_fivetwenty_models/ff_nn_howk_",
                                                                                      fold_i, "_model.h5"))
                                            
                                            # Get the predictions:
                                            test_data <- model_dataset_adj_xy[test_data_i,]
                                            scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                            
                                            # Join the prediction column:
                                            test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                                scaled_test_data))
                                            
                                            ## Remove the file that stores the best model
                                            #file.remove("my_model.h5") just keep for now
                                            
                                            return(test_data)
                                          })
# Save these results:
saveRDS(ff_fivetwenty_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_fivetwenty_test_predictions.rds")
rm(ff_fivetwenty_test_predictions)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# FF - Ten layers 20 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_tentwenty_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                         function(fold_i) {
                                           
                                           fold <- lowo_game_id_folds[[fold_i]]
                                           
                                           # Train the model on the training data:
                                           train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                   fold$train_game_ids)
                                           test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                  fold$test_game_ids)
                                           # Get the training data covariates
                                           train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                            adj_covariate_names])
                                           # Create scaled version:
                                           scaled_train_data_cov <- scale(train_data_cov)
                                           train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                           
                                           # Specify the neural network model using the bc-models.R code:
                                           nn_model <- keras_model_sequential() %>%
                                             layer_dense(units = 20, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 20, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 20, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 20, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 20, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 20, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 20, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 20, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 20, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 20, activation = "relu") %>%
                                             layer_dense(units = 1)
                                           
                                           nn_model %>% compile(
                                             loss = "mse",
                                             optimizer = "adam", 
                                             metrics = list("mean_squared_error"))
                                           
                                           callbacks_list <- list(
                                             callback_early_stopping(
                                               monitor = "mean_squared_error",
                                               patience = 5
                                             ), 
                                             callback_model_checkpoint(
                                               filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_tentwenty_models/ff_nn_howk_",
                                                                 fold_i, "_model.h5"),
                                               monitor = "val_loss",
                                               save_best_only = TRUE)
                                           )
                                           
                                           nn_model %>% fit(
                                             scaled_train_data_cov,
                                             train_data_y,
                                             epochs = 100,
                                             validation_split = 0.2,
                                             verbose = 0, 
                                             callbacks = callbacks_list
                                           )
                                           
                                           ## Load the model w/ the best parameters 
                                           nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_tentwenty_models/ff_nn_howk_",
                                                                                     fold_i, "_model.h5"))
                                           
                                           # Get the predictions:
                                           test_data <- model_dataset_adj_xy[test_data_i,]
                                           scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                           
                                           # Join the prediction column:
                                           test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                               scaled_test_data))
                                           
                                           ## Remove the file that stores the best model
                                           #file.remove("my_model.h5") just keep for now
                                           
                                           return(test_data)
                                         })
# Save these results:
saveRDS(ff_tentwenty_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_tentwenty_test_predictions.rds")
rm(ff_tentwenty_test_predictions)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# FF - 2 layers 50 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_twofifty_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                            function(fold_i) {
                                              
                                              fold <- lowo_game_id_folds[[fold_i]]
                                              
                                              # Train the model on the training data:
                                              train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                      fold$train_game_ids)
                                              test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                     fold$test_game_ids)
                                              # Get the training data covariates
                                              train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                               adj_covariate_names])
                                              # Create scaled version:
                                              scaled_train_data_cov <- scale(train_data_cov)
                                              train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                              
                                              # Specify the neural network model using the bc-models.R code:
                                              nn_model <- keras_model_sequential() %>%
                                                layer_dense(units = 50, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 50, activation = "relu", 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                #layer_dense(units = 5, activation = "relu") %>%
                                                layer_dense(units = 1)
                                              
                                              nn_model %>% compile(
                                                loss = "mse",
                                                optimizer = "adam", 
                                                metrics = list("mean_squared_error"))
                                              
                                              callbacks_list <- list(
                                                callback_early_stopping(
                                                  monitor = "mean_squared_error",
                                                  patience = 5
                                                ), 
                                                callback_model_checkpoint(
                                                  filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_twofifty_models/ff_nn_howk_",
                                                                    fold_i, "_model.h5"),
                                                  monitor = "val_loss",
                                                  save_best_only = TRUE)
                                              )
                                              
                                              nn_model %>% fit(
                                                scaled_train_data_cov,
                                                train_data_y,
                                                epochs = 100,
                                                validation_split = 0.2,
                                                verbose = 0, 
                                                callbacks = callbacks_list
                                              )
                                              
                                              ## Load the model w/ the best parameters 
                                              nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_twofifty_models/ff_nn_howk_",
                                                                                        fold_i, "_model.h5"))
                                              
                                              # Get the predictions:
                                              test_data <- model_dataset_adj_xy[test_data_i,]
                                              scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                              
                                              # Join the prediction column:
                                              test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                                  scaled_test_data))
                                              
                                              ## Remove the file that stores the best model
                                              #file.remove("my_model.h5") just keep for now
                                              
                                              return(test_data)
                                            })
# Save these results:
saveRDS(ff_twofifty_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_twofifty_test_predictions.rds")
rm(ff_twofifty_test_predictions)


# ------------------------------------------------------------------------------
# FF - 2 layers 100 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_two100_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                           function(fold_i) {
                                             
                                             fold <- lowo_game_id_folds[[fold_i]]
                                             
                                             # Train the model on the training data:
                                             train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                     fold$train_game_ids)
                                             test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                    fold$test_game_ids)
                                             # Get the training data covariates
                                             train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                              adj_covariate_names])
                                             # Create scaled version:
                                             scaled_train_data_cov <- scale(train_data_cov)
                                             train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                             
                                             # Specify the neural network model using the bc-models.R code:
                                             nn_model <- keras_model_sequential() %>%
                                               layer_dense(units = 100, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 100, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               #layer_dense(units = 5, activation = "relu") %>%
                                               layer_dense(units = 1)
                                             
                                             nn_model %>% compile(
                                               loss = "mse",
                                               optimizer = "adam", 
                                               metrics = list("mean_squared_error"))
                                             
                                             callbacks_list <- list(
                                               callback_early_stopping(
                                                 monitor = "mean_squared_error",
                                                 patience = 5
                                               ), 
                                               callback_model_checkpoint(
                                                 filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_two100_models/ff_nn_howk_",
                                                                   fold_i, "_model.h5"),
                                                 monitor = "val_loss",
                                                 save_best_only = TRUE)
                                             )
                                             
                                             nn_model %>% fit(
                                               scaled_train_data_cov,
                                               train_data_y,
                                               epochs = 100,
                                               validation_split = 0.2,
                                               verbose = 0, 
                                               callbacks = callbacks_list
                                             )
                                             
                                             ## Load the model w/ the best parameters 
                                             nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_two100_models/ff_nn_howk_",
                                                                                       fold_i, "_model.h5"))
                                             
                                             # Get the predictions:
                                             test_data <- model_dataset_adj_xy[test_data_i,]
                                             scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                             
                                             # Join the prediction column:
                                             test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                                 scaled_test_data))
                                             
                                             ## Remove the file that stores the best model
                                             #file.remove("my_model.h5") just keep for now
                                             
                                             return(test_data)
                                           })
# Save these results:
saveRDS(ff_two100_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_two100_test_predictions.rds")
rm(ff_two100_test_predictions)


# ------------------------------------------------------------------------------
# FF - Three layers 50 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_threefifty_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                           function(fold_i) {
                                             
                                             fold <- lowo_game_id_folds[[fold_i]]
                                             
                                             # Train the model on the training data:
                                             train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                     fold$train_game_ids)
                                             test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                    fold$test_game_ids)
                                             # Get the training data covariates
                                             train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                              adj_covariate_names])
                                             # Create scaled version:
                                             scaled_train_data_cov <- scale(train_data_cov)
                                             train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                             
                                             # Specify the neural network model using the bc-models.R code:
                                             nn_model <- keras_model_sequential() %>%
                                               layer_dense(units = 50, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 50, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 50, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 1)
                                             
                                             nn_model %>% compile(
                                               loss = "mse",
                                               optimizer = "adam", 
                                               metrics = list("mean_squared_error"))
                                             
                                             callbacks_list <- list(
                                               callback_early_stopping(
                                                 monitor = "mean_squared_error",
                                                 patience = 5
                                               ), 
                                               callback_model_checkpoint(
                                                 filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_threefifty_models/ff_nn_howk_",
                                                                   fold_i, "_model.h5"),
                                                 monitor = "val_loss",
                                                 save_best_only = TRUE)
                                             )
                                             
                                             nn_model %>% fit(
                                               scaled_train_data_cov,
                                               train_data_y,
                                               epochs = 100,
                                               validation_split = 0.2,
                                               verbose = 0, 
                                               callbacks = callbacks_list
                                             )
                                             
                                             ## Load the model w/ the best parameters 
                                             nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_threefifty_models/ff_nn_howk_",
                                                                                       fold_i, "_model.h5"))
                                             
                                             # Get the predictions:
                                             test_data <- model_dataset_adj_xy[test_data_i,]
                                             scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                             
                                             # Join the prediction column:
                                             test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                                 scaled_test_data))
                                             
                                             ## Remove the file that stores the best model
                                             #file.remove("my_model.h5") just keep for now
                                             
                                             return(test_data)
                                           })
# Save these results:
saveRDS(ff_threefifty_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_threefifty_test_predictions.rds")
rm(ff_threefifty_test_predictions)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# FF - Three layers 100 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_three100_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                             function(fold_i) {
                                               
                                               fold <- lowo_game_id_folds[[fold_i]]
                                               
                                               # Train the model on the training data:
                                               train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                       fold$train_game_ids)
                                               test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                      fold$test_game_ids)
                                               # Get the training data covariates
                                               train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                                adj_covariate_names])
                                               # Create scaled version:
                                               scaled_train_data_cov <- scale(train_data_cov)
                                               train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                               
                                               # Specify the neural network model using the bc-models.R code:
                                               nn_model <- keras_model_sequential() %>%
                                                 layer_dense(units = 100, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                             kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                 layer_dense(units = 100, activation = "relu", 
                                                             kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                 layer_dense(units = 100, activation = "relu", 
                                                             kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                 layer_dense(units = 1)
                                               
                                               nn_model %>% compile(
                                                 loss = "mse",
                                                 optimizer = "adam", 
                                                 metrics = list("mean_squared_error"))
                                               
                                               callbacks_list <- list(
                                                 callback_early_stopping(
                                                   monitor = "mean_squared_error",
                                                   patience = 5
                                                 ), 
                                                 callback_model_checkpoint(
                                                   filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_three100_models/ff_nn_howk_",
                                                                     fold_i, "_model.h5"),
                                                   monitor = "val_loss",
                                                   save_best_only = TRUE)
                                               )
                                               
                                               nn_model %>% fit(
                                                 scaled_train_data_cov,
                                                 train_data_y,
                                                 epochs = 100,
                                                 validation_split = 0.2,
                                                 verbose = 0, 
                                                 callbacks = callbacks_list
                                               )
                                               
                                               ## Load the model w/ the best parameters 
                                               nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_three100_models/ff_nn_howk_",
                                                                                         fold_i, "_model.h5"))
                                               
                                               # Get the predictions:
                                               test_data <- model_dataset_adj_xy[test_data_i,]
                                               scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                               
                                               # Join the prediction column:
                                               test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                                   scaled_test_data))
                                               
                                               ## Remove the file that stores the best model
                                               #file.remove("my_model.h5") just keep for now
                                               
                                               return(test_data)
                                             })
# Save these results:
saveRDS(ff_three100_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_three100_test_predictions.rds")
rm(ff_three100_test_predictions)
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# FF - 5 layers 50 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_fivefifty_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                             function(fold_i) {
                                               
                                               fold <- lowo_game_id_folds[[fold_i]]
                                               
                                               # Train the model on the training data:
                                               train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                       fold$train_game_ids)
                                               test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                      fold$test_game_ids)
                                               # Get the training data covariates
                                               train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                                adj_covariate_names])
                                               # Create scaled version:
                                               scaled_train_data_cov <- scale(train_data_cov)
                                               train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                               
                                               # Specify the neural network model using the bc-models.R code:
                                               nn_model <- keras_model_sequential() %>%
                                                 layer_dense(units = 50, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                             kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                 layer_dense(units = 50, activation = "relu", 
                                                             kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                 layer_dense(units = 50, activation = "relu", 
                                                             kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                 layer_dense(units = 50, activation = "relu", 
                                                             kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                 layer_dense(units = 50, activation = "relu", 
                                                             kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                 layer_dense(units = 1)
                                               
                                               nn_model %>% compile(
                                                 loss = "mse",
                                                 optimizer = "adam", 
                                                 metrics = list("mean_squared_error"))
                                               
                                               callbacks_list <- list(
                                                 callback_early_stopping(
                                                   monitor = "mean_squared_error",
                                                   patience = 5
                                                 ), 
                                                 callback_model_checkpoint(
                                                   filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_fivefifty_models/ff_nn_howk_",
                                                                     fold_i, "_model.h5"),
                                                   monitor = "val_loss",
                                                   save_best_only = TRUE)
                                               )
                                               
                                               nn_model %>% fit(
                                                 scaled_train_data_cov,
                                                 train_data_y,
                                                 epochs = 100,
                                                 validation_split = 0.2,
                                                 verbose = 0, 
                                                 callbacks = callbacks_list
                                               )
                                               
                                               ## Load the model w/ the best parameters 
                                               nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_fivefifty_models/ff_nn_howk_",
                                                                                         fold_i, "_model.h5"))
                                               
                                               # Get the predictions:
                                               test_data <- model_dataset_adj_xy[test_data_i,]
                                               scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                               
                                               # Join the prediction column:
                                               test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                                   scaled_test_data))
                                               
                                               ## Remove the file that stores the best model
                                               #file.remove("my_model.h5") just keep for now
                                               
                                               return(test_data)
                                             })
# Save these results:
saveRDS(ff_fivefifty_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_fivefifty_test_predictions.rds")
rm(ff_fivefifty_test_predictions)
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# FF - 5 layers 100 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_five100_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                            function(fold_i) {
                                              
                                              fold <- lowo_game_id_folds[[fold_i]]
                                              
                                              # Train the model on the training data:
                                              train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                      fold$train_game_ids)
                                              test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                     fold$test_game_ids)
                                              # Get the training data covariates
                                              train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                               adj_covariate_names])
                                              # Create scaled version:
                                              scaled_train_data_cov <- scale(train_data_cov)
                                              train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                              
                                              # Specify the neural network model using the bc-models.R code:
                                              nn_model <- keras_model_sequential() %>%
                                                layer_dense(units = 100, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 100, activation = "relu", 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 100, activation = "relu", 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 100, activation = "relu", 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 100, activation = "relu", 
                                                            kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                                layer_dense(units = 1)
                                              
                                              nn_model %>% compile(
                                                loss = "mse",
                                                optimizer = "adam", 
                                                metrics = list("mean_squared_error"))
                                              
                                              callbacks_list <- list(
                                                callback_early_stopping(
                                                  monitor = "mean_squared_error",
                                                  patience = 5
                                                ), 
                                                callback_model_checkpoint(
                                                  filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_five100_models/ff_nn_howk_",
                                                                    fold_i, "_model.h5"),
                                                  monitor = "val_loss",
                                                  save_best_only = TRUE)
                                              )
                                              
                                              nn_model %>% fit(
                                                scaled_train_data_cov,
                                                train_data_y,
                                                epochs = 100,
                                                validation_split = 0.2,
                                                verbose = 0, 
                                                callbacks = callbacks_list
                                              )
                                              
                                              ## Load the model w/ the best parameters 
                                              nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_five100_models/ff_nn_howk_",
                                                                                        fold_i, "_model.h5"))
                                              
                                              # Get the predictions:
                                              test_data <- model_dataset_adj_xy[test_data_i,]
                                              scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                              
                                              # Join the prediction column:
                                              test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                                  scaled_test_data))
                                              
                                              ## Remove the file that stores the best model
                                              #file.remove("my_model.h5") just keep for now
                                              
                                              return(test_data)
                                            })
# Save these results:
saveRDS(ff_five100_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_five100_test_predictions.rds")
rm(ff_five100_test_predictions)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# FF - Ten layers 50 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_tenfifty_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                         function(fold_i) {
                                           
                                           fold <- lowo_game_id_folds[[fold_i]]
                                           
                                           # Train the model on the training data:
                                           train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                   fold$train_game_ids)
                                           test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                  fold$test_game_ids)
                                           # Get the training data covariates
                                           train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                            adj_covariate_names])
                                           # Create scaled version:
                                           scaled_train_data_cov <- scale(train_data_cov)
                                           train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                           
                                           # Specify the neural network model using the bc-models.R code:
                                           nn_model <- keras_model_sequential() %>%
                                             layer_dense(units = 50, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 50, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 50, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 50, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 50, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 50, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 50, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 50, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 50, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 50, activation = "relu", 
                                                         kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                             layer_dense(units = 1)
                                           
                                           nn_model %>% compile(
                                             loss = "mse",
                                             optimizer = "adam", 
                                             metrics = list("mean_squared_error"))
                                           
                                           callbacks_list <- list(
                                             callback_early_stopping(
                                               monitor = "mean_squared_error",
                                               patience = 5
                                             ), 
                                             callback_model_checkpoint(
                                               filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_tenfifty_models/ff_nn_howk_",
                                                                 fold_i, "_model.h5"),
                                               monitor = "val_loss",
                                               save_best_only = TRUE)
                                           )
                                           
                                           nn_model %>% fit(
                                             scaled_train_data_cov,
                                             train_data_y,
                                             epochs = 100,
                                             validation_split = 0.2,
                                             verbose = 0, 
                                             callbacks = callbacks_list
                                           )
                                           
                                           ## Load the model w/ the best parameters 
                                           nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_tenfifty_models/ff_nn_howk_",
                                                                                     fold_i, "_model.h5"))
                                           
                                           # Get the predictions:
                                           test_data <- model_dataset_adj_xy[test_data_i,]
                                           scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                           
                                           # Join the prediction column:
                                           test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                               scaled_test_data))
                                           
                                           ## Remove the file that stores the best model
                                           #file.remove("my_model.h5") just keep for now
                                           
                                           return(test_data)
                                         })
# Save these results:
saveRDS(ff_tenfifty_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_tenfifty_test_predictions.rds")
rm(ff_tenfifty_test_predictions)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# FF - Ten layers 50 units each

plan(multiprocess, workers = length(lowo_game_id_folds)) 
ff_ten100_test_predictions <- future_map(1:length(lowo_game_id_folds),
                                           function(fold_i) {
                                             
                                             fold <- lowo_game_id_folds[[fold_i]]
                                             
                                             # Train the model on the training data:
                                             train_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                     fold$train_game_ids)
                                             test_data_i <- which(model_dataset_adj_xy$gameId %in% 
                                                                    fold$test_game_ids)
                                             # Get the training data covariates
                                             train_data_cov <- as.matrix(model_dataset_adj_xy[train_data_i,
                                                                                              adj_covariate_names])
                                             # Create scaled version:
                                             scaled_train_data_cov <- scale(train_data_cov)
                                             train_data_y <- model_dataset_adj_xy$field_x_change[train_data_i]
                                             
                                             # Specify the neural network model using the bc-models.R code:
                                             nn_model <- keras_model_sequential() %>%
                                               layer_dense(units = 100, activation = "relu", input_shape = dim(scaled_train_data_cov)[2], 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 100, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 100, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 100, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 100, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 100, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 100, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 100, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 100, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 100, activation = "relu", 
                                                           kernel_regularizer = regularizer_l1(l = 0.005)) %>%
                                               layer_dense(units = 1)
                                             
                                             nn_model %>% compile(
                                               loss = "mse",
                                               optimizer = "adam", 
                                               metrics = list("mean_squared_error"))
                                             
                                             callbacks_list <- list(
                                               callback_early_stopping(
                                                 monitor = "mean_squared_error",
                                                 patience = 5
                                               ), 
                                               callback_model_checkpoint(
                                                 filepath = paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_ten100_models/ff_nn_howk_",
                                                                   fold_i, "_model.h5"),
                                                 monitor = "val_loss",
                                                 save_best_only = TRUE)
                                             )
                                             
                                             nn_model %>% fit(
                                               scaled_train_data_cov,
                                               train_data_y,
                                               epochs = 100,
                                               validation_split = 0.2,
                                               verbose = 0, 
                                               callbacks = callbacks_list
                                             )
                                             
                                             ## Load the model w/ the best parameters 
                                             nn_model <- keras::load_model_hdf5(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/ff_ten100_models/ff_nn_howk_",
                                                                                       fold_i, "_model.h5"))
                                             
                                             # Get the predictions:
                                             test_data <- model_dataset_adj_xy[test_data_i,]
                                             scaled_test_data <- scale(as.matrix(test_data[, adj_covariate_names]))
                                             
                                             # Join the prediction column:
                                             test_data$pred_field_x_change <- as.numeric(predict(nn_model,
                                                                                                 scaled_test_data))
                                             
                                             ## Remove the file that stores the best model
                                             #file.remove("my_model.h5") just keep for now
                                             
                                             return(test_data)
                                           })
# Save these results:
saveRDS(ff_ten100_test_predictions, "src/modeling/lowo_model_datasets_adj_xydir_1022/ff_ten100_test_predictions.rds")
rm(ff_ten100_test_predictions)
# ------------------------------------------------------------------------------

