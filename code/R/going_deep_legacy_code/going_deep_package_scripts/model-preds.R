# Francesca Matano
# Purpose: Produce prediction for lstm model

#' Computing lstm model prediction
#' This function takes a scaled matrix in input and an it computes the lstm prediction
#'
#' @param X scaled matrix of predictors
#' @param model lstm model: you can load model with load_model_hdf5(model_path)
#' @param batch_size batch size used in the lstm model. If don't know try  length of the max seq. If null the function will try to use max length
#' @param seq_ids sequence id for the X matrix, needed for padding 
#' @param frame_ids column to sort the padded X's by
lstm_pred_fun <- function (X, model, batch_size = NULL, seq_ids, frame_ids) {
  
  ## Pad the x's
  message("Padding the design matrix")
  X_new <- padding_fun(X = X, seq_ids = seq_ids, frame_ids = frame_ids)
  
  ## Creating the array of predictors
  n_predictors <- ncol(X_new)
  x_arr <- array(data = as.matrix(X_new), dim = c(nrow(X_new), 1, n_predictors))
  
  ## Predictions
  if (is.null(batch_size)) {
    batch_size = 129
    warning("Not batch_size in input, tried 129 which is the max length 
            of a sequence")
  }
  
  message("Making predictions")
  preds <- model %>%
    predict(x_arr, batch_size = batch_size) %>%
    .[,1,]
  
  ## Unpad the x's
  message("Unpadding the predictions")
  wwhich_unpad <- which(X_new[[1]] != 0)

  return(preds[wwhich_unpad])

}

#' Padding the design matrix
#' 
#' This function pads the design matrix
#' @param X design matrix
#' @param seq_ids column to use to pad the sequences
#' @param frame_ids column to sort the padded X's by
padding_fun <- function(X, seq_ids, frame_ids) {
  
  ## Create fake date col to pad the sequences
  tmp <- data.frame(X, seq_ids = seq_ids, frame_ids = frame_ids) %>% 
    group_by(seq_ids) %>%
    mutate(Date = seq(as.Date("2000/1/1"), by = "day", 
                      length.out = length(seq_ids))) %>% 
    ungroup() %>% 
    as.data.frame()
  
  ## Pad the sequences based upon date and remove date and ids
  X_new <- padr::pad(tmp, start_val = min(tmp$Date), end_val = max(tmp$Date), 
                     group = "seq_ids") %>% 
    replace(., is.na(.), 0) 
  
  X_new <- X_new %>% 
    arrange(seq_ids, frame_ids) %>% # need this to make sure the beginning is padded
    select(-c(Date, seq_ids, frame_ids))
  
  return(X_new)
}
