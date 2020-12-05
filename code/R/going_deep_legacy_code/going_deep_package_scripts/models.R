#' Compute the MSE
#'
#' @param diff
#'
mse_fun <- function(diff) { return( diff^2 %>% mean() ) }
