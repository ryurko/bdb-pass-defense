# Lee Richardson 
# Purpose: Append together all ball carrier design matrices for individual games 

base_directory <- "/home/lee/Dropbox/NFL-EP-tracking"
output_directory <- file.path(base_directory, "data/ball_carrier_frames")
bc_files <- list.files(output_directory, full.names = TRUE)

## Append together the ball-carrier design matrix for each game 
for (file in bc_files) {
  game_df <- suppressMessages(readr::read_csv(file=file))
  if (ncol(game_df) != 177) { print(paste0("Skipping game ", file)); next }
  bc_design_matrix <- goingdeepR::append_df(df=game_df, full_df=bc_design_matrix, iter=file, iter_list=bc_files)
}

## Convert the character columns to numerics 
last_char_column <- which(names(bc_design_matrix) == "bc_orientation")
for (i in (last_char_column+1):ncol(bc_design_matrix)) {
  bc_design_matrix[, i] <- as.numeric(unlist(bc_design_matrix[,i]))
}

## Write out the final, appended design matrix 
readr::write_csv(x=bc_design_matrix, path=file.path(base_directory, "data/bc-design-matrix.csv"))
