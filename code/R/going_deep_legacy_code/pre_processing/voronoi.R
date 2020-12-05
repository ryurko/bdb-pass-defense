# Nicholas Granered, edited by Lee Richardson
# Purpose: Compute the Voronoi diagrams for each frame 
#base_directory <- "/home/lee/Dropbox/NFL-EP-tracking"
# Ron edits for directory path to update the data
base_directory <- getwd()
devtools::load_all(paste0(base_directory, "/src/goingdeepR"))
library(tidyverse)
#library(readr)
#library(stringr)
library(deldir)
library(prodlim)

#design_matrix_path <- paste0(base_directory, "/data/initial_bc_design_matrices/rusher_bc_design_matrix.csv")
design_matrix_path <- paste0(base_directory, "/data/initial_bc_design_matrices/rusher_bc_design_matrix_0923.csv")



design_matrix <- readr::read_csv(file = design_matrix_path)

# v_summary: summary output for each Voronoi tesselation 
# v_points: 3D array of vertex and edge information for each Voronoi tesselation. 
# Each row is a combination of the numbers 1:22, and the x and y coordinates of the 
# of the vertices if the two players are an edge of the tesselation. Index 1 is the ball-carrier,
# indices 2:11 are offense, and indices 12:22 are defense. 
num_players <- 22
num_combs <- choose(num_players, 2)
num_frames <- nrow(design_matrix)
v_summary <- array(NA, c(num_players, 9, nrow(design_matrix)))
v_points <- array(data = matrix(c(t(combn(num_players, 2)), rep(NA, num_combs * 4)), byrow=F, nrow=num_combs),
                  dim = c(num_combs, 6, nrow(design_matrix)))
v_point_rows <- v_points[, 1:2, 1]

## Get the columns for the (x, y) coordinates for the different groups in the design matrix 
bc_x_col <- str_detect(string=names(design_matrix), pattern="bc_x")
bc_y_col <- str_detect(string=names(design_matrix), pattern="bc_y")

offense_x_cols <- str_starts(string=names(design_matrix), pattern="offense") & str_ends(string=names(design_matrix), pattern="_x")
offense_y_cols <- str_starts(string=names(design_matrix), pattern="offense") & str_ends(string=names(design_matrix), pattern="_y")

defense_x_cols <- str_starts(string=names(design_matrix), pattern="defense") & str_ends(string=names(design_matrix), pattern="_x")
defense_y_cols <- str_starts(string=names(design_matrix), pattern="defense") & str_ends(string=names(design_matrix), pattern="_y")

## Construct a Voronoi tesselation for each 
for (frame in 1:num_frames) {
  ## Get the (x, y) coordinates for the ball-carrier, offernse, and defense 
  x_coords <- design_matrix[frame, (bc_x_col + offense_x_cols + defense_x_cols) %>% as.logical] %>% as.numeric 
  y_coords <- design_matrix[frame, (bc_y_col + offense_y_cols + defense_y_cols) %>% as.logical] %>% as.numeric

  ## Make sure the coordinates are within the boundaries
  coord_df <- data.frame(x = pmax(0, pmin(120, x_coords)), 
                         y = pmax(0, pmin(53.3, y_coords)))
  
  ## If there's a missing player, indicate which index is missing, then 
  ## remove this index before computing the voronoi diagram 
  missing_player_inds <- complete.cases(coord_df)
  if ( all(missing_player_inds) == FALSE ) {
    coord_df <- coord_df[missing_player_inds, ]
  }
  
  ## Jitter the coordinates if any are duplicated. Make sure this doesn't
  ## create another duplicate 
  duplicates <- duplicated(coord_df)
  if (any(duplicates)) {  
    coord_df$x[duplicates] <- coord_df$x[duplicates] + .02
    stopifnot( all(duplicated(coord_df) == FALSE) )
  }

  ## Compute the Voronoi diagram for these coordinates 
  voronoi <- deldir::deldir(coord_df, rw = c(0, 120, 0, 53.3))
  
  ## Store the results in our large arrays 
  v_summary[missing_player_inds, , frame] <- as.matrix(voronoi$summary)
  voronoi_edge_rows <- goingdeepR::v_filter(voronoi=voronoi, v_point_rows=v_point_rows, missing_inds=missing_player_inds)
  v_points[voronoi_edge_rows, 3:6, frame] <- as.matrix(voronoi$dirsgs[, 1:4])

  # ## Optionally plot the current Voronoi diagram 
  # if (frame < 100) {
  #   par(mar=c(6, 6, 6, 6))
  #   plot(voronoi$summary$x, voronoi$summary$y, ylim=c(0, 54), xlim=c(0, 120), 
  #        main="Example Voronoi Diagram", xlab="X", ylab="Y", cex.main=2, cex.lab=2)
  #   points(voronoi$summary$x[1], voronoi$summary$y[1], pch=19, col="red")
  #   points(voronoi$summary$x[2:11], voronoi$summary$y[2:11], pch=19, col="blue")
  #   points(voronoi$summary$x[12:22], voronoi$summary$y[12:22], pch=19, col="green")
  #   plot(voronoi, wlines = "tess", add = TRUE)
  #   legend("topleft", c("ball-carrier", "offense", "defense"), col=c("red", "blue", "green"), pch=19)
  # }
  # 
  # if (frame > 100) { stop() }
  
}

## Add names to the two output arrays 
colnames(v_summary) <- colnames(voronoi$summary)
colnames(v_points) <- colnames(voronoi$dirsgs)[c(5:6, 1:4)]

## Save out the raw computed values 
#saveRDS(object=v_summary, file=paste0(base_directory, "/data/voronoi/v_summmary.rds"))
saveRDS(object=v_summary, file=paste0(base_directory, "/data/voronoi/v_summmary_0923.rds"))
#saveRDS(object=v_points, file=paste0(base_directory, "/data/voronoi/v_points.rds"))
saveRDS(object=v_points, file=paste0(base_directory, "/data/voronoi/v_points_0923.rds"))
