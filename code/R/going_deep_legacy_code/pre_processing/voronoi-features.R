# Nicholas Granered, edited by Lee Richardson
# Purpose: Compute the Voronoi diagrams for each frame 
#base_directory <- "/home/lee/Dropbox/NFL-EP-tracking"
# Ron edits for directory path to update the data
base_directory <- getwd()
devtools::load_all(paste0(base_directory, "/src/goingdeepR"))
library(readr)
library(stringr)
library(deldir)
library(prodlim)

## Read in the raw design matrix 
#design_matrix_path <- paste0(base_directory, "/data/initial_bc_design_matrices/rusher_bc_design_matrix.csv")
design_matrix_path <- paste0(base_directory, "/data/initial_bc_design_matrices/rusher_bc_design_matrix_0923.csv")

design_matrix <- readr::read_csv(file=design_matrix_path)

## Save out the raw computed values 
#v_summary <- readRDS(file=paste0(base_directory, "/data/voronoi/v_summmary.rds"))
v_summary <- readRDS(file=paste0(base_directory, "/data/voronoi/v_summmary_0923.rds"))
#v_points <- readRDS(file=paste0(base_directory, "/data/voronoi/v_points.rds"))
v_points <- readRDS(file=paste0(base_directory, "/data/voronoi/v_points_0923.rds"))

# --- Maximum and Minimum ----
bc_minmax_x <- apply(X = v_points, MARGIN = 3, FUN = goingdeepR::v_bc_minmax)

## Determine whether it's the min or the max based on the target variable
bc_range <- matrix(NA, nrow = nrow(design_matrix), ncol = 2)

for (i in 1:nrow(design_matrix)) {
  ## Skip this frame if it's an NA
  if (is.na(design_matrix$target_x[i])) { next }
  
  if (design_matrix$target_x[i] == 0) {
    bc_range[i, 1] <- min(bc_minmax_x[, i])
    bc_range[i, 2] <- max(bc_minmax_x[, i])
  } else if (design_matrix$target_x[i] == 120) {
    bc_range[i, 1] <- max(bc_minmax_x[, i])
    bc_range[i, 2] <- min(bc_minmax_x[, i])
  }
}

# --- Offensive Bubble ---

# Determine whether each ball-carrier is in an "offensive bubble", AKA whether every edge of their Voronoi is
# an edge that they share with one of their teammates. Takes values 0 or 1
bc_voronoi_offensive_bubble <- vector(mode="integer", length=nrow(design_matrix))

for (i in 1:nrow(design_matrix)) {
  if ( sum( !is.na(v_points[12:21, 3:6, i]) ) > 0) {
    bc_voronoi_offensive_bubble[i] <- 0
  } else { 
    bc_voronoi_offensive_bubble[i] <- 1
  }
}

## Bind the created features to the design matrix and save 
design_matrix$voronoi_bc_close <- bc_range[, 1]
design_matrix$voronoi_bc_far <- bc_range[, 2]
design_matrix$voronoi_bc_area <- v_summary[1, 8, ]
design_matrix$voronoi_bc_bubble <- bc_voronoi_offensive_bubble

## Write out the new design matrix w/ the appended features 
#voronoi_path <- paste0(base_directory, "/data/initial_bc_design_matrices/rusher_bc_design_matrix_voronoi.csv")
voronoi_path <- paste0(base_directory, "/data/initial_bc_design_matrices/rusher_bc_design_matrix_voronoi_0923.csv")
readr::write_csv(x=design_matrix, path=voronoi_path)
