# PURPOSE: Create updated voronoi features such as the area of the field towards
#          which the ball-carrier is heading
# AUTHOR: Nicholas Granered
# EDITED BY: Ron Yurko

base_directory <- getwd()
devtools::load_all(paste0(base_directory, "/src/goingdeepR"))

library(tidyverse)
library(deldir)
library(prodlim)
library(retistruct)
library(pracma)

# Load the design matrix
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

#Sane as above but for when we remove all non-ball-carrier offensive players
num_players_bc_only <- 12
num_combs_bc_only <- choose(num_players_bc_only, 2)
v_summary_bc_only <- array(NA, c(num_players_bc_only, 9, nrow(design_matrix)))
v_points_bc_only <- array(data = matrix(c(t(combn(num_players_bc_only, 2)), 
                                          rep(NA, num_combs_bc_only * 4)), byrow = F, 
                                        nrow = num_combs_bc_only),
                          dim = c(num_combs_bc_only, 6, nrow(design_matrix)))
v_point_rows_bc_only <- v_points_bc_only[, 1:2, 1]

## Get the columns for the (x, y) coordinates for the different groups in the design matrix 
bc_x_col <- str_detect(string = names(design_matrix), pattern = "bc_x")
bc_y_col <- str_detect(string = names(design_matrix), pattern = "bc_y")

offense_x_cols <- str_starts(string = names(design_matrix), pattern = "offense") & 
  str_ends(string = names(design_matrix), pattern = "_x")
offense_y_cols <- str_starts(string = names(design_matrix), pattern = "offense") & 
  str_ends(string = names(design_matrix), pattern = "_y")

defense_x_cols <- str_starts(string = names(design_matrix), pattern = "defense") & 
  str_ends(string = names(design_matrix), pattern = "_x")
defense_y_cols <- str_starts(string = names(design_matrix), pattern = "defense") & 
  str_ends(string = names(design_matrix), pattern = "_y")

## Construct a Voronoi tesselation for each 
for (frame in 1:num_frames) {
  #print(frame)
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
  voronoi_edge_rows <- goingdeepR::v_filter(voronoi = voronoi, 
                                            v_point_rows = v_point_rows, 
                                            missing_inds = missing_player_inds)
  v_points[voronoi_edge_rows, 3:6, frame] <- as.matrix(voronoi$dirsgs[, 1:4])
  
  ##Same as above, but for ball-carrier only
  x_coords_bc_only <- design_matrix[frame, (bc_x_col + defense_x_cols) %>% as.logical] %>% as.numeric 
  y_coords_bc_only <- design_matrix[frame, (bc_y_col + defense_y_cols) %>% as.logical] %>% as.numeric
  
  coord_df_bc_only <- data.frame(x = pmax(0, pmin(120, x_coords_bc_only)), 
                                 y = pmax(0, pmin(53.3, y_coords_bc_only)))
  
  ## If there's a missing player, indicate which index is missing, then 
  ## remove this index before computing the voronoi diagram 
  missing_player_inds_bc_only <- complete.cases(coord_df_bc_only)
  if ( all(missing_player_inds_bc_only) == FALSE ) {
    coord_df_bc_only <- coord_df_bc_only[missing_player_inds_bc_only, ]
  }
  
  ## Jitter the coordinates if any are duplicated. Make sure this doesn't
  ## create another duplicate 
  duplicates_bc_only <- duplicated(coord_df_bc_only)
  if (any(duplicates_bc_only)) {  
    coord_df_bc_only$x[duplicates_bc_only] <- coord_df_bc_only$x[duplicates_bc_only] + .02
    stopifnot( all(duplicated(coord_df_bc_only) == FALSE) )
  }
  
  ## Compute the Voronoi diagram for these coordinates 
  voronoi_bc_only <- deldir::deldir(coord_df_bc_only, rw = c(0, 120, 0, 53.3))
  
  ## Store the results in our large arrays 
  v_summary_bc_only[missing_player_inds_bc_only, , frame] <- as.matrix(voronoi_bc_only$summary)
  voronoi_edge_rows_bc_only <- goingdeepR::v_filter(voronoi = voronoi_bc_only,
                                                    v_point_rows = v_point_rows_bc_only, 
                                                    missing_inds = missing_player_inds_bc_only)
  v_points_bc_only[voronoi_edge_rows_bc_only, 3:6, frame] <- as.matrix(voronoi_bc_only$dirsgs[, 1:4])
  
}


## Add names to the two output arrays 
colnames(v_summary) <- colnames(voronoi$summary)
colnames(v_points) <- colnames(voronoi$dirsgs)[c(5:6, 1:4)]

## Save out the raw computed values 
saveRDS(object = v_summary, file = paste0(base_directory, "/data/voronoi/v_summmary_1022.rds"))
saveRDS(object = v_points, file = paste0(base_directory, "/data/voronoi/v_points_1022.rds"))


###################################################################################################


##Messy pre-processing for doing voronoi area to a side of the ball-carrier
##The idea is that to calculate the area from just the points, the calculation will be wrong in cases where
##the voronoi is open to at least one corner. The pre-processing attempts to add back each possible combination
##of missing corners and sees which one makes the area match what deldir gives out. 
##Luckily, every singly frame matches exactly one of the tests, as seen in the checks that are commented out.
##These match-types are stored and the corresponding missing corner points are added back in to calculate the area correctly.

##Also fixes issues with min/max values in certain cases.
##Frames where the ball-carrier's tesselation is completely open to one of the endzones returned the wrong close/far value
##because there were no points in the endzone (x=0 or x=120 depending on the endzone) in the voronoi coordinate set. 
##This affected about 4000 frames

#Match area within this mutch
tol=0.0001

#All voronoi endpoints are accounted for
area_match_type1=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points[1:21,3,frame]))
  points_used_x=c(v_points[points_used,3,frame],v_points[points_used,5,frame])
  points_used_y=c(v_points[points_used,4,frame],v_points[points_used,6,frame])
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type1[frame]=abs(poly_area-v_summary[1,8,frame])
}

#Voronoi is open to the entire left side of the field
area_match_type2=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points[1:21,3,frame]))
  points_used_x=c(v_points[points_used,3,frame],v_points[points_used,5,frame],0,0)
  points_used_y=c(v_points[points_used,4,frame],v_points[points_used,6,frame],0,53.3)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type2[frame]=abs(poly_area-v_summary[1,8,frame])
}

#Voronoi is open to the lower left corner of the field
area_match_type3=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points[1:21,3,frame]))
  points_used_x=c(v_points[points_used,3,frame],v_points[points_used,5,frame],0)
  points_used_y=c(v_points[points_used,4,frame],v_points[points_used,6,frame],0)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type3[frame]=abs(poly_area-v_summary[1,8,frame])
}

#Voronoi is open to the upper left corner of the field
area_match_type4=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points[1:21,3,frame]))
  points_used_x=c(v_points[points_used,3,frame],v_points[points_used,5,frame],0)
  points_used_y=c(v_points[points_used,4,frame],v_points[points_used,6,frame],53.3)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type4[frame]=abs(poly_area-v_summary[1,8,frame])
}

#Voronoi is open to the entire right side of the field
area_match_type5=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points[1:21,3,frame]))
  points_used_x=c(v_points[points_used,3,frame],v_points[points_used,5,frame],120,120)
  points_used_y=c(v_points[points_used,4,frame],v_points[points_used,6,frame],0,53.3)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type5[frame]=abs(poly_area-v_summary[1,8,frame])
}

#Voronoi is open to the lower right corner of the field
area_match_type6=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points[1:21,3,frame]))
  points_used_x=c(v_points[points_used,3,frame],v_points[points_used,5,frame],120)
  points_used_y=c(v_points[points_used,4,frame],v_points[points_used,6,frame],0)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type6[frame]=abs(poly_area-v_summary[1,8,frame])
}

#Voronoi is open to the upper right corner of the field
area_match_type7=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points[1:21,3,frame]))
  points_used_x=c(v_points[points_used,3,frame],v_points[points_used,5,frame],120)
  points_used_y=c(v_points[points_used,4,frame],v_points[points_used,6,frame],53.3)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type7[frame]=abs(poly_area-v_summary[1,8,frame])
}

#Voronoi is open to the entire top half of the field
area_match_type8=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points[1:21,3,frame]))
  points_used_x=c(v_points[points_used,3,frame],v_points[points_used,5,frame],0,120)
  points_used_y=c(v_points[points_used,4,frame],v_points[points_used,6,frame],53.3,53.3)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type8[frame]=abs(poly_area-v_summary[1,8,frame])
}

#Voronoi is open to the entire bottom half of the field
area_match_type9=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points[1:21,3,frame]))
  points_used_x=c(v_points[points_used,3,frame],v_points[points_used,5,frame],0,120)
  points_used_y=c(v_points[points_used,4,frame],v_points[points_used,6,frame],0,0)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type9[frame]=abs(poly_area-v_summary[1,8,frame])
}


#Check to make sure that every frame matches at least one of the 7 area match types
#sum(area_match_type1 > tol &
#	area_match_type2 > tol & 
#	area_match_type3 > tol & 
#	area_match_type4 > tol & 
#	area_match_type5 > tol & 
#	area_match_type6 > tol & 
#	area_match_type7 > tol &
#	area_match_type8 > tol & 
#	area_match_type9 > tol)	== 0


#Check to make sure that every frame matches exactly one of the 7 area match types
#sum(area_match_type1 < tol) +
#sum(area_match_type2 < tol) +
#sum(area_match_type3 < tol) +
#sum(area_match_type4 < tol) +
#sum(area_match_type5 < tol) +
#sum(area_match_type6 < tol) +
#sum(area_match_type7 < tol) +
#sum(area_match_type8 < tol) +
#sum(area_match_type9 < tol) == nrow(design_matrix)


area_match_type=matrix(c((area_match_type1 < tol) ,
                         (area_match_type2 < tol) ,
                         (area_match_type3 < tol) ,
                         (area_match_type4 < tol) ,
                         (area_match_type5 < tol) ,
                         (area_match_type6 < tol) ,
                         (area_match_type7 < tol) ,
                         (area_match_type8 < tol) ,
                         (area_match_type9 < tol)),
                       nrow=nrow(design_matrix),
                       byrow=FALSE)


##Same as above but for bc_only
#All voronoi endpoints are accounted for
area_match_type1_bc_only=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points_bc_only[1:11,3,frame]))
  points_used_x=c(v_points_bc_only[points_used,3,frame],v_points_bc_only[points_used,5,frame])
  points_used_y=c(v_points_bc_only[points_used,4,frame],v_points_bc_only[points_used,6,frame])
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type1_bc_only[frame]=abs(poly_area-v_summary_bc_only[1,8,frame])
}

#Voronoi is open to the entire left side of the field
area_match_type2_bc_only=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points_bc_only[1:11,3,frame]))
  points_used_x=c(v_points_bc_only[points_used,3,frame],v_points_bc_only[points_used,5,frame],0,0)
  points_used_y=c(v_points_bc_only[points_used,4,frame],v_points_bc_only[points_used,6,frame],0,53.3)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type2_bc_only[frame]=abs(poly_area-v_summary_bc_only[1,8,frame])
}

#Voronoi is open to the lower left corner of the field
area_match_type3_bc_only=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points_bc_only[1:11,3,frame]))
  points_used_x=c(v_points_bc_only[points_used,3,frame],v_points_bc_only[points_used,5,frame],0)
  points_used_y=c(v_points_bc_only[points_used,4,frame],v_points_bc_only[points_used,6,frame],0)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type3_bc_only[frame]=abs(poly_area-v_summary_bc_only[1,8,frame])
}

#Voronoi is open to the upper left corner of the field
area_match_type4_bc_only=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points_bc_only[1:11,3,frame]))
  points_used_x=c(v_points_bc_only[points_used,3,frame],v_points_bc_only[points_used,5,frame],0)
  points_used_y=c(v_points_bc_only[points_used,4,frame],v_points_bc_only[points_used,6,frame],53.3)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type4_bc_only[frame]=abs(poly_area-v_summary_bc_only[1,8,frame])
}

#Voronoi is open to the entire right side of the field
area_match_type5_bc_only=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points_bc_only[1:11,3,frame]))
  points_used_x=c(v_points_bc_only[points_used,3,frame],v_points_bc_only[points_used,5,frame],120,120)
  points_used_y=c(v_points_bc_only[points_used,4,frame],v_points_bc_only[points_used,6,frame],0,53.3)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type5_bc_only[frame]=abs(poly_area-v_summary_bc_only[1,8,frame])
}

#Voronoi is open to the lower right corner of the field
area_match_type6_bc_only=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points_bc_only[1:11,3,frame]))
  points_used_x=c(v_points_bc_only[points_used,3,frame],v_points_bc_only[points_used,5,frame],120)
  points_used_y=c(v_points_bc_only[points_used,4,frame],v_points_bc_only[points_used,6,frame],0)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type6_bc_only[frame]=abs(poly_area-v_summary_bc_only[1,8,frame])
}

#Voronoi is open to the upper right corner of the field
area_match_type7_bc_only=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points_bc_only[1:11,3,frame]))
  points_used_x=c(v_points_bc_only[points_used,3,frame],v_points_bc_only[points_used,5,frame],120)
  points_used_y=c(v_points_bc_only[points_used,4,frame],v_points_bc_only[points_used,6,frame],53.3)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type7_bc_only[frame]=abs(poly_area-v_summary_bc_only[1,8,frame])
}

#Voronoi is open to the entire top of the field
area_match_type8_bc_only=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points_bc_only[1:11,3,frame]))
  points_used_x=c(v_points_bc_only[points_used,3,frame],v_points_bc_only[points_used,5,frame],0,120)
  points_used_y=c(v_points_bc_only[points_used,4,frame],v_points_bc_only[points_used,6,frame],53.3,53.3)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type8_bc_only[frame]=abs(poly_area-v_summary_bc_only[1,8,frame])
}

#Voronoi is open to the entire bottom of the field
area_match_type9_bc_only=rep(0,nrow(design_matrix))
for (frame in 1:nrow(design_matrix) ) {
  points_used=which(!is.na(v_points_bc_only[1:11,3,frame]))
  points_used_x=c(v_points_bc_only[points_used,3,frame],v_points_bc_only[points_used,5,frame],0,120)
  points_used_y=c(v_points_bc_only[points_used,4,frame],v_points_bc_only[points_used,6,frame],0,0)
  poly_points=chull(points_used_x,points_used_y)
  poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
  area_match_type9_bc_only[frame]=abs(poly_area-v_summary_bc_only[1,8,frame])
}


#Check to make sure that every frame matches at least one of the 7 area match types
#sum(area_match_type1_bc_only > tol &
#	area_match_type2_bc_only > tol & 
#	area_match_type3_bc_only > tol & 
#	area_match_type4_bc_only > tol & 
#	area_match_type5_bc_only > tol & 
#	area_match_type6_bc_only > tol & 
#	area_match_type7_bc_only > tol &
#	area_match_type8_bc_only > tol &
#	area_match_type9_bc_only > tol)== 0


#Check to make sure that every frame matches exactly one of the 7 area match types
#sum(area_match_type1_bc_only < tol) +
#sum(area_match_type2_bc_only < tol) +
#sum(area_match_type3_bc_only < tol) +
#sum(area_match_type4_bc_only < tol) +
#sum(area_match_type5_bc_only < tol) +
#sum(area_match_type6_bc_only < tol) +
#sum(area_match_type7_bc_only < tol) +
#sum(area_match_type8_bc_only < tol) +
#sum(area_match_type9_bc_only < tol) == nrow(design_matrix)

area_match_type_bc_only=matrix(c((area_match_type1_bc_only < tol) ,
                                 (area_match_type2_bc_only < tol) ,
                                 (area_match_type3_bc_only < tol) ,
                                 (area_match_type4_bc_only < tol) ,
                                 (area_match_type5_bc_only < tol) ,
                                 (area_match_type6_bc_only < tol) ,
                                 (area_match_type7_bc_only < tol) ,
                                 (area_match_type8_bc_only < tol) ,
                                 (area_match_type9_bc_only < tol)),
                               nrow=nrow(design_matrix),
                               byrow=FALSE)


##########Feature Creation
##This code gets the voronoi areas, closest and furthest values (correctly this time), 
##and voronoi areas towards target endzone for both the full-offense and bc_only cases

#These are the features we want to construct
voronoi_bc_close=rep(NA,nrow(design_matrix))
voronoi_bc_far=rep(NA,nrow(design_matrix))

voronoi_bc_close_bc_only=rep(NA,nrow(design_matrix))
voronoi_bc_far_bc_only=rep(NA,nrow(design_matrix))

voronoi_bc_area=v_summary[1, 8, ]
voronoi_bc_area_bc_only=v_summary_bc_only[1, 8, ]

voronoi_bc_area_in_front=rep(NA,nrow(design_matrix))
voronoi_bc_area_in_front_bc_only=rep(NA,nrow(design_matrix))


#Indicator for which endzone is the target for each frame. We loop over these separately for convenience.
#Frames_to_NA does not have well-defined values for close/far/bc area in front.
frames_to_0=which(design_matrix$target_x==0)
frames_to_120=which(design_matrix$target_x==120)
frames_to_NA=which(is.na(design_matrix$target_x))

##Adjust at 0 is the set of match types where the value closest to the x=0 endzone will be 0 (not necessarily calculated incorrectly before)
##Adjust at 120 is the set of match types where the value closest to the x=120 endzone will be 120 (not necessarily calculated incorrectly before)
adjust_at_0=c(2,3,4,8,9)
adjust_at_120=c(5,6,7,8,9)

##First, loop over frames facing the x=0 endzone
for (frame in frames_to_0) {
  #Identify match type as 1 through 9
  match_type=which(area_match_type[frame,]==TRUE)
  match_type_bc_only=which(area_match_type_bc_only[frame,]==TRUE)
  
  points_used=which(!is.na(v_points[1:21,3,frame]))
  points_used_bc_only=which(!is.na(v_points_bc_only[1:11,3,frame]))
  
  #voronoi_bc_close
  if (match_type%in%adjust_at_0) {
    voronoi_bc_close[frame] = 0
  } else { voronoi_bc_close[frame] = min(c(v_points[points_used,3,frame],v_points[points_used,5,frame]))
  }
  
  #voronoi_bc_far
  if (match_type%in%adjust_at_120) {
    voronoi_bc_far[frame] = 120
  } else { voronoi_bc_far[frame] = max(c(v_points[points_used,3,frame],v_points[points_used,5,frame]))
  }
  
  #voronoi_bc_close_bc_only
  if (match_type_bc_only%in%adjust_at_0) {
    voronoi_bc_close_bc_only[frame] = 0
  } else { voronoi_bc_close_bc_only[frame] = min(c(v_points[points_used_bc_only,3,frame],v_points[points_used_bc_only,5,frame]))
  }
  
  #voronoi_bc_far_bc_only
  if (match_type_bc_only%in%adjust_at_120) {
    voronoi_bc_far_bc_only[frame] = 120
  } else { voronoi_bc_far_bc_only[frame] = max(c(v_points[points_used_bc_only,3,frame],v_points[points_used_bc_only,5,frame]))
  }
  
  ##Voronoi area towards the x=0 endzone for the ballcarrier
  temp_points=matrix(v_points[points_used,3:6,frame],ncol=4) 
  del_points=c(which(temp_points[,1]>design_matrix[frame,bc_x_col] & temp_points[,3]>design_matrix[frame,bc_x_col]),which(temp_points[,1]==temp_points[,3] & temp_points[,2]==temp_points[,4]))
  if (length(del_points) > 0 ) {temp_points=temp_points[-del_points,]}
  
  #Catches for errors
  if ( !is.null(nrow(temp_points)) ) {
    for (i in 1:nrow(temp_points) ) {
      seg_int=line.line.intersection(temp_points[i,1:2], 
                                     temp_points[i,3:4], unlist(c(design_matrix[frame, bc_x_col] - .008, 0)),
                                     unlist(c(design_matrix[frame, bc_x_col] + .008, 53.3)), 
                                     interior.only = TRUE)
      
      #Move Voronoi coordinate along the line segment so its x=bc_x
      if (!is.na(seg_int[1]) ) {
        ind_max=which.max(c(temp_points[i,1],-1,temp_points[i,3]))
        temp_points[i,ind_max:(ind_max+1)]=seg_int
      }
    }
    
    #Calculate the area based on the appropriate area match type
    if 		  (match_type==1) {
      points_used_x=c(temp_points[,c(1,3)])
      points_used_y=c(temp_points[,c(2,4)])
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==2) {
      points_used_x=c(temp_points[,c(1,3)],0,0)
      points_used_y=c(temp_points[,c(2,4)],0,53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==3) {
      points_used_x=c(temp_points[,c(1,3)],0)
      points_used_y=c(temp_points[,c(2,4)],0)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==4) {
      points_used_x=c(temp_points[,c(1,3)],0)
      points_used_y=c(temp_points[,c(2,4)],53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==5) {
      points_used_x=c(temp_points[,c(1,3)],120,120)
      points_used_y=c(temp_points[,c(2,4)],0,53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==6) {
      points_used_x=c(temp_points[,c(1,3)],120)
      points_used_y=c(temp_points[,c(2,4)],0)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==7) {
      points_used_x=c(temp_points[,c(1,3)],120)
      points_used_y=c(temp_points[,c(2,4)],53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==8) {
      points_used_x=c(temp_points[,c(1,3)],0,120)
      points_used_y=c(temp_points[,c(2,4)],53.3,53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==9) {
      points_used_x=c(temp_points[,c(1,3)],0,120)
      points_used_y=c(temp_points[,c(2,4)],0,0)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    }
    voronoi_bc_area_in_front[frame] = poly_area
  } else {voronoi_bc_area_in_front[frame] = 0 }
  
  ##Area towards the x=0 endzone for the ballcarrier in the bc_only setup
  temp_points=matrix(v_points_bc_only[points_used_bc_only,3:6,frame],ncol=4)
  del_points=c(which(temp_points[,1]>design_matrix[frame,bc_x_col] & temp_points[,3]>design_matrix[frame,bc_x_col]),which(temp_points[,1]==temp_points[,3] & temp_points[,2]==temp_points[,4]))
  if (length(del_points) > 0 ) {temp_points=temp_points[-del_points,]}
  
  #Catches for errors
  if (!is.null(nrow(temp_points)) ) {
    for (i in 1:nrow(temp_points) ) {
      seg_int=line.line.intersection(temp_points[i,1:2],temp_points[i,3:4],
                                     unlist(c(design_matrix[frame,bc_x_col]-.005,0)),
                                     unlist(c(design_matrix[frame,bc_x_col]+.005,53.3)), interior.only=TRUE )
      
      #Move Voronoi coordinate along the line segment so its x=bc_x
      if (!is.na(seg_int[1]) ) {
        ind_max=which.max(c(temp_points[i,1],-1,temp_points[i,3]))
        temp_points[i,ind_max:(ind_max+1)]=seg_int
      }
    }
    
    #Calculate the area based on the appropriate area match type
    if 		  (match_type_bc_only==1) {
      points_used_x=c(temp_points[,c(1,3)])
      points_used_y=c(temp_points[,c(2,4)])
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==2) {
      points_used_x=c(temp_points[,c(1,3)],0,0)
      points_used_y=c(temp_points[,c(2,4)],0,53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==3) {
      points_used_x=c(temp_points[,c(1,3)],0)
      points_used_y=c(temp_points[,c(2,4)],0)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==4) {
      points_used_x=c(temp_points[,c(1,3)],0)
      points_used_y=c(temp_points[,c(2,4)],53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==5) {
      points_used_x=c(temp_points[,c(1,3)],120,120)
      points_used_y=c(temp_points[,c(2,4)],0,53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==6) {
      points_used_x=c(temp_points[,c(1,3)],120)
      points_used_y=c(temp_points[,c(2,4)],0)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==7) {
      points_used_x=c(temp_points[,c(1,3)],120)
      points_used_y=c(temp_points[,c(2,4)],53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==8) {
      points_used_x=c(temp_points[,c(1,3)],0,120)
      points_used_y=c(temp_points[,c(2,4)],53.3,53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==9) {
      points_used_x=c(temp_points[,c(1,3)],0,120)
      points_used_y=c(temp_points[,c(2,4)],0,0)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    }
    voronoi_bc_area_in_front_bc_only[frame] = poly_area
  } else {voronoi_bc_area_in_front_bc_only[frame] = 0 }
  
}

##Next, loop over frames facing the x=120 endzone. Inequalities are flipped accordingly
for (frame in frames_to_120) {
  
  #Identify match type as 1 through 9
  match_type=which(area_match_type[frame,]==TRUE)
  match_type_bc_only=which(area_match_type_bc_only[frame,]==TRUE)
  
  points_used=which(!is.na(v_points[1:21,3,frame]))
  points_used_bc_only=which(!is.na(v_points_bc_only[1:11,3,frame]))
  
  #voronoi_bc_close
  if (match_type%in%adjust_at_120) {
    voronoi_bc_close[frame] = 120
  } else { voronoi_bc_close[frame] = max(c(v_points[points_used,3,frame],v_points[points_used,5,frame]))
  }
  
  #voronoi_bc_far
  if (match_type%in%adjust_at_0) {
    voronoi_bc_far[frame] = 0
  } else { voronoi_bc_far[frame] = min(c(v_points[points_used,3,frame],v_points[points_used,5,frame]))
  }
  
  #voronoi_bc_close_bc_only
  if (match_type_bc_only%in%adjust_at_120) {
    voronoi_bc_close_bc_only[frame] = 120
  } else { voronoi_bc_close_bc_only[frame] = max(c(v_points[points_used_bc_only,3,frame],v_points[points_used_bc_only,5,frame]))
  }
  
  #voronoi_bc_far_bc_only
  if (match_type_bc_only%in%adjust_at_0) {
    voronoi_bc_far_bc_only[frame] = 0
  } else { voronoi_bc_far_bc_only[frame] = min(c(v_points[points_used_bc_only,3,frame],v_points[points_used_bc_only,5,frame]))
  }
  
  ####Now do the area to the right of the ball-carrier
  temp_points=matrix(v_points[points_used,3:6,frame],ncol=4) 
  del_points=c(which(temp_points[,1] < design_matrix[frame,bc_x_col] & temp_points[,3] < design_matrix[frame,bc_x_col]),which(temp_points[,1]==temp_points[,3] & temp_points[,2]==temp_points[,4]))
  if (length(del_points) > 0 ) {temp_points=temp_points[-del_points,]}
  
  #Catches for errors
  if ( !is.null(nrow(temp_points)) ) {
    for (i in 1:nrow(temp_points) ) {
      seg_int=line.line.intersection(temp_points[i,1:2],temp_points[i,3:4],
                                     unlist(c(design_matrix[frame,bc_x_col]-.008,0)),
                                     unlist(c(design_matrix[frame,bc_x_col]+.008,53.3)),
                                     interior.only=TRUE )
      
      #Move Voronoi coordinate along the line segment so its x=bc_x
      if (!is.na(seg_int[1]) ) {
        ind_min=which.min(c(temp_points[i,1],1000,temp_points[i,3]))
        temp_points[i,ind_min:(ind_min+1)]=seg_int
      }
    }
    
    #Calculate the area based on the appropriate area match type
    if 		  (match_type==1) {
      points_used_x=c(temp_points[,c(1,3)])
      points_used_y=c(temp_points[,c(2,4)])
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==2) {
      points_used_x=c(temp_points[,c(1,3)],0,0)
      points_used_y=c(temp_points[,c(2,4)],0,53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==3) {
      points_used_x=c(temp_points[,c(1,3)],0)
      points_used_y=c(temp_points[,c(2,4)],0)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==4) {
      points_used_x=c(temp_points[,c(1,3)],0)
      points_used_y=c(temp_points[,c(2,4)],53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==5) {
      points_used_x=c(temp_points[,c(1,3)],120,120)
      points_used_y=c(temp_points[,c(2,4)],0,53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==6) {
      points_used_x=c(temp_points[,c(1,3)],120)
      points_used_y=c(temp_points[,c(2,4)],0)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==7) {
      points_used_x=c(temp_points[,c(1,3)],120)
      points_used_y=c(temp_points[,c(2,4)],53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==8) {
      points_used_x=c(temp_points[,c(1,3)],0,120)
      points_used_y=c(temp_points[,c(2,4)],53.3,53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type==9) {
      points_used_x=c(temp_points[,c(1,3)],0,120)
      points_used_y=c(temp_points[,c(2,4)],0,0)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    }
    voronoi_bc_area_in_front[frame] = poly_area
  } else {voronoi_bc_area_in_front[frame] = 0 }
  
  ##Now do the area to the x=120 of the ball-carrier for bc_only 
  temp_points=matrix(v_points_bc_only[points_used_bc_only,3:6,frame],ncol=4)
  del_points=c(which(temp_points[,1] < design_matrix[frame,bc_x_col] & temp_points[,3] < design_matrix[frame,bc_x_col]),which(temp_points[,1]==temp_points[,3] & temp_points[,2]==temp_points[,4]))
  
  if (length(del_points) > 0 ) {temp_points=temp_points[-del_points,]}
  
  #Catches for errors
  if (!is.null(nrow(temp_points)) ) {
    for (i in 1:nrow(temp_points) ) {
      seg_int=line.line.intersection(temp_points[i,1:2],temp_points[i,3:4],
                                     unlist(c(design_matrix[frame,bc_x_col]-.005,0)),
                                     unlist(c(design_matrix[frame,bc_x_col]+.005,53.3)),
                                     interior.only=TRUE )
      
      #Move Voronoi coordinate along the line segment so its x=bc_x
      if (!is.na(seg_int[1]) ) {
        ind_min=which.min(c(temp_points[i,1],1000,temp_points[i,3]))
        temp_points[i,ind_min:(ind_min+1)]=seg_int
      }
    }
    
    #Calculate the area based on the appropriate area match type
    if 		  (match_type_bc_only==1) {
      points_used_x=c(temp_points[,c(1,3)])
      points_used_y=c(temp_points[,c(2,4)])
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==2) {
      points_used_x=c(temp_points[,c(1,3)],0,0)
      points_used_y=c(temp_points[,c(2,4)],0,53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==3) {
      points_used_x=c(temp_points[,c(1,3)],0)
      points_used_y=c(temp_points[,c(2,4)],0)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==4) {
      points_used_x=c(temp_points[,c(1,3)],0)
      points_used_y=c(temp_points[,c(2,4)],53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==5) {
      points_used_x=c(temp_points[,c(1,3)],120,120)
      points_used_y=c(temp_points[,c(2,4)],0,53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==6) {
      points_used_x=c(temp_points[,c(1,3)],120)
      points_used_y=c(temp_points[,c(2,4)],0)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==7) {
      points_used_x=c(temp_points[,c(1,3)],120)
      points_used_y=c(temp_points[,c(2,4)],53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==8) {
      points_used_x=c(temp_points[,c(1,3)],0,120)
      points_used_y=c(temp_points[,c(2,4)],53.3,53.3)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    } else if (match_type_bc_only==9) {
      points_used_x=c(temp_points[,c(1,3)],0,120)
      points_used_y=c(temp_points[,c(2,4)],0,0)
      poly_points=chull(points_used_x,points_used_y)
      poly_area=abs(polyarea(points_used_x[poly_points],points_used_y[poly_points]))
    }
    voronoi_bc_area_in_front_bc_only[frame] = poly_area
  } else {voronoi_bc_area_in_front_bc_only[frame] = 0 }
  
}

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

# Repeat but for bc_only:
bc_only_minmax_x <- apply(X = v_points_bc_only, MARGIN = 3, FUN = goingdeepR::v_bc_minmax)

## Determine whether it's the min or the max based on the target variable
bc_only_range <- matrix(NA, nrow = nrow(design_matrix), ncol = 2)

for (i in 1:nrow(design_matrix)) {
  ## Skip this frame if it's an NA
  if (is.na(design_matrix$target_x[i])) { next }
  
  if (design_matrix$target_x[i] == 0) {
    bc_only_range[i, 1] <- min(bc_only_minmax_x[, i])
    bc_only_range[i, 2] <- max(bc_only_minmax_x[, i])
  } else if (design_matrix$target_x[i] == 120) {
    bc_only_range[i, 1] <- max(bc_only_minmax_x[, i])
    bc_only_range[i, 2] <- min(bc_only_minmax_x[, i])
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

# Okay so will use the far and close columns from the previous code, but add in
# the area columns from Nick's new code:
## Bind the created features to the design matrix and save 
design_matrix$voronoi_bc_close <- bc_range[, 1]
design_matrix$voronoi_bc_far <- bc_range[, 2]
design_matrix$voronoi_bc_only_close <- bc_only_range[, 1]
design_matrix$voronoi_bc_only_far <- bc_only_range[, 2]

design_matrix$voronoi_bc_area <- v_summary[1, 8, ]
design_matrix$voronoi_bc_only_area <- v_summary_bc_only[1, 8, ]

design_matrix$voronoi_bc_area_in_front <- voronoi_bc_area_in_front
design_matrix$voronoi_bc_area_in_front_bc_only <- voronoi_bc_area_in_front_bc_only

design_matrix$voronoi_bc_bubble <- bc_voronoi_offensive_bubble

## Write out the new design matrix w/ the appended features 
voronoi_path <- paste0(base_directory, "/data/initial_bc_design_matrices/rusher_bc_design_matrix_voronoi_1022.csv")
readr::write_csv(x = design_matrix, 
                 path = voronoi_path)



