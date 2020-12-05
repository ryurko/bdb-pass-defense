#' Get the rows w/ edges in the Voronoi tesselation
#'
#' @param voronoi object of class 'delldir'
#' @param v_point_rows array of row indices to match
#' @param missing_inds logical vector of missing indices
#'
v_filter <- function(voronoi, v_point_rows, missing_inds) {
  v_dirsgs <- voronoi$dirsgs
  
  ## Update missing indices if there are any
  if (all(missing_inds) == FALSE) {
    missing_ind <- which(missing_inds == FALSE)
    v_dirsgs$ind1[v_dirsgs$ind1 > missing_ind] <- v_dirsgs$ind1[v_dirsgs$ind1 > missing_ind] + 1
    v_dirsgs$ind2[v_dirsgs$ind2 > missing_ind] <- v_dirsgs$ind2[v_dirsgs$ind2 > missing_ind] + 1
  }
  
  ## Make sure ind1 is the lower index and ind2 is the higher index
  min_index <- apply(v_dirsgs[, 5:6], 1, min)
  max_index <- apply(v_dirsgs[, 5:6], 1, max)
  v_dirsgs$ind1 <- min_index
  v_dirsgs$ind2 <- max_index
  
  ## Matches the indices which have a vertex with corresponding row in the v_points matrix
  v_rows <-  prodlim::row.match(x=v_dirsgs[, 5:6], v_point_rows)
  return( v_rows )
}

#' Minimum and maximum x-coords among all connections to player 1
#'
#' @param v_points
#'
v_bc_minmax <- function(v_points) {
  min_coord <- min(v_points[1:21, c(3, 5)], na.rm=TRUE)
  max_coord <- max(v_points[1:21, c(3, 5)], na.rm=TRUE)

  return( c(min_coord, max_coord) )
}
