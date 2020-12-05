#' Get the closest players to the ball
#'
#' @param frame_df
#'
get_closest_players <- function(frame_df) {
  ## Get the index of the frame_df which includes the ball-carrier
  bc_ind <- frame_df$is_ball_carrier == 1

  ## If there's more than one ball carrier, then skip for now.
  if ( sum(bc_ind) != 1 ) {
    print("There is not one unique ball-carrier!")
    return( NULL )
  }

  ## Determine the offensive and defensive teams
  offense_home_away <- as.character(frame_df[bc_ind, "team"])
  frame_df$offense_defense <- ifelse(frame_df$team == offense_home_away, "offense", "defense")

  ## Set the offensive and defensive columns
  euclid_distances <- as.matrix(dist(x=frame_df[, c("x", "y")]))
  dist_to_ball <- euclid_distances[bc_ind, ]
  player_dists <- cbind(frame_df[, c("x", "y", "s", "dis", "dir", "offense_defense")], dist_to_ball)

  ## Remove the index of the ball-carrier
  player_dists <- player_dists[!(bc_ind),]

  ## Rank the defensive players
  player_dists_defense <- player_dists[player_dists$offense_defense == "defense", ]
  player_dists_defense <- player_dists_defense[order(player_dists_defense$dist_to_ball),]

  if (nrow(player_dists_defense) < 11) {
    print("Less than 11 on defense!")
    player_dists_defense <- goingdeepR::add_na_rows(df=player_dists_defense, rows=1)
  } else if (nrow(player_dists_defense) > 11) {
    print("More than 11 on defense!")
    player_dists_defense <- player_dists_defense[1:11, ]
  }

  player_dists_defense$rank <- 1:11

  ## Rank the offensive players
  player_dists_offense <- player_dists[player_dists$offense_defense == "offense", ]
  player_dists_offense <- player_dists_offense[order(player_dists_offense$dist_to_ball),]

  if (nrow(player_dists_offense) < 10) {
    print("Less than 10 on offense!")
    player_dists_offense <- goingdeepR::add_na_rows(df=player_dists_offense, rows=1)
  } else if (nrow(player_dists_offense) > 10) {
    print("More than 10 on offense!")
    player_dists_offense <- player_dists_offense[1:10, ]
  }

  player_dists_offense$rank <- 1:10

  ## Return the coordinates of players ranked by distance
  coordinates <- c(player_dists_offense$x, player_dists_offense$y, player_dists_offense$s, player_dists_offense$dis,
                   player_dists_offense$dir, player_dists_offense$dist_to_ball, player_dists_defense$x, player_dists_defense$y,
                   player_dists_defense$s, player_dists_defense$dis, player_dists_defense$dir, player_dists_defense$dist_to_ball)

  return( coordinates )
}

#' Get orientation for each team
#'
#' @param game_df
#' @param play_meta
#'
get_team_orientation <- function(game_df, play_meta, game_meta_row) {
  home_team <- game_meta_row$homeTeamAbbr
  away_team <- game_meta_row$visitorTeamAbbr
  game_plays <- play_meta[play_meta$gameId == game_meta_row$gameId, ]
  quarter_orientation <- data.frame(matrix(data=NA, nrow=4, ncol=2))
  names(quarter_orientation) <- c(home_team, away_team)

  ## Get the frame of the ball when the kickoff occurs
  kickoff_id <- unique(game_df$playId)[1]
  kickoff_meta <- game_plays[game_plays$playId == kickoff_id, ]
  stopifnot( as.numeric(kickoff_meta$quarter) == 1)

  kickoff_team <- as.character(kickoff_meta[, "possessionTeam"])
  kickoff_df <- game_df[game_df$playId == kickoff_id, ]
  kickoff_frame <- as.numeric(kickoff_df[which(kickoff_df$event == "kickoff" | kickoff_df$event == "onside_kick"), "frame.id"][1,1])
  kickoff_event_df <- kickoff_df[kickoff_df$frame.id == kickoff_frame,]
  kickoff_ball_df <- kickoff_event_df[kickoff_event_df$displayName == "football", ]

  kickoff_yardline <- as.numeric(kickoff_ball_df[, "x"])
  out_of_range <- (kickoff_yardline < 43 | (kickoff_yardline > 47 && kickoff_yardline < 72) | kickoff_yardline > 77)

  ## Handle the case where the ball has missing coordinates or the ball is out of range
  if ( is.na(kickoff_yardline) | out_of_range ) {
    warning("The ball camera has missing values on this kickoff!")
    if (kickoff_team == home_team) {
      kickoff_ball_df <- kickoff_event_df[which(kickoff_event_df$team == "home")[1], ]
      kickoff_yardline <- as.numeric(kickoff_ball_df[, "x"])
    } else if (kickoff_team == away_team) {
      kickoff_ball_df <- kickoff_event_df[which(kickoff_event_df$team == "away")[1], ]
      kickoff_yardline <- as.numeric(kickoff_ball_df[, "x"])
    }
  }

  ## Determine the orientation of the kickoff and recieving teams
  start_0 <-  c("to_0", "to_120", "to_0", "to_120")
  start_120 <- c("to_120", "to_0", "to_120", "to_0")

  if (kickoff_yardline < 60) {
    kickoff_team_orientation <- start_120
    receiving_team_orientation <- start_0
  } else {
    kickoff_team_orientation <- start_0
    receiving_team_orientation <- start_120
  }

  quarter_orientation[, kickoff_team == names(quarter_orientation)] <- kickoff_team_orientation
  quarter_orientation[, !(kickoff_team == names(quarter_orientation))] <- receiving_team_orientation
  print(kickoff_team); print(kickoff_yardline); print(quarter_orientation); #browser()
  return( quarter_orientation )
}

#' Get orientation of the ball-carrier
#'
#' @param home_away
#' @param quarter
#' @param team_orientation
#'
get_bc_orientation <- function(home_away, quarter, team_orientation) {
  if (home_away == "home") {
    return ( team_orientation[quarter, 1] )
  } else if (home_away == "away") {
    return ( team_orientation[quarter, 2] )
  } else {
    stop("home_or_away must be 'home' or 'away'")
  }
}

#' Utility function to add NA rows
#'
#' @param df data.frame to append
#' @param rows number of rows
#'
add_na_rows <- function(df, rows=1) {
  na_rows <- data.frame(matrix(data=NA, nrow=rows, ncol=ncol(df)))
  names(na_rows) <- names(df)

  return( dplyr::bind_rows(df, na_rows) )
}

#' Append data-frames
#'
#' @param df
#' @param full_df
#' @param iter
#' @param iter_list
#'
append_df <- function(df, full_df, iter, iter_list) {
  if (iter == iter_list[1]) {
    return_df <- dplyr::mutate_all(df, as.character)
  } else {
    return_df <- dplyr::bind_rows(full_df, dplyr::mutate_all(df, as.character))
  }

  return( return_df )
}
