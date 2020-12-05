# This file contains the functions necessary for identifying the ball carrier
# in a dataset of NFL tracking data in the format from the NFL Big Data Bowl
# joined together with nflscrapR accessed information regarding the different
# roles of players directly involved in the play

#' Identify play frames where player is the ball carrier
#'
#' Given a dataset of tracking data frames for an individual player in a single
#' play, the input dataset is returned along with indicator columns designating
#' when the player is the ball carrier.
#'
#' @param play_player_tracking_data Dataframe of tracking data for an individual
#' player in an individual play containing the necessary columns joined to it
#' with the player information such as their name and position as well as the
#' nflscrapR data about the different roles of players inolved in the play.
#' @return Modified version of the input \code{play_player_tracking_data} with
#' columns indicating if the player is the ball carrier for an individual frame
#' and which of the possible roles the player matches in name.
#' @examples
#' # INSERT EXAMPLES HERE
#' @export

identify_ball_carrier_frames <- function(play_player_tracking_data) {

  # First check that the necessary columns are in the provided data:
  assertthat::assert_that(all(c("play_type",
                                "nflId",
                                "passer_player_tracking_id",
                                "receiver_player_tracking_id",
                                "rusher_player_tracking_id",
                                "interception_player_tracking_id",
                                "punt_returner_player_tracking_id",
                                "kickoff_returner_player_tracking_id",
                                "own_kickoff_recovery_player_tracking_id",
                                "fumble_recovery_1_player_tracking_id",
                                "fumble_recovery_2_player_tracking_id",
                                "home_team", "away_team",
                                "posteam", "defteam",
                                "displayName", "event", "frame.id", "team",
                                "FirstName", "LastName", "PositionAbbr",
                                "qb_scramble", "sack", "qb_hit", "fumble",
                                "interception", "complete_pass",
                                "fumble_out_of_bounds", "touchdown",
                                "yards_after_catch", "pass_touchdown") %in%
                                colnames(play_player_tracking_data)),
                          msg = paste0("The input dataset is missing the following columns: ",
                                       paste(c("play_type",
                                               "nflId",
                                               "passer_player_tracking_id",
                                               "receiver_player_tracking_id",
                                               "rusher_player_tracking_id",
                                               "interception_player_tracking_id",
                                               "punt_returner_player_tracking_id",
                                               "kickoff_returner_player_tracking_id",
                                               "own_kickoff_recovery_player_tracking_id",
                                               "fumble_recovery_1_player_tracking_id",
                                               "fumble_recovery_2_player_tracking_id",
                                               "home_team", "away_team",
                                               "posteam", "defteam",
                                               "displayName", "event", "frame.id",
                                               "team", "FirstName", "LastName", "PositionAbbr",
                                               "qb_scramble", "sack", "qb_hit",
                                               "fumble", "interception", 'complete_pass',
                                               "fumble_out_of_bounds", "touchdown",
                                               "yards_after_catch", "pass_touchdown")[
                                                 which(!(c("play_type",
                                                           "nflId",
                                                           "passer_player_tracking_id",
                                                           "receiver_player_tracking_id",
                                                           "rusher_player_tracking_id",
                                                           "interception_player_tracking_id",
                                                           "punt_returner_player_tracking_id",
                                                           "kickoff_returner_player_tracking_id",
                                                           "own_kickoff_recovery_player_tracking_id",
                                                           "fumble_recovery_1_player_tracking_id",
                                                           "fumble_recovery_2_player_tracking_id",
                                                           "home_team", "away_team",
                                                           "posteam", "defteam",
                                                           "displayName", "event", "frame.id",
                                                           "team", "FirstName", "LastName", "PositionAbbr",
                                                           "qb_scramble", "sack", "qb_hit", "fumble",
                                                           "interception", "complete_pass",
                                                           "fumble_out_of_bounds", "touchdown",
                                                           "yards_after_catch", "pass_touchdown") %in%
                                                        colnames(play_player_tracking_data)))],
                                             collapse = ", "), "!"))

  # Check that there is only one player involved in the provided data:
  assertthat::assert_that(length(unique(play_player_tracking_data$displayName)) == 1,
                          msg = "More than one player in the input data, clean your data!")

  # Now proceed to use the joined information to designate which of the frames
  # the player is the ball carrier.

  # First designate whether the player is on offense or defense based on the
  # posteam, defteam, home_team, and away_team columns:
  is_offense <- ifelse(play_player_tracking_data$posteam[1] == play_player_tracking_data$home_team[1],
                       ifelse(play_player_tracking_data$team[1] == "home",
                              TRUE, FALSE),
                       # Else the possession team is away, so just check if the
                       # player is on the away team:
                       ifelse(play_player_tracking_data$team[1] == "away",
                              TRUE, FALSE))

  # Initialize booleans for each of the possible player roles all to be FALSE
  is_passer <- FALSE
  is_rusher <- FALSE
  is_receiver <- FALSE
  is_ko_returner <- FALSE
  is_punt_returner <- FALSE
  is_own_ko_recover <- FALSE
  is_interceptor <- FALSE

  # The following name matching code is DEPRECATED - the logic below was
  # initially used before but can be safely ignored now based on the table
  # mapping the player IDs together:
  # Create the string of the first and last name in 'T.Stark' format based
  # on the tracking data columns, to then compare to the different player roles:
  # first_letter <- stringr::str_sub(string=play_player_tracking_data$displayName[1], start = 1, end = 1)
  # tracking_player_id <- paste(first_letter, play_player_tracking_data$LastName[1], sep = ".")

  # Instead now grab the player's tracking id to detect which role a player is:
  tracking_player_id <- play_player_tracking_data$nflId[1]

  # Since fumble recovery can occur on any play, and since a player can
  # have that role following any of the above roles - we find these right away:
  is_fumble_rec_1 <- tracking_player_id == play_player_tracking_data$fumble_recovery_1_player_tracking_id[1] &
    !is.na(play_player_tracking_data$fumble_recovery_1_player_tracking_id[1])
  is_fumble_rec_2 <- tracking_player_id == play_player_tracking_data$fumble_recovery_2_player_tracking_id[1] &
    !is.na(play_player_tracking_data$fumble_recovery_2_player_tracking_id[1])
  # NOTE:  For now we will NOT identify the frames which fumble recovery players
  #        have the ball. There are plays with multiple fumble recoveries, one
  #        of which that has 3 fumbles! A single play with 3 fumbles!!! It just
  #        creates a weird setting that makes sense for exclusion - since these
  #        probably won't have normal possession frames in some sense in terms
  #        of the type of space management.

  # Grab the play type:
  play_type <- play_player_tracking_data$play_type[1]

  # If the player is on offense, we proceed to check the following:
  if (is_offense) {

    # Next break it down by play_type:
    if (play_type == "pass") {
      # Check to see if the player is the passer:
      is_passer <- tracking_player_id == play_player_tracking_data$passer_player_tracking_id[1] &
                            !is.na(play_player_tracking_data$passer_player_tracking_id[1])
      # Receiver:
      is_receiver <- tracking_player_id == play_player_tracking_data$receiver_player_tracking_id[1] &
        !is.na(play_player_tracking_data$receiver_player_tracking_id[1])

    } else if (play_type == "run") {
      # Rusher:
      is_rusher <- tracking_player_id == play_player_tracking_data$rusher_player_tracking_id[1] &
        !is.na(play_player_tracking_data$rusher_player_tracking_id[1])

    } else if (play_type == "kickoff") {
      # Returner is offense on kickoffs (opposite is true for punts):
      is_ko_returner <- tracking_player_id == play_player_tracking_data$kickoff_returner_player_tracking_id[1] &
        !is.na(play_player_tracking_data$kickoff_returner_player_tracking_id[1])
    }

  } else {
    # Player is on defense, again go through the different play types:
    # Next break it down by play_type:
    if (play_type == "pass") {
      # Check to see if the player is the interceptor:
      is_interceptor <- tracking_player_id == play_player_tracking_data$interception_player_tracking_id[1] &
        !is.na(play_player_tracking_data$interception_player_tracking_id[1])

    } else if (play_type == "punt") {
      # Returner is defense on punts (opposite is true for kickoffs):
      is_punt_returner <- tracking_player_id == play_player_tracking_data$punt_returner_player_tracking_id[1] &
        !is.na(play_player_tracking_data$punt_returner_player_tracking_id[1])

    } else if (play_type == "kickoff") {
      # Own kickoff recovery occurs from onside kicks - might just exclude these anyway:
      is_own_ko_recover <- tracking_player_id == play_player_tracking_data$own_kickoff_recovery_player_tracking_id[1] &
        !is.na(play_player_tracking_data$own_kickoff_recovery_player_tracking_id[1])
    }
  }

  # Now that the player's role has been determined, can now proceed to create
  # the column denoting when the player has the ball.

  # Create the vector of events with the NA removed:
  clean_events <- ifelse(is.na(play_player_tracking_data$event), "no_event",
                         play_player_tracking_data$event)


  # We're assuming the frames are in the correct order, now proceed to indicate
  # the starting frames for when when the player is carrying the ball based on
  # their role. We initialize it to be NA and only update if the starting events
  # are actually identified:
  start_frame_i <- NA
  # Same with end_frame_i
  end_frame_i <- NA

  # NOTE: Since the NFL tracking data is notorious for missing events that help
  # us identify when a player has the ball, need to always check first that the
  # event is in the vector of events to then use:
  if (is_passer) {
    # QBs have the ball starting with the ball_snap event, but only update if
    # the event is there:
    if (any(clean_events == "ball_snap")) {
      start_frame_i <- which(clean_events == "ball_snap")[1]
    }

    # Receivers only have the ball if it is a catch (for now ignore the logic
    # necessary for air TDs, can filter out those plays later since touchdown
    # will always be a separate frame)
  } else if (is_receiver & play_player_tracking_data$complete_pass[1] == 1) {

    # Receivers have the ball starting with the pass_outcome_caught or pass_arrived event:
    if (any(clean_events %in% c("pass_outcome_caught", "pass_arrived"))) {

      # If pass_outcome_caught is there, use that - otherwise use pass_arrived:
      if (any(clean_events == "pass_outcome_caught")) {
        start_frame_i <- which(clean_events == "pass_outcome_caught")[1]
      } else {
        start_frame_i <- which(clean_events == "pass_arrived")[1]
      }
    }

  } else if (is_rusher) {
      # If the player's position is QB then the start index is the ball_snap or
      # snap_direct apparently - I hate the NFL's coding system for events...
      if (play_player_tracking_data$PositionAbbr[1] == "QB") {
        if (any(clean_events %in% c("ball_snap", "snap_direct"))) {
          start_frame_i <- start_frame_i <- which(clean_events %in% c("ball_snap", "snap_direct"))[1]
        }
      } else {
        # Otherwise use either the handoff, snap_direct, lateral
        if (any(clean_events %in% c("snap_direct",
                                    "lateral", "handoff"))) {
          start_frame_i <- which(clean_events %in% c("snap_direct",
                                                     "lateral", "handoff"))[1]
        }
      }
  } else if (is_interceptor) {
    # Interceptors have the ball starting with the pass_outcome_interception event:
    if (any(clean_events %in% c("pass_outcome_interception",
                                                   "pass_arrived"))) {
      start_frame_i <- which(clean_events %in% c("pass_outcome_interception",
                                                 "pass_arrived"))[1]
    }

    # Kickoff returns and punts are simple, just when they are received (this
    # will avoid cases where the play description differs from the tracking data):
  } else if (is_ko_returner &
             any(clean_events == "kick_received")) {
    start_frame_i <- which(clean_events == "kick_received")[1]
  } else if (is_punt_returner &
             any(clean_events == "punt_received")) {
    start_frame_i <- which(clean_events == "punt_received")[1]
  }

  # Now determine the end frame - only the passer role is unique with regards
  # to the possible ending frame:
  if (is_passer) {
    # End of their turn as ball carrier occurs if they throw the ball or if
    # no pass was thrown and they are sacked:
    if (any(clean_events %in% c("pass_forward", "pass_shovel",
                                "lateral", "handoff"))) {
      end_frame_i <- which(clean_events %in% c("pass_forward", "pass_shovel",
                                               "lateral", "handoff"))[1]
    } else {
      # No longer the ball carrier if sacked or strip sacked, tackled, run out of bounds, scored
      # a touchdown or fumbled - take the first one to account for additional
      # fumbles that take place:
      if (any(clean_events %in% c("qb_sack", "qb_strip_sack"))) {
        end_frame_i <- which(clean_events %in% c("qb_sack", "qb_strip_sack"))[1]
      }
    }
    # Otherwise if they are any of the other roles then can use the standard
    # ending frame designation:
  } else if ((is_receiver & play_player_tracking_data$complete_pass[1] == 1) |
             is_rusher | is_interceptor |
             (is_ko_returner & any(clean_events == "kick_received")) |
             (is_punt_returner & any(clean_events == "punt_received"))) {
    # End of their turn as ball carrier occurs if they are tackled, go
    # out_of_bounds, fumble, or score a touchdown. Just use the first one to
    # properly handle fumbles:
    if (any(clean_events %in% c("tackle", "out_of_bounds",
                                "fumble", "touchdown"))) {
      end_frame_i <- which(clean_events %in% c("tackle", "out_of_bounds",
                                               "fumble", "touchdown"))[1]
    }
  }

  # Add the columns to the input data containing the ball carrier indicator
  # and their roles:
  play_player_tracking_data$is_ball_carrier <- rep(0, nrow(play_player_tracking_data))
  # Only update if BOTH the start AND end frame indices are not missing:
  if (!is.na(start_frame_i) & !is.na(end_frame_i)) {
    play_player_tracking_data$is_ball_carrier[start_frame_i:end_frame_i] <- 1
  }
  # Can do the same for the fumble recovery indices as well later on, as well
  # as the own kickoff recovery (going to ignore those for now as well).

  # Columns for the roles:
  play_player_tracking_data$is_passer <- as.numeric(is_passer)
  play_player_tracking_data$is_rusher <- as.numeric(is_rusher)
  play_player_tracking_data$is_receiver <- as.numeric(is_receiver)
  play_player_tracking_data$is_interceptor <- as.numeric(is_interceptor)
  play_player_tracking_data$is_ko_returner <- as.numeric(is_ko_returner)
  play_player_tracking_data$is_punt_returner <- as.numeric(is_punt_returner)
  play_player_tracking_data$is_own_ko_recover <- as.numeric(is_own_ko_recover)
  play_player_tracking_data$is_fumble_rec_1 <- as.numeric(is_fumble_rec_1)
  play_player_tracking_data$is_fumble_rec_2 <- as.numeric(is_fumble_rec_2)

  # Return the modified input:
  return(play_player_tracking_data)
}
