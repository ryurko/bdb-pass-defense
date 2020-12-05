# Author: Ron Yurko
# Purpose: Identify the ball-carrier during plays

# The simplest way to do this will be to use the NFL API's information
# about the player's involved a play. This is the same as grabbing the names
# from the play description. Since the ball carrier will be identified based
# on matching the player's name in the format of 'T.Stark', the first step
# is to set-up a dataset that contains for each play in the data columns
# containing the passer, receiver, rusher, and/or returners. These columns
# can then be joined to the tracking frame level data in order to create
# indicators for if a player matches one of the potential ball carriers.
# Then the event column in the tracking data will be used to indicate which
# of the possible ball carrier types is currently carrying the ball.

# ------------------------------------------------------------------------------

# Install data.table
# install.packages("data.table")
# Access data.table functions:
library(data.table)

# Install magrittr
# install.packages("magrittr")
library(magrittr)

# Install readr
# install.packages("readr")
library(readr)

# Now assuming the current directory contains the big_data_bowl_repo folder,
# load the play level data:
plays_data <- fread("big_data_bowl_repo/Data/plays.csv")
# Use the following if not using a the R Project in the current directory:
#plays_data <- fread("../../big_data_bowl_repo/Data/plays.csv")

# Now get the 2017 nflscrapR play-by-play data to join:
nflscrapr_pbp_17_data_url <- "https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv"
nflscrapr_pbp_17 <- readr::read_csv(nflscrapr_pbp_17_data_url) %>%
  as.data.table()

# Rename the play id and game id columns for the nflscrapR data, then create
# the keys used to join the play and nflscrapR data together:
setnames(nflscrapr_pbp_17, c("game_id", "play_id"), c("gameId", "playId"))
setkeyv(nflscrapr_pbp_17, c("gameId", "playId"))
setkeyv(plays_data, c("gameId", "playId"))

# Join the plays_data with the nflscrapR data, using an inner join since we are
# requiring that nflscrapR data is available
model_plays_data <- plays_data %>%
  .[nflscrapr_pbp_17, nomatch = 0] %>%
  # Only want the pass, run, kickoffs, or punts for the ballcarrier model, and
  # we are ignoring the penalties denoted by no_plays:
  .[play_type %in% c("kickoff", "pass", "punt", "run"),] %>%
  # Only want kickoffs or punts that had returns:
  .[touchback == 0 & kickoff_fair_catch == 0 & punt_fair_catch == 0,]

# Now only need the columns for joining with the tracking data (gameId and
# playId) along with the columns containing the player IDs and names for individuals
# that could be holding the ball and the play type. Additionally, to simplify
# the joining process later with the types of events for the ball carrier,
# add the team information so it is known whether or not the player is on
# offense or defense:
plays_player_data <- model_plays_data %>%
  .[, list(gameId, playId, play_type, 
           passer_player_name, passer_player_id,
           receiver_player_name, receiver_player_id,
           rusher_player_name, rusher_player_id,
           interception_player_name,  interception_player_id,
           punt_returner_player_name, punt_returner_player_id,
           kickoff_returner_player_name, kickoff_returner_player_id,
           own_kickoff_recovery_player_name, own_kickoff_recovery_player_id,
           fumble_recovery_1_player_name, fumble_recovery_1_player_id,
           fumble_recovery_2_player_name, fumble_recovery_2_player_id,
           # Get the team information regarding the play:
           home_team, away_team, posteam, defteam,
           # Grab additional context about the play that is useful for
           # identifying events for the possible player roles that will be used
           # for identifying when a player has the ball:
           qb_scramble, sack, qb_hit, fumble, interception, complete_pass,
           fumble_out_of_bounds, touchdown, yards_after_catch, pass_touchdown)]
# NOTE: the player name columns are manually selected above since these are
# of specific interest for individuals that can be carrying the football.
# View(nflscrapr_pbp_17[which(!is.na(nflscrapr_pbp_17$fumble_recovery_1_player_id) & !is.na(nflscrapr_pbp_17$fumble_recovery_2_player_id))])
# Ok there is actually a play with 3 fumbles on a single play!!! That's a pain
# in the ass to work with - so for now, I will only focus on identifying who
# is carrying the football until the first fumble. It might be more appropriate
# to isolate fumble recoveries completely - but for now will ignore the
# fumble recovery player's possession times.

# Since we are only working with the first six weeks of the 2017 season, we
# check to see if any of the columns containing player names for people that
# could be potentially holding the football are all NA. For instance there
# are only two fumble recovery player columns in case two fumble recoveries
# occur on a single play (pretty unlikely).

colnames(plays_player_data)[which(apply(plays_player_data, 2,
                                        function(x) all(is.na(x))))]
# character(0)
# Well as it turns out there are indeed some plays with multiple fumble
# recovery players, so all of the columns will be kept.

# We can proceed to save this dataset:
readr::write_csv(plays_player_data,
                 "data/plays_with_player_names_data.csv")
                 #"../../data/plays_with_player_names_data.csv")

# Now proceed to join the NFL tracking data to the different player roles
# identified from the nflscrapR data. Load the nfl_player_id_table file:
nfl_player_id_table <- fread("data/players_gsis.csv")

# Use dplyr join functions to make this simpler:
# install.packages("dplyr")
library(dplyr)

# For each of the different player roles, left join to the plays_player_data
# the NFL tracking data ID based by joining on the GSISID that is in the 
# nflscrapR data:
plays_player_data <- plays_player_data %>%
  # First the passer:
  left_join(# Modify the data to only join the tracking id and name it:
    {(
      nfl_player_id_table %>%
        dplyr::select(nflId, GSISID) %>%
        rename(passer_player_tracking_id = nflId)
      )},
    by = c("passer_player_id" = "GSISID")) %>%
  # Receiver:
  left_join(
    {(
      nfl_player_id_table %>%
        dplyr::select(nflId, GSISID) %>%
        rename(receiver_player_tracking_id = nflId)
    )},
    by = c("receiver_player_id" = "GSISID")) %>%
  # Rusher
  left_join(
    {(
      nfl_player_id_table %>%
        dplyr::select(nflId, GSISID) %>%
        rename(rusher_player_tracking_id = nflId)
    )},
    by = c("rusher_player_id" = "GSISID")) %>%
  # Interception
  left_join(
    {(
      nfl_player_id_table %>%
        dplyr::select(nflId, GSISID) %>%
        rename(interception_player_tracking_id = nflId)
    )},
    by = c("interception_player_id" = "GSISID")) %>%
  # Punt returner:
  left_join(
    {(
      nfl_player_id_table %>%
        dplyr::select(nflId, GSISID) %>%
        rename(punt_returner_player_tracking_id = nflId)
    )},
    by = c("punt_returner_player_id" = "GSISID")) %>%
  # Kickoff returner:
  left_join(
    {(
      nfl_player_id_table %>%
        dplyr::select(nflId, GSISID) %>%
        rename(kickoff_returner_player_tracking_id = nflId)
    )},
    by = c("kickoff_returner_player_id" = "GSISID")) %>%
  # Own kickoff recovery:
  left_join(
    {(
      nfl_player_id_table %>%
        dplyr::select(nflId, GSISID) %>%
        rename(own_kickoff_recovery_player_tracking_id = nflId)
    )},
    by = c("own_kickoff_recovery_player_id" = "GSISID")) %>%
  # Fumble recovery players
  left_join(
    {(
      nfl_player_id_table %>%
        dplyr::select(nflId, GSISID) %>%
        rename(fumble_recovery_1_player_tracking_id = nflId)
    )},
    by = c("fumble_recovery_1_player_id" = "GSISID")) %>%
  left_join(
    {(
      nfl_player_id_table %>%
        dplyr::select(nflId, GSISID) %>%
        rename(fumble_recovery_2_player_tracking_id = nflId)
    )},
    by = c("fumble_recovery_2_player_id" = "GSISID"))

# Can check that the new columns line up in terms of missing values using
# table("tracking" = as.numeric(is.na(plays_player_data$passer_player_tracking_id)),
#       "gsisid" = as.numeric(is.na(plays_player_data$passer_player_id)))
# Did this for each of the roles and saw that they lined up.

# Save the updates:
readr::write_csv(plays_player_data,
                 "data/plays_with_player_names_data.csv")
  
# Load the data back:
plays_player_data <- fread("data/plays_with_player_names_data.csv")

# Set the keys for joining this data to the tracking data:
setkeyv(plays_player_data, c("gameId", "playId"))

# Next thing is to list all of the different events available in all of the
# tracking data for each of the types of plays. This will go through each
# file of tracking data playIds in the plays_player_data, join the data above,
# and count of the different events (only using the frames for the football).

# Get the list of tracking data files:
tracking_data_files <- list.files("big_data_bowl_repo/Data",
                                  # ../../big_data_bowl_repo/Data",
                                  pattern = "tracking",
                                  full.names = TRUE)

# Go through each game to create a dataset of the events in each play type:
play_type_events <- lapply(tracking_data_files,
                           function(game_file_name) {

                             # Load the file for the game's tracking data:
                             game_tracking_data <- fread(game_file_name)

                             # For each play in this data, find the number of
                             # players with data (number of unique ids - 1 for
                             # football) for then dividing the event counts by:
                             play_involved_counts <- game_tracking_data %>%
                               .[displayName != "football",
                                 .(n_players = length(unique(nflId))),
                                 by = .(playId)]

                             # Set the keys for joining:
                             setkeyv(play_involved_counts, "playId")
                             setkeyv(game_tracking_data, c("gameId", "playId"))


                             # Inner join the data with the plays_player_data
                             # file created above since we only want to include
                             # plays that have data in both:
                             game_tracking_data %>%
                               .[plays_player_data, nomatch = 0] %>%
                               # Group by the play_type and event, and count
                               # events (removing the football rows since those
                               # do not include the events)
                               .[displayName != "football",
                                 .N, by = .(playId, play_type, event)] %>%
                               setkeyv("playId") %>%
                               # Join the number of players above:
                               .[play_involved_counts, nomatch = 0] %>%
                               # Divide the counts by the number of players:
                               .[, .(n_event = round(N / n_players), playId,
                                     play_type, event)] %>%
                               # Now total up the events across the plays:
                               .[, .(n_game_events = sum(n_event)),
                                 by = .(play_type, event)] %>%
                               return()
                           }) %>%
  # Join the files together:
  rbindlist() %>%
  # Now aggregate over all games:
  .[, .(n_events_total = sum(n_game_events)), by = .(play_type, event)]

# We can view this table:
View(play_type_events[order(play_type, -n_events_total)])

# This gives us the necessary information we need for understanding which
# events signal when to denote when an individual is now the ball carrier for
# certain types of plays.

# For play_type == "kickoff":
#   ---------------------------------------------------
#   starting events : corresponding player name columns
#   ---------------------------------------------------
#   kick_received :  kickoff_returner_player_name
#   fumble_defense_recovered / fumble_offense_recovered : fumble_recovery_1_player_name or fumble_recovery_2_player_name
#   kick_recovered : own_kickoff_recovery_player_name ?
#   punt_received : NEED TO INVESTIGATE THIS FURTHER
#   ---------------------------------------------------
#   ending events : tackle, out_of_bounds, fumble, touchdown
#   ---------------------------------------------------

# For play_type == "pass":
#   ---------------------------------------------------
#   starting events : corresponding player name columns
#   ---------------------------------------------------
#   ball_snap :  passer_player_name - unless its a trick play
#   pass_outcome_caught : receiver_player_name
#   pass_outcome_interception : interception_player_name
#   fumble_defense_recovered / fumble_offense_recovered : fumble_recovery_1_player_name or fumble_recovery_2_player_name
#   run : NEED TO INVESTIGATE THIS FURTHER
#   ---------------------------------------------------
#   ending events : corresponding player name columns
#   ---------------------------------------------------
#   pass_forward, qb_sack, qb_strip_sack, pass_shovel, lateral, handoff? : passer_player_name
#   tackle, out_of_bounds, fumble, touchdown : receiver_player_name or interception_player_name

# For play_type == "punt":
#   ---------------------------------------------------
#   starting events : corresponding player name columns
#   ---------------------------------------------------
#   punt_received :  punt_returner_player_name
#   fumble_defense_recovered / fumble_offense_recovered : fumble_recovery_1_player_name or fumble_recovery_2_player_name
#   ---------------------------------------------------
#   ending events : tackle, out_of_bounds, fumble, touchdown, two_point_conversion
#   ---------------------------------------------------

# For play_type == "run":
#   ---------------------------------------------------
#   starting events : corresponding player name columns
#   ---------------------------------------------------
#   handoff :  rusher_player_name
#   snap_direct : rusher_player_name
#   fumble_defense_recovered / fumble_offense_recovered : fumble_recovery_1_player_name or fumble_recovery_2_player_name
#   ending events : tackle, out_of_bounds, fumble, touchdown, safety, two_point_conversion

# Access the goingdeepR functions:
#devtools::load_all(".")
devtools::load_all("src/goingdeepR")

# Load the players data to join:
#players_data <- fread("../../big_data_bowl_repo/Data/players.csv")
players_data <- fread("big_data_bowl_repo/Data/players.csv")
setkey(players_data, "nflId")

# Now need to proceed to apply this function to every single set of player-plays
# in the tracking data. For ease, will save the joined data in individual game
# files separately just like the data is provided.

# Go through each game to create a dataset of the events in each play type:
# (will use the purrr walk function to not return anything):
identify_ball_carrier_data <- purrr::walk(tracking_data_files,
                                          function(game_file_name) {
                                            # Load the file for the game's tracking data:
                                            game_tracking_data <- fread(game_file_name) %>%
                                              # Remove the rows for the football:
                                              .[displayName != "football"]
                                            # Set the keys for joining
                                            setkeyv(game_tracking_data, c("gameId", "playId", "nflId"))
                                            # Join the nflscrapR plays-player data
                                            # and the player data. This join will
                                            # remove the plays we are not interested
                                            # in looking at for the ball carrier
                                            # model:
                                            game_tracking_data <- game_tracking_data %>%
                                              .[plays_player_data, nomatch = 0] %>%
                                              merge(., players_data, all.x = TRUE)

                                            # Now go through each play and player
                                            # identifying which frames they are
                                            # the ball carrier, joining together
                                            # into a final dataset:
                                            game_tracking_data_bcid <- lapply(unique(game_tracking_data$playId),
                                                                              function(play_id) {
                                                                                play_data <- game_tracking_data[playId == play_id,]
                                                                                # For each player:
                                                                                lapply(unique(play_data$nflId),
                                                                                       function(player_id) {
                                                                                         goingdeepR::identify_ball_carrier_frames(
                                                                                           play_data[nflId == player_id,]
                                                                                         )
                                                                                       }) %>%
                                                                                  rbindlist()
                                                                              }) %>%
                                              rbindlist()
                                            # Now save the data with the game id:
                                            readr::write_csv(game_tracking_data_bcid,
                                                             paste0("data/ball_carrier_tracking_data/bc_tracking_data_game_",
                                                                    #"../../data/ball_carrier_tracking_data/bc_tracking_data_game_",
                                                                    game_tracking_data$gameId[1],
                                                                    ".csv"))
                                          })
