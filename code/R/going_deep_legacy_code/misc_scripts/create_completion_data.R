# PURPOSE: Extract rows of pass locations just for completions

library(data.table)
library(tidyverse)

# Load the play data:
plays_data <- fread("big_data_bowl_repo/Data/plays.csv")

# Only look at rows which are completions:
completions_data <- plays_data[PassResult == "C",]

# Get the list of tracking data files:
tracking_data_files <- list.files("big_data_bowl_repo/Data",
                                  # ../../big_data_bowl_repo/Data",
                                  pattern = "tracking",
                                  full.names = TRUE)

# Go through each game to create a dataset of the events in each play type:
comp_pass_coordinates <- map_dfr(tracking_data_files,
                           function(game_file_name) {
                             game_id <- str_remove(game_file_name,
                                                   "big_data_bowl_repo/Data/tracking_gameId_") %>%
                               str_remove("\\.csv")
                             
                             game_comp_plays <- completions_data[gameId == game_id,]
                             # Only look at these plays:
                             game_comp_data <- fread(game_file_name) %>%
                               .[playId %in% game_comp_plays$playId,]
                             
                             # Which frames are when the pass is caught:
                             pass_caught_frames <- game_comp_data %>%
                               .[event %in% c("pass_outcome_caught", 
                                              "pass_outcome_touchdown",
                                              "pass_arrived"),] %>%
                               as.data.frame() %>%
                               dplyr::select(gameId, playId, frame.id, event) %>%
                               group_by(gameId, playId) %>%
                               arrange(frame.id) %>%
                               slice(1) %>%
                               unite("play_frame", playId, frame.id, remove = FALSE)
                             
                             # Which frames mark the snap?
                             pass_snap_frames <- game_comp_data %>%
                               .[playId %in% pass_caught_frames$playId &
                                   event %in% c("ball_snap"),] %>%
                               as.data.frame() %>%
                               dplyr::select(gameId, playId, frame.id, event) %>%
                               group_by(gameId, playId) %>%
                               arrange(frame.id) %>%
                               slice(1) %>%
                               unite("play_frame", playId, frame.id, remove = FALSE)
                             
                             pass_end_frames <- game_comp_data %>%
                               .[playId %in% pass_caught_frames$playId,] %>%
                               as.data.frame() %>%
                               dplyr::select(gameId, playId, frame.id, event) %>%
                               group_by(gameId, playId) %>%
                               summarize(last_frame = max(frame.id)) %>%
                               unite("play_frame", playId, last_frame, remove = FALSE) %>%
                               ungroup()

                             # Now get the ball coordinates for each of these 
                             pass_caught_data <- game_comp_data %>%
                               .[displayName == "football",] %>%
                               as.data.frame() %>%
                               dplyr::select(gameId, playId, frame.id, x, y) %>%
                               unite("play_frame", playId, frame.id, remove = FALSE) %>%
                               filter(play_frame %in% pass_caught_frames$play_frame) %>%
                               dplyr::select(-play_frame)
                             
                             pass_snap_data <- game_comp_data %>%
                               .[displayName == "football",] %>%
                               as.data.frame() %>%
                               dplyr::select(gameId, playId, frame.id, x, y) %>%
                               unite("play_frame", playId, frame.id, remove = FALSE) %>%
                               filter(play_frame %in% pass_snap_frames$play_frame) %>%
                               dplyr::select(-play_frame) %>%
                               rename(snap_x = x, snap_y = y,
                                      snap_frame = frame.id) 
                             
                             pass_end_data <- game_comp_data %>%
                               .[displayName == "football",] %>%
                               as.data.frame() %>%
                               dplyr::select(gameId, playId, frame.id, x, y) %>%
                               unite("play_frame", playId, frame.id, remove = FALSE) %>%
                               filter(play_frame %in% pass_end_frames$play_frame) %>%
                               dplyr::select(-play_frame) %>%
                               rename(end_x = x, end_y = y,
                                      end_frame = frame.id) 
                             
                             # Join and return:
                             inner_join(pass_caught_data, pass_snap_data,
                                        by = c("gameId", "playId")) %>%
                               inner_join(pass_end_data, 
                                          by = c("gameId", "playId"))
                           }) 

# Access all completions from the 2017 play-by-play data:
nflscrapr_pbp_17_data_url <- "https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv"
pbp_play_info <- readr::read_csv(nflscrapr_pbp_17_data_url) %>%
  dplyr::select(game_id, play_id, posteam, defteam, home_team, away_team,
                ep, wp, epa, wpa, yardline_100, yards_gained, air_yards, yards_after_catch,
                passer_player_id, passer_player_name, 
                receiver_player_id, receiver_player_name) %>%
  unite("game_play", game_id, play_id, remove = FALSE)

completions_data <- completions_data %>%
  as.data.frame() %>%
  unite("game_play", gameId, playId, remove = FALSE) %>%
  inner_join(pbp_play_info, by = "game_play")

# Now with coordinates:
comp_coordinate_data <- completions_data %>%
  inner_join(comp_pass_coordinates,
             by = c("gameId", "playId"))

comp_coordinate_data <- comp_coordinate_data %>%
  mutate(target_x = case_when(
  (yards_gained > 0 & end_x > snap_x) ~ 120,
  (yards_gained > 0 & end_x < snap_x) ~ 0,
  (yards_gained < 0 & end_x > snap_x) ~ 0,
  (yards_gained < 0 & end_x < snap_x) ~ 120,
  (yards_gained == 0) ~ 60,
  TRUE ~ NA_real_))

comp_coordinate_data <- comp_coordinate_data %>%
  mutate(target_x = case_when(
    target_x == 60 & (air_yards > 0 & x > snap_x) ~ 120,
    target_x == 60 & (air_yards > 0 & x < snap_x) ~ 0,
    target_x == 60 & (air_yards < 0 & x > snap_x) ~ 0,
    target_x == 60 & (air_yards < 0 & x < snap_x) ~ 120,
    target_x == 60 & (air_yards == 0) ~ 60,
    TRUE ~ target_x))

# Save this dataset:
write_csv(comp_coordinate_data,
          "data/completion_coordinates.csv")


