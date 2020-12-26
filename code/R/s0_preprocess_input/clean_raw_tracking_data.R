# PURPOSE: Pre-process raw tracking data csv files

#          This script mostly follows the Kaggle tutorial: https://www.kaggle.com/tombliss/tutorial

# Access necessary packages -----------------------------------------------

library(tidyverse)

# Load non-tracking input -------------------------------------------------

games_data <- read_csv("data/input/games.csv")
plays_data <- read_csv("data/input/plays.csv")
players_data <- read_csv("data/input/players.csv")
targeted_receiver_data <- read_csv("data/input/bonus_files/targetedReceiver.csv")

# Load weekly raw tracking data and stack ---------------------------------

# NOTE: This will be fairly large - may be more appropriate to use data.table

# For ease, will flip the coordinates for the x and y for each week file
# separately. This aligns the coordinates so that they are in relation to the
# target endzone. (This was a lot more difficult back when they did NOT provide
# a playDirection indicator variable...)
tracking_data <-
  map_dfr(list.files("data/input/weekly_csv_raw_tracking", full.names = TRUE),
          function(file_name) {
            read_csv(file_name) %>%
              # First rename the original x,y
              dplyr::rename(old_x = x, old_y = y) %>%
              # Now create the coordinates with respect to the target endzone
              mutate(x = ifelse(playDirection == "left", 120 - old_x, old_x),
                     y = ifelse(playDirection == "left", 160 / 3 - old_y, old_y))
            })


# Join the play and game info ---------------------------------------------

tracking_data <- tracking_data %>%
  left_join(games_data, by = "gameId") %>%
  left_join(plays_data, by = c("gameId", "playId")) %>%
  left_join(targeted_receiver_data, by = c("gameId", "playId"))


# Only keep plays where the ball was thrown -------------------------------

# What are the types of plays in the dataset?
table(plays_data$passResult)
#     C     I    IN     R     S
# 11370  6135   420     4  1308

# Can drop plays with R (runs) and S (sacks)

# Make dataset to join over for filtering the tracking data:
pass_play_ids <- plays_data %>%
  filter(passResult %in% c("C", "I", "IN")) %>%
  dplyr::select(gameId, playId) %>%
  mutate(is_pass_thrown = 1)

# Only keep the rows in tracking data corresponding to these plays:
tracking_data <- tracking_data %>%
 left_join(pass_play_ids, by = c("gameId", "playId")) %>%
  filter(!is.na(is_pass_thrown)) %>%
  # drop this column:
  dplyr::select(-is_pass_thrown)

# Add info on players in tracking data ------------------------------------

tracking_data <- tracking_data %>%
  # Make an indicator if the player is the targeted receiver:
  mutate(is_target = as.numeric(nflId == targetNflId),
         # Side of ball the player is on:
         side_of_ball = ifelse(((team == "home") & (possessionTeam == homeTeamAbbr)) |
                                 ((team == "away") & (possessionTeam == visitorTeamAbbr)),
                               "offense", "defense"),
         # Fix side of ball for football:
         side_of_ball = ifelse(displayName == "Football", "football", side_of_ball))

# Quick check on alignment:
table(tracking_data$side_of_ball)
# defense football  offense
# 8984816  1168172  6984600


# Save each week separately -----------------------------------------------

# As rds files:
walk(1:17,
     function(week_i) {

       tracking_data %>%
         filter(week == week_i) %>%
         write_rds(paste0("data/input/weekly_pass_tracking/week",
                          week_i, ".rds"), compress = "gz")

     })






