# Author: Ron Yurko
# Purpose: Fix the target x for the running plays since it's currently broken

library(data.table)
library(tidyverse)

# Load the design matrix file constructed by Lee:
bc_design_matrix <- read_csv("data/bc-design-matrix.csv")


# Ok let's join the the nflscrapR yardline information to use instead:
nflscrapr_pbp_17_data_url <- "https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv"
pbp_play_info <- readr::read_csv(nflscrapr_pbp_17_data_url) %>%
  dplyr::select(game_id, play_id, yardline_100, posteam, defteam, desc,
                home_team, away_team, yards_gained)

# Left join this information:
bc_design_matrix_join <- bc_design_matrix %>%
  left_join(pbp_play_info,
            by = c("gameId" = "game_id", "playId" = "play_id"))

# Filter to only rushing plays (includes scrambles for now but should probably
# remove):
rusher_design_matrix <- bc_design_matrix_join %>%
  filter(is_rusher == 1)

rusher_design_matrix <- rusher_design_matrix %>%
  # Next use the ball_start (its the ball carrier's starting x) and ball_end (bc
  # final x for carry) to determine which target_x the bc is going towards. This
  # can commence in 2 ways:
  # X) if yardline_100 does not equal 50 then just use that with ball_start and
  #    and ball_end to determine target_x, - DEPRECATED this logic is flawed since
  #    the ball-carrier starts behind the line of scrimmage you dolt!
  # 1) if yardline_100 == 50 - IGNORE yardline_100 now! just use yards_gained
  #     then use the yards_gained != 0 with ball_end - ball_start
  #    to find target_x,
  # 2) else if yardline_100 == 50 and yards_gained == 0 (you're fucked) then go
  #    into the tracking data for these plays (should only be two of them and
  #    find the average x for the offense and defense)
  mutate(target_x = case_when(
    # THIS FIRST CHUNK IS INCORRECT!
    # First all the sitiuations where yardline_100 does not equal 50
    # (yardline_100 > 50 & ball_start < 60) ~ 120,
    # (yardline_100 > 50 & ball_start > 60) ~ 0,
    # (yardline_100 < 50 & ball_start < 60) ~ 0,
    # (yardline_100 < 50 & ball_start > 60) ~ 120,
    # # Next if yardline_100 is 50 exactly, use the non-zero yards gained:
    # (yardline_100 == 50 & yards_gained > 0 & ball_end >= ball_start) ~ 120,
    # (yardline_100 == 50 & yards_gained > 0 & ball_end < ball_start) ~ 0,
    # (yardline_100 == 50 & yards_gained < 0 & ball_end >= ball_start) ~ 0,
    # (yardline_100 == 50 & yards_gained < 0 & ball_end < ball_start) ~ 120,
    
    # Just use the yards_gained and the ball_end, ball_start fields:
    (yards_gained > 0 & ball_end > ball_start) ~ 120,
    (yards_gained > 0 & ball_end < ball_start) ~ 0,
    # WE CAN'T USE NEGATIVE YARDS GAINED SINCE THEY CAN RUN POSITIVE BUT END
    # UP BEHIND THE LINE OF SCRIMMAGE!!!!!! THE EASIEST THING MIGHT BE TO JUST
    # USE THE AVERAGE OFFENSE AND DEFENSE POSITIONS
    # (yards_gained < 0 & ball_end >= ball_start) ~ 0,
    # (yards_gained < 0 & ball_end <= ball_start) ~ 120,
    # Finally if yardline_100 == 50 and yards_gained == 0 set equal to 60 to
    # designate the frames that need more tracking data invovled:
    (yards_gained == 0) ~ 60,
    TRUE ~ NA_real_))

table(rusher_design_matrix$target_x)
#     0    60   120 
# 62897 11280 69290

length(which(is.na(rusher_design_matrix$target_x)))
# [1] 13586

# Next - get the unique game_play_id combos corresponding to the NA and 60
# target_x frames:
need_team_x_game_plays <- rusher_design_matrix %>%
  filter(target_x == 60 | is.na(target_x)) %>%
  unite("game_play_id", gameId, playId) %>%
  pull(game_play_id) %>%
  unique()

need_team_x_games <- rusher_design_matrix %>%
  filter(target_x == 60 | is.na(target_x)) %>%
  pull(gameId) %>%
  as.character() %>%
  unique()


# Get the list of tracking data files:
tracking_data_files <- list.files("big_data_bowl_repo/Data",
                                  pattern = "tracking",
                                  full.names = TRUE)
tracking_data_files <- sapply(need_team_x_games, function(x) str_subset(tracking_data_files,
                                                                        x)) %>%
  unlist()

# Go through each game and summarize the average starting x for the offense 
# and defense for each play - this will give us the correct target_x variable:
play_start_locations <- purrr::map_dfr(tracking_data_files,
                                       function(game_file_name) {
                                         # Load the file for the game's tracking data:
                                         game_file <- read_csv(game_file_name)
                                         
                                         # Get the starting frames of the ball for each play:
                                         play_start_data <- game_file %>%
                                           # Create the united id:
                                           unite("game_play", c("gameId", "playId"), remove = FALSE) %>%
                                           # Only look at ids that are in tracking_play_ids:
                                           filter(game_play %in% need_team_x_game_plays) %>%
                                           # Group by that id:
                                           group_by(game_play) %>%
                                           # Find the first non-missing event:
                                           summarize(start_frame_id = frame.id[which(!is.na(event) & event != "man_in_motion" &
                                                                                       event != "shift")[1]],
                                                     start_event = event[which(!is.na(event) & event != "man_in_motion" &
                                                                                 event != "shift")[1]]) %>%
                                           ungroup() %>%
                                           separate(game_play, c("gameId", "playId"),
                                                    remove = FALSE) %>%
                                           # Make a column that is the play_frame united:
                                           unite("play_frame", c("playId", "start_frame_id"), remove = FALSE)
                                         
                                         # If there is at least one play get the ball start_data:
                                         if (nrow(play_start_data) > 0) {
                                           ball_start_data <- game_file %>%
                                             # Create the united id:
                                             unite("play_frame", c("playId", "frame.id"), remove = FALSE) %>%
                                             # Only look at the rows for players
                                             filter(play_frame %in% play_start_data$play_frame &
                                                      !is.na(team)) %>%
                                             # Now group by the team to compute their average x at this 
                                             # starting frame - and then spread so its two columns:
                                             group_by(play_frame, team) %>%
                                             summarize(ave_x = mean(x, na.rm = TRUE)) %>%
                                             ungroup() %>%
                                             spread(team, ave_x)
                                           
                                           output <- play_start_data %>%
                                             inner_join(ball_start_data, by = "play_frame") %>%
                                             dplyr::select(gameId, playId, start_frame_id, start_event, 
                                                           away, ball, home) %>%
                                             rename(ave_away_x = away, ave_ball_x = ball,
                                                    ave_home_x = home)
                                         } else {
                                           # Otherwise 
                                           output <- data.frame("gameId" = NA, 
                                                                "playId" = NA, 
                                                                "start_frame_id" = NA, 
                                                                "start_event" = NA, 
                                                                "ave_away_x" = NA,
                                                                "ave_ball_x" = NA,
                                                                "ave_home_x" = NA)
                                         }
                                         return(output)
                                       })

# Find these plays in the nflscrapR data to get which team was home, away, on
# offense and defense:
missing_play_team_info <- pbp_play_info %>%
  unite("game_play_id", game_id, play_id, remove = FALSE) %>%
  filter(game_play_id %in% need_team_x_game_plays) %>%
  #dplyr::select(game_play_id, game_id, play_id, posteam, defteam, home_team, away_team) %>%
  mutate(home_offense = ifelse(home_team == posteam, 1, 0)) %>%
  dplyr::select(game_play_id, game_id, play_id, home_offense)
  
# Join this info to find the target x for the missing plays
play_start_locations <- play_start_locations %>%
  unite("game_play_id", gameId, playId, remove = FALSE) %>%
  left_join(missing_play_team_info, by = c("game_play_id")) %>%
  mutate(target_x = case_when(
    home_offense == 1 & ave_home_x < ave_away_x ~ 120,
    home_offense == 1 & ave_home_x > ave_away_x ~ 0,
    home_offense == 0 & ave_home_x < ave_away_x ~ 0,
    home_offense == 0 & ave_home_x > ave_away_x ~ 120,
    TRUE ~ NA_real_
  ))

# Finally join this back to the ball carrier data - first make a temporary
# column for the game_play_id:
rusher_design_matrix <- rusher_design_matrix %>%
  unite("game_play_id", gameId, playId, remove = FALSE)
# Now for each each missing play, replace the missing (or 60) target x with
# actual from the ball tracking data:
for (i in 1:nrow(play_start_locations)) {
  
  # Whats the game_play_id:
  current_game_play_id <- play_start_locations$game_play_id[i]
  
  # Replace the target_x for the design matrix with the actual for all frames
  # with the game_play_id:
  rusher_design_matrix$target_x[which(rusher_design_matrix$game_play_id == current_game_play_id)] <-
    play_start_locations$target_x[i]
  
  # Yup that's it
}

table(rusher_design_matrix$target_x)
#     0   120 
# 75461 81592  
any(is.na(rusher_design_matrix$target_x))
# [1] FALSE

# Finally drop the game_play_id column and then calculate the raw_x_change and
# the field_x_change variables:
rusher_design_matrix <- rusher_design_matrix %>%
  dplyr::select(-game_play_id) %>%
  mutate(raw_x_change = abs(ball_end - bc_x),
         # Now determine the change in x with respect to the target_x, this
         # just determines the sign to use for raw_x_change
         field_x_change = case_when(
           (target_x == 120 & ball_end > bc_x) ~ raw_x_change,
           (target_x == 0 & ball_end < bc_x) ~ raw_x_change,
           (target_x == 120 & ball_end < bc_x) ~ -1 * raw_x_change,
           (target_x == 0 & ball_end > bc_x) ~ -1 * raw_x_change,
           ball_end == bc_x ~ raw_x_change,
           TRUE ~ NA_real_
         ))
any(is.na(rusher_design_matrix$field_x_change))
# [1] FALSE


# Okay so now figure out why are there still massive negative plays? is this 
# just erroneous data to throw out? does not make sense...

summary(rusher_design_matrix$field_x_change)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -8.450   0.720   3.490   5.599   7.870  96.150 
summary(rusher_design_matrix$yards_gained)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -10.000   1.000   4.000   5.836   7.000  90.000 

# YES YES YES It is finally correct


write_csv(rusher_design_matrix, "data/initial_bc_design_matrices/rusher_bc_design_matrix_0923.csv")



