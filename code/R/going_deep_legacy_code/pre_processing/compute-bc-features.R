# Lee Richardson
# Purpose: Compute design matrix for ball-carrier model 
rm(list=ls())

devtools::load_all("../goingdeepR")
library(dplyr)
library(readr)

# --- Set file paths and load metadata ---
base_directory <- "/home/lee/Dropbox/NFL-EP-tracking"
output_directory <- file.path(base_directory, "data/ball_carrier_frames")

## Get a vector of all tracking game IDs
game_files <- list.files(file.path(base_directory, "data/ball_carrier_tracking_data"), full.names=TRUE)
big_data_bowl_directory <- paste0(base_directory, "/big_data_bowl_repo/Data")

## Load the play-level meta-data
play_meta <- readr::read_csv(file=file.path(base_directory, "big_data_bowl_repo/Data/plays.csv"))
game_meta <- readr::read_csv(file=file.path(base_directory, "big_data_bowl_repo/Data/games.csv"))

# --- Compute features ---

## Create the column names that we will use in the features data frame
#   ---------------------------------------------------
# - One set of features from the complete game-level data
# - One set of features from the play-level
# - One set of features with the frame-level coordinates
#   ---------------------------------------------------
play_features_to_keep <- c("gameId", "playId", "frame.id", "play_type", "event", "time", "nflId", "displayName",
                        "team", "home_team", "away_team", "jerseyNumber", "passer_player_name", "receiver_player_name",
                        "rusher_player_name", "interception_player_name", "punt_returner_player_name", "kickoff_returner_player_name",
                        "own_kickoff_recovery_player_name", "fumble_recovery_1_player_name", "fumble_recovery_2_player_name",
                        "qb_scramble", "sack", "qb_hit", "fumble", "interception", "complete_pass", "fumble_out_of_bounds",
                        "touchdown", "yards_after_catch","pass_touchdown", "is_ball_carrier", "is_passer", "is_rusher",
                        "is_receiver", "is_interceptor", "is_ko_returner", "is_punt_returner", "is_own_ko_recover", "is_fumble_rec_1",
                        "is_fumble_rec_2")
new_play_features <- c("bc_orientation", "ball_start", "ball_end", "ball_pos")
bc_coord_names <- c("bc_x", "bc_y", "bc_s", "bc_dis", "bc_dir")
offense_coord_names <- c(paste0("offense", 1:10, "_x"), paste0("offense", 1:10, "_y"), paste0("offense", 1:10, "_s"),
                         paste0("offense", 1:10, "_dis"), paste0("offense", 1:10, "_dir"),
                         paste0("offense", 1:10, "_dist_to_ball"))
defense_coord_names <- c(paste0("defense", 1:11, "_x"), paste0("defense", 1:11, "_y"), paste0("defense", 1:11, "_s"),
                         paste0("defense", 1:11, "_dis"), paste0("defense", 1:11, "_dir"),
                         paste0("defense", 1:11, "_dist_to_ball"))
feature_names <- c(play_features_to_keep, new_play_features, bc_coord_names, offense_coord_names, defense_coord_names)

## Indicate the last column that should be a character 
last_char_column <- which(feature_names == "bc_orientation")

## Loop over all games in directory
bc_sequence_id <- 1
for (file in game_files) {
  game_df <- suppressMessages(readr::read_csv(file=file))
  game_id <- game_df$gameId[1]
  cat("Game: ", game_id, " \n")
  game_meta_row <- game_meta[game_meta$gameId == game_id, ]
  plays <- unique(game_df$playId)
  
  ## Extract the team orientation from the first tracking play 
  original_game_df <- suppressMessages(readr::read_csv(file.path(big_data_bowl_directory, 
                                                                 paste0("tracking_gameId_", game_id, ".csv"))))
  team_orientation <- goingdeepR::get_team_orientation(original_game_df, play_meta, game_meta_row)

  ## Loop over all plays in a game
  for (play in plays) { 
    play_df <- game_df[game_df$playId == play, ]
    play_meta_row <- play_meta[play_meta$gameId == game_id & play_meta$playId == play, ]
    players <- unique(play_df$displayName)
    num_ball_carrier <- 0 

    ## Loop over all players involved in the play
    for (player in players) {
      player_df <- play_df[play_df$displayName == player, ]
      bc_frames <- player_df$frame.id[player_df$is_ball_carrier == 1]

      ## Check if this player has any frames where they are the ball carrier
      if ( length(bc_frames) > 0 ) {
        ## Verify that all of the frames are consecutive
        if ( !all(min(bc_frames):max(bc_frames) == bc_frames) ) {
          stop("Ball carrier has > 1 sequence with the ball on this play")
        }
        
        ## Update the number of ball carrier's on this particular play 
        num_ball_carrier <- num_ball_carrier + 1
        
        ## Compute play-level features
        play_features <- data.frame(matrix(data=NA, nrow=length(bc_frames), ncol=length(feature_names)))
        bc_df <- player_df[bc_frames, ]
        ball_start <- bc_df$x[1]
        ball_end <- bc_df$x[nrow(bc_df)]
        bc_orientation <- goingdeepR::get_bc_orientation(home_away=as.character(bc_df[1, "team"]),  
                                                         quarter=play_meta_row$quarter, 
                                                         team_orientation=team_orientation)
        ## Compute frame-level features
        for (bc_frame in bc_frames) {
          bc_df_ind <- bc_df$frame.id == bc_frame
          ball_pos <- bc_df[bc_df_ind, "x"]
          frame_df <- play_df[play_df$frame.id == bc_frame, ]
          bc_coords <- bc_df[bc_df_ind, c("x", "y", "s", "dis", "dir")]
          player_coords <- goingdeepR::get_closest_players(frame_df=frame_df)
          if ( is.null(player_coords) ) { next }

          ## Store the frame level-features in the play-level dataframe
          play_features[bc_df_ind, 1:length(play_features_to_keep)] <- bc_df[bc_df_ind, play_features_to_keep]
          play_features[bc_df_ind, (length(play_features_to_keep) + 1):ncol(play_features)] <- c(bc_orientation, 
                                                                                                 ball_start, 
                                                                                                 ball_end, 
                                                                                                 ball_pos, 
                                                                                                 as.numeric(bc_coords), 
                                                                                                 player_coords)
        }
        
        ## Add a ball carrier sequence ID 
        play_features$bc_sequence_id <- bc_sequence_id
        bc_sequence_id <- bc_sequence_id + 1
      
        ## Append together all the ball carriers on a single play
        full_play_features <- goingdeepR::append_df(df=play_features, full_df=full_play_features, 
                                                    iter=num_ball_carrier, iter_list=1:10)
        
      ## If this player is never the ball carrier, move to the next play
      } else {
        next
      }
    }
    
    ## Append this play onto the the data-frame for all plays in this game 
    if (num_ball_carrier == 0) { next }
    game_features <- goingdeepR::append_df(df=full_play_features, full_df=game_features, iter=play, iter_list=plays)
  }
  
  ## Add column names and write out the final data-frame 
  names(game_features)[1:length(feature_names)] <- feature_names
  output_path <- file.path(output_directory, paste0("game_", game_id, ".csv"))
  readr::write_csv(x=game_features, path=output_path)
}
