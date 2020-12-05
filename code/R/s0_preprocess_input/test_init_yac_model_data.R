# PURPOSE: Construct the datasets for all receptions with information about
#          each player on the field with respect to the ball-carrier. This
#          will ignore interceptions and just focus on offensive receptions.
#          The main idea here is to construct a wide dataset at the frame level
#          containing the covariates listed on page 19 in Table 2 of  the
#          Going Deep manuscript: https://arxiv.org/pdf/1906.01760.pdf

#          NOTE: For now we are NOT constructing voronoi variables due to
#                complications with handling the missing linemen...


# Access necessary packages -----------------------------------------------

library(tidyverse)

# Load test week for figuring out steps -----------------------------------

# Load week 2 since its over 25 MB compressed
test_week_data <- read_rds("data/input/weekly_pass_tracking/week2.rds")

# Filter down to completions:
test_week_data <- test_week_data %>%
  filter(passResult == "C")


# Find the ball-carrier frames --------------------------------------------

# Want to filter down to the frames after the receiver caught the football,
# finding the start and end points of the ball carry sequence:
bc_test_data <- test_week_data %>%
  # Can just use the is_target indicator for this since they're receptions:
  filter(is_target == 1)

# What are the events:
table(bc_test_data$event)
# ball_snap            first_contact                   fumble fumble_defense_recovered
#       817                      402                       15                        5
# fumble_offense_recovered                  handoff                  lateral                 line_set
#                       7                        7                        1                       47
# man_in_motion                     None            out_of_bounds             pass_arrived
#           32                    50424                      167                      807
# pass_forward             pass_lateral      pass_outcome_caught   pass_outcome_touchdown
#         810                        4                      779                       33
# pass_shovel              play_action                punt_fake          run_pass_option
#         7                      144                        1                        1
# shift              snap_direct                   tackle                touchdown
#   22                        1                      585                       32

# There is a fake punt in there... hmmm could consider removing later on...

# Create an indicator variable denoting the start and end points:
bc_test_data <- bc_test_data %>%
  # Will NOT include in the starting point when the pass is caught in the endzone!
  # Only interested in plays with yards after the catch to start:
  mutate(is_start_bc = as.numeric(event %in% c("pass_outcome_caught", "pass_arrived")),
         # Now all of the options for the end of the ball carrier sequence:
         is_end_bc = as.numeric(event %in% c("fumble", "out_of_bounds", "tackle",
                                             "touchdown", "pass_outcome_touchdown")))

# Summarize to check how many plays have both start and end frames, including
# where they are at:
bc_seq_info <- bc_test_data %>%
  group_by(gameId, playId) %>%
  mutate(any_start = any(is_start_bc == 1),
         any_end = any(is_end_bc == 1)) %>%
  # Only use plays with both:
  filter(any_start, any_end) %>%
  # Now summarize to find the start and end frames: (just using the first
  # one for start now - but see NOTE below)
  summarize(start_bc_frame = frameId[which(is_start_bc == 1)[1]],
            end_bc_frame = frameId[which(is_end_bc == 1 &
                                           # Need to only consider end points
                                           # that are after the pass arrived
                                           # since QBs can fumble the snap and
                                           # then still throw
                                           frameId > start_bc_frame)[1]]) %>%
  ungroup()

# Join these start and end points over:
bc_test_data <- bc_test_data %>%
  left_join(bc_seq_info, by = c("gameId", "playId")) %>%
  # Remove plays without start and end frames:
  filter(!is.na(start_bc_frame)) %>%
  # Next for each play - only want the frames between these points:
  filter(frameId >= start_bc_frame, frameId <= end_bc_frame)

# NOTE: May have to make a decision on how to handle pass outcome caught versus
#       pass arrived - since there are plays where pass caught is missing...


# Create dataset of ball carrier info for plays ---------------------------

# Convert the dataset with all of the information above into a more concise
# dataset with just the necessary ball carrier info to then create variables
# for offense and defense players with respect to:
bc_test_info <- bc_test_data %>%
  dplyr::select(gameId, playId, frameId, nflId,
                event, playDirection, displayName, position,
                x, y, s, a, dis, o, dir, is_start_bc, is_end_bc,
                start_bc_frame, end_bc_frame) %>%
  # Rename columns:
  dplyr::rename(bc_nflId = nflId, bc_displayName = displayName, bc_position = position,
                bc_x = x, bc_y = y, bc_s = s, bc_a = a, bc_dis = dis,
                bc_o = o, bc_dir = dir)



# Create dataset of info on other players ---------------------------------


# Now want to join the ball carrier information at the frame level to the rest
# of the tracking data, removing rows without data, to then compute distances
# for each defender, sorting their covariate by distance:
other_players_test_info <- test_week_data %>%
  filter(is_target == 0, displayName != "Football") %>%
  left_join(dplyr::select(bc_test_info, gameId, playId, frameId, bc_x, bc_y),
            by = c("gameId", "playId", "frameId")) %>%
  filter(!is.na(bc_x))

# Now compute distance to ball carrier for each player at each frame:
long_other_players_test_info <- other_players_test_info %>%
  mutate(dist_to_bc = sqrt((x - bc_x)^2 + (y - bc_y)^2)) %>%
  # Now group by the game, play, frame, and side of ball to sort the players
  group_by(gameId, playId, frameId, side_of_ball) %>%
  # Sort by closest distance:
  arrange(dist_to_bc) %>%
  # Make a column denoting this order:
  mutate(player_dist_bc_rank = 1:n()) %>%
  # Finally ungroup:
  ungroup() %>%
  # Select the necessary columns:
  dplyr::select(gameId, playId, frameId,
                side_of_ball, player_dist_bc_rank,
                nflId, displayName, position,
                dist_to_bc, x, y, s, a, dis, o, dir)

# Make a wide version:
wide_other_players_test_info <- long_other_players_test_info %>%
  # First unite side_of_ball and player_dist_bc_rank:
  unite("player_type", side_of_ball:player_dist_bc_rank, sep = "_") %>%
  # Now convert to wider dataset using the player_type as the name:
  pivot_wider(names_from = player_type,
              values_from = nflId:dir,
              names_glue = "{player_type}_{.value}")


# Join wide data for other players to ball carrier data -------------------

test_week_model_data <- bc_test_info %>%
  left_join(wide_other_players_test_info, by = c("gameId", "playId", "frameId"))




