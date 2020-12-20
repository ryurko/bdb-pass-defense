# PURPOSE: Construct datasets for all receptions with information about
#          each player on the field with respect to the ball-carrier. This
#          will ignore interceptions and just focus on offensive receptions.
#          The main idea here is to construct a wide dataset at the frame level
#          containing the covariates listed on page 19 in Table 2 of  the
#          Going Deep manuscript: https://arxiv.org/pdf/1906.01760.pdf

#          NOTE: For now we are NOT constructing voronoi variables due to
#                complications with handling the missing linemen...


# Access necessary packages -----------------------------------------------

library(tidyverse)


# Construct dataset for each week -----------------------------------------

# Walk through each week, load the week's respective file then save the
# processed version of the dataset with player information with respect to
# the ball carrier (following the steps in code/R/s0_preprocess_input/test_init_yac_model_data.R)

walk(1:17,
     function(week_i) {

       week_data <-
         read_rds(paste0("data/input/weekly_pass_tracking/week",
                          week_i, ".rds")) %>%
         # Only use completions
         filter(passResult == "C")

       # Next get the ball carrier information:
       bc_data <- week_data %>%
         # Can just use the is_target indicator for this since they're receptions:
         filter(is_target == 1) %>%
         # Will NOT include in the starting point when the pass is caught in the endzone!
         # Only interested in plays with yards after the catch to start:
         mutate(is_start_bc = as.numeric(event %in% c("pass_outcome_caught", "pass_arrived")),
                # Now all of the options for the end of the ball carrier sequence:
                is_end_bc = as.numeric(event %in% c("fumble", "out_of_bounds", "tackle",
                                                    "touchdown", "pass_outcome_touchdown")))

       # Summarize to check how many plays have both start and end frames, including
       # where they are at:
       bc_seq_info <- bc_data %>%
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
       bc_data <- bc_data %>%
         left_join(bc_seq_info, by = c("gameId", "playId")) %>%
         # Remove plays without start and end frames:
         filter(!is.na(start_bc_frame)) %>%
         # Next for each play - only want the frames between these points:
         filter(frameId >= start_bc_frame, frameId <= end_bc_frame)

       # Convert the dataset with all of the information above into a more concise
       # dataset with just the necessary ball carrier info to then create variables
       # for offense and defense players with respect to:
       bc_info <- bc_data %>%
         dplyr::select(gameId, playId, frameId, nflId,
                       event, playDirection, displayName, position,
                       x, y, s, a, dis, o, dir, is_start_bc, is_end_bc,
                       start_bc_frame, end_bc_frame) %>%
         # Rename columns:
         dplyr::rename(bc_nflId = nflId, bc_displayName = displayName, bc_position = position,
                       bc_x = x, bc_y = y, bc_s = s, bc_a = a, bc_dis = dis,
                       bc_o = o, bc_dir = dir)

       # Now want to join the ball carrier information at the frame level to the rest
       # of the tracking data, removing rows without data, to then compute distances
       # for each defender, sorting their covariate by distance:
       other_players_info <- week_data %>%
         filter(is_target == 0, displayName != "Football") %>%
         left_join(dplyr::select(bc_info, gameId, playId,
                                 frameId, bc_x, bc_y, bc_dir),
                   by = c("gameId", "playId", "frameId")) %>%
         filter(!is.na(bc_x))

       # Now compute distance to ball carrier for each player at each frame:
       long_other_players_info <- other_players_info %>%
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
                       playDirection, side_of_ball, player_dist_bc_rank,
                       nflId, displayName, position,
                       dist_to_bc, x, y, s, a, dis, o, dir,
                       # Keep the bc_x, bc_y, bc_dir for constructing covariates
                       # then will drop after
                       bc_x, bc_y, bc_dir) %>%
         # Now make variables adjusted to the target endzone (since these are only
         # for receptions I don't have to worry about plays flipping). The orientation
         # is set up so that all plays are going to the right with 110 marking the
         # target endzone and y from 0 to (160 / 3) denotes from right to left (from
         # the QB's perspective).
         # First adjust the x coordinates
         mutate(adj_x = 110 - x, adj_bc_x = 110 - bc_x,
                # Next the y so that 0 indicates middle of field  (from QB POV)
                # while > 0 indicates left side and < 0 indicates right side
                adj_y = y - (160 / 6), adj_bc_y = bc_y - (160 / 6),
                # Compute change variables with respect to ball carrier and target
                # endzone:
                adj_x_change = adj_bc_x - adj_x, adj_y_change = adj_bc_y - adj_y,
                # Next compute direction with respect to target endzone:
                dir_target_endzone =
                  case_when(
                    (playDirection == "left") & (90 < dir) ~ 270 - dir,
                    (playDirection == "left") & (dir <= 90) ~ -(dir + 90),
                    (playDirection == "right") & (dir < 270) ~ 90 - dir,
                    (playDirection == "right") & (dir >= 270) ~ 450 - dir,
                    TRUE ~ NA_real_),
                # Now compute the direction with respect to the ball-carrier as
                # simply the absolute minimum difference - this will be the minimum
                # across a few possible scenarios to deal 0 to 360 limits
                dir_wrt_bc_diff = pmin(
                  pmin(abs(bc_dir - dir),
                       abs(bc_dir - (dir - 360))), abs(bc_dir - (dir + 360)))) %>%
         # Now drop the bc columns:
         dplyr::select(-bc_x, -bc_y, -bc_dir)


       # Make a wide version:
       wide_other_players_info <- long_other_players_info %>%
         # First unite side_of_ball and player_dist_bc_rank:
         unite("player_type", side_of_ball:player_dist_bc_rank, sep = "_") %>%
         # Now convert to wider dataset using the player_type as the name:
         pivot_wider(names_from = player_type,
                     values_from = nflId:dir,
                     names_glue = "{player_type}_{.value}")


       # Join wide data for other players to ball carrier data and save
       bc_info %>%
         left_join(wide_other_players_info, by = c("gameId", "playId", "frameId")) %>%
         write_rds(paste0("data/input/weekly_bc_features/week",
                          week_i, ".rds"), compress = "gz")

     })
