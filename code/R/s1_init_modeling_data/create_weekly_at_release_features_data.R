# PURPOSE: Construct datasets for all receptions with information about
#          each player on the field with respect to the target receiver at the
#          time of release of the throw. The main idea here is to construct a
#          wide dataset at the frame level containing the covariates listed on
#          page 19 in Table 2 of the Going Deep manuscript: https://arxiv.org/pdf/1906.01760.pdf
#          along with variables with respect to how far the target receiver is
#          from the QB at the release

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

       # Event for pass release: pass_forward

       # Get the QB position at the point of passing:
       qb_data <- week_data %>%
         filter(position == "QB", event == "pass_forward") %>%
         distinct()

       # In case there are multiple QBs - find the one closest to the football
       # at the time of release:
       football_data <- week_data %>%
         filter(displayName == "Football", event == "pass_forward") %>%
         distinct() %>%
         dplyr::select(gameId, playId, frameId, x, y) %>%
         dplyr::rename(ball_x = x, ball_y = y)

       # Join to the QB data, then only keep the QB closest to the ball:
       qb_data <- qb_data %>%
         dplyr::left_join(football_data, by = c("gameId", "playId", "frameId")) %>%
         mutate(dist_to_ball = sqrt((x - ball_x)^2 + (y - ball_y)^2)) %>%
         group_by(gameId, playId, frameId) %>%
         arrange(dist_to_ball) %>%
         slice(1) %>%
         ungroup() %>%
         dplyr::select(-ball_x, -ball_y, -dist_to_ball)

       # Get the receiver info at the release point:
       rec_data <- week_data %>%
         filter(is_target == 1, event == "pass_forward") %>%
         distinct()

       # Convert the dataset with all of the information above into a more concise
       # dataset with just the necessary ball carrier info to then create variables
       # for offense and defense players with respect to:
       rec_info <- rec_data %>%
         dplyr::select(gameId, playId, frameId, nflId, displayName, position,
                       x, y, s, a, dis, o, dir, playDirection) %>%
         # Rename columns:
         dplyr::rename(bc_nflId = nflId, bc_displayName = displayName, bc_position = position,
                       bc_x = x, bc_y = y, bc_s = s, bc_a = a, bc_dis = dis,
                       bc_o = o, bc_dir = dir) %>%
         dplyr::mutate(adj_bc_x = 110 - bc_x,
                       adj_bc_y = bc_y - (160 / 6),
                       bc_dir_target_endzone =
                         case_when(
                           (playDirection == "left") & (90 < bc_dir) ~ 270 - bc_dir,
                           (playDirection == "left") & (bc_dir <= 90) ~ -(bc_dir + 90),
                           (playDirection == "right") & (bc_dir < 270) ~ 90 - bc_dir,
                           (playDirection == "right") & (bc_dir >= 270) ~ 450 - bc_dir,
                           TRUE ~ NA_real_)) %>%
         dplyr::select(-playDirection)

       # Do the same for QB info to then join over:
       qb_info <- qb_data %>%
         dplyr::select(gameId, playId, frameId, nflId, displayName,
                       x, y, s, a, dis, o, dir, playDirection) %>%
         # Rename columns:
         dplyr::rename(qb_nflId = nflId, qb_displayName = displayName,
                       qb_x = x, qb_y = y, qb_s = s, qb_a = a, qb_dis = dis,
                       qb_o = o, qb_dir = dir) %>%
         dplyr::mutate(adj_qb_x = 110 - qb_x,
                       adj_qb_y = qb_y - (160 / 6),
                       qb_dir_target_endzone =
                         case_when(
                           (playDirection == "left") & (90 < qb_dir) ~ 270 - qb_dir,
                           (playDirection == "left") & (qb_dir <= 90) ~ -(qb_dir + 90),
                           (playDirection == "right") & (qb_dir < 270) ~ 90 - qb_dir,
                           (playDirection == "right") & (qb_dir >= 270) ~ 450 - qb_dir,
                           TRUE ~ NA_real_)) %>%
         dplyr::select(-playDirection)

       # Now want to join the ball carrier information at the frame level to the rest
       # of the tracking data, removing rows without data, to then compute distances
       # for each defender, sorting their covariate by distance:
       other_players_info <- week_data %>%
         filter(is_target == 0, displayName != "Football",
                position != "QB") %>%
         left_join(dplyr::select(rec_info, gameId, playId,
                                 frameId, bc_x, bc_y, adj_bc_x, adj_bc_y),
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
                       # Keep the bc_x, bc_y for constructing covariates
                       # then will drop after
                       bc_x, bc_y, adj_bc_x, adj_bc_y) %>%
         # Now make variables adjusted to the target endzone (since these are only
         # for receptions I don't have to worry about plays flipping). The orientation
         # is set up so that all plays are going to the right with 110 marking the
         # target endzone and y from 0 to (160 / 3) denotes from right to left (from
         # the QB's perspective).
         # First adjust the x coordinates
         mutate(adj_x = 110 - x,
                # Next the y so that 0 indicates middle of field  (from QB POV)
                # while > 0 indicates left side and < 0 indicates right side
                adj_y = y - (160 / 6),
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
                # Compute the angle between the ball-carrier and the player:
                angle_with_bc = (atan2(adj_y_change, -adj_x_change) * 180) / pi,
                # Now compute the direction with respect to the ball-carrier as
                # simply the absolute minimum difference - this will be the minimum
                # across a few possible scenarios to deal 0 to 360 limits
                dir_wrt_bc_diff = pmin(
                  pmin(abs(angle_with_bc - dir_target_endzone),
                       abs(angle_with_bc - (dir_target_endzone - 360))),
                  abs(angle_with_bc - (dir_target_endzone + 360)))) %>%
         # Now drop the bc columns
         dplyr::select(-bc_x, -bc_y, -angle_with_bc, -adj_bc_x, -adj_bc_y)


       # Make a wide version:
       wide_other_players_info <- long_other_players_info %>%
         # First unite side_of_ball and player_dist_bc_rank:
         unite("player_type", side_of_ball:player_dist_bc_rank, sep = "_") %>%
         # Now convert to wider dataset using the player_type as the name:
         pivot_wider(names_from = player_type,
                     values_from = nflId:dir_wrt_bc_diff,
                     names_glue = "{player_type}_{.value}")


       # Join wide data for other players to ball carrier data and save
       rec_info %>%
         left_join(qb_info, by = c("gameId", "playId", "frameId")) %>%
         # Compute distance to QB in two ways - change in x and then actual distance:
         mutate(adj_x_change_to_qb = adj_qb_x - adj_bc_x,
                bc_dist_to_qb = sqrt((bc_x - qb_x)^2 + (bc_y - qb_y)^2)) %>%
         left_join(wide_other_players_info, by = c("gameId", "playId", "frameId")) %>%
         write_rds(paste0("data/input/weekly_at_release_features/week",
                          week_i, ".rds"), compress = "gz")

     })
