# PURPOSE: Create the modeling dataset for conditional density estimation,
#          joining the weekly datasets together but only retaining the plays
#          where the reception did not occur in the endzone. Also add the actual
#          response variable to each play - denoting the change in x from the
#          start to end of the play

library(tidyverse)



# Construct each week's model dataset and save ----------------------------

walk(1:17,
     function(week_i) {

            # Load the week's data
            week_data <-
              read_rds(paste0("data/input/weekly_bc_features/week",
                              week_i, ".rds")) %>%
              # Make a unique identifier for each play:
              unite(game_play_id, gameId:playId,
                    sep = "_", remove = FALSE)

            # Now summarize each play with info about where the ball-carrier
            # was at the start and end - this info will be used to remove
            # plays where the ball carrier started in the endzone. Additionally,
            # find if and when the possible start outcomes of "pass_outcome_caught", "pass_arrived"
            # occurred. If caught is there then use it, otherwise use arrived

            week_play_summary <- week_data %>%
              group_by(game_play_id) %>%
              arrange(frameId) %>%
              # Determine the start frame based on when the ball was caught,
              # otherwise if that is missing then use when the ball arrived:
              summarize(adj_start_bc_frame = ifelse(any(event == "pass_outcome_caught"),
                                                    frameId[which(event == "pass_outcome_caught")],
                                                    frameId[which(event == "pass_arrived")]),
                        start_adj_bc_x = ifelse(any(event == "pass_outcome_caught"),
                                                adj_bc_x[which(event == "pass_outcome_caught")],
                                                adj_bc_x[which(event == "pass_arrived")]),
                        end_adj_bc_x = last(adj_bc_x)) %>%
              ungroup()

            # Which plays correspond to starting in the endzone?
            td_rec_play_ids <- week_play_summary %>%
              filter(start_adj_bc_x <= 0) %>%
              pull(game_play_id)

            # Now filter the data to exclude these TD receptions and then
            # join the summary data over to only keep rows corresponding to
            # the actual start of the ball carrier sequence, followed by
            # computing the yards gained from ball carrier's position:
            week_data %>%
              filter(!(game_play_id %in% td_rec_play_ids)) %>%
              dplyr::left_join(week_play_summary, by = "game_play_id") %>%
              # Remove rows before ball is caught:
              filter(frameId >= adj_start_bc_frame) %>%
              # Calculate response variable:
              mutate(end_x_change = adj_bc_x - end_adj_bc_x,
                     # Make a new is_start_bc column:
                     is_start_bc = as.numeric(frameId == adj_start_bc_frame)) %>%
              # Drop some unnecessary columns:
              dplyr::select(-start_bc_frame, -end_bc_frame, -adj_start_bc_frame) %>%
              # Add the week id to use for the cross validation purpose:
              mutate(week_id = week_i) %>%
              distinct() %>%
              write_rds(paste0("data/model_data/weekly/week",
                               week_i, ".rds"), compress = "gz")

          })


