# Initial attempts at constructing data in manner accessible for modeling

library(data.table)
library(magrittr)

# Access the games, players, and plays files along with the first game 

games_data <- fread("big_data_bowl_repo/Data/games.csv")
players_data <- fread("big_data_bowl_repo/Data/players.csv")
plays_data <- fread("big_data_bowl_repo/Data/plays.csv")

ex_game_tracking_data <- fread("big_data_bowl_repo/Data/tracking_gameId_2017090700.csv")

# First merge the game level info:
setkeyv(ex_game_tracking_data, c("gameId", "playId", "nflId"))
setkeyv(plays_data, c("gameId", "playId"))
setkeyv(games_data, c("gameId"))
setkey(players_data, nflId)

# They provide this code for merging
ex_game_tracking_merged <- ex_game_tracking_data %>%
  .[games_data, nomatch = 0] %>%
  .[plays_data, nomatch = 0] %>%
  merge(., players_data, all.x = TRUE)

# What are the possible events?
table(ex_game_tracking_data$event)

# ball_snap             extra_point     extra_point_attempt              fair_catch              field_goal 
#     3564                     198                     198                     110                      44 
# field_goal_attempt           first_contact                  fumble                 handoff           kick_received 
#                 44                    2178                      88                    1188                     198 
# kickoff            kickoff_land                 lateral                line_set           man_in_motion 
#     286                      88                      44                     242                     307 
# out_of_bounds            pass_arrived            pass_forward     pass_outcome_caught pass_outcome_incomplete 
#           506                    1166                    1496                     902                     594 
# pass_outcome_touchdown             pass_shovel             pass_tipped                    punt               punt_land 
#                     22                      88                      22                     308                      44 
# punt_received                qb_kneel                 qb_sack                     run                   shift 
#           154                      66                     110                      88                      44 
# snap_direct                  tackle               touchback               touchdown 
#         22                    1848                     132                     198 

# Can use a combination of the PassResult, isPenalty, isSTPlay to filter the 
# plays considered to only be run and pass plays:
ex_game_pass_run_plays <- ex_game_tracking_merged[!isPenalty & !isSTPlay,]
table(ex_game_pass_run_plays$event)
# ball_snap           first_contact                  fumble                 handoff                 lateral 
#     2926                    1892                      44                    1188                      44 
# line_set           man_in_motion           out_of_bounds            pass_arrived            pass_forward 
#     220                     285                     396                    1100                    1452 
# pass_outcome_caught pass_outcome_incomplete  pass_outcome_touchdown             pass_shovel             pass_tipped 
#                 858                     594                      22                      66                      22 
# qb_kneel                 qb_sack                     run                   shift             snap_direct 
#       66                      88                      88                      44                      22 
# tackle               touchdown 
#   1584                     176 

# Number of plays:
length(unique(ex_game_pass_run_plays$playId))
# 135

# Need to summarise each play to when the different events occurred. Is it safe
# to assume that for every player/ball in the play they all have the same number
# of frames and the same events? I would think so. Let's first see how many players
# are in each play (should be the same for every play):
ex_game_play_summary <- ex_game_pass_run_plays[, 
                                               .(n_players = length(unique(nflId)),
                                                 n_frames = length(unique(frame.id)),
                                                 has_ball = any(displayName == "football"),
                                                 has_snap = any(event == "ball_snap"),
                                                 has_qb = any(PositionAbbr == "QB")),
                                               by = .(playId)]
all(ex_game_play_summary$n_players == 23) # 23 for the ball?
# FALSE - meaning there's a play missing something
View(ex_game_pass_run_plays[playId == ex_game_play_summary$playId[which(ex_game_play_summary$n_players != 23)],])
# Missing a player...

# Animate this play:
library(tidyverse)
library(gganimate)
library(cowplot)

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

# Use Lopez's code - just grab the example play above with the missing play:
example.play <- ex_game_pass_run_plays[playId == ex_game_play_summary$playId[which(ex_game_play_summary$n_players != 23)],]

## Specific boundaries for a given play
ymin <- max(round(min(example.play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example.play$x, na.rm = TRUE) + 10, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

animate.play <- ggplot() +
  geom_point(data = example.play, aes(x = (xmax-y), y = x, 
                                      colour = team, group = nflId, pch = team, size = team)) + 
  geom_text(data = example.play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(19, 16, 19), guide = FALSE) +
  scale_colour_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  ylim(ymin, ymax) + 
  coord_fixed() +  
  theme_nothing() + 
  transition_time(frame.id)  +
  ease_aes('linear') + 
  NULL

## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.play$frame.id))
animate(animate.play, fps = 10, nframe = play.length.ex)

# Looks like the defense is missing a player, see what the team counts look like:
ex_team_play_summary <- ex_game_pass_run_plays[, 
                                               .(n_players = length(unique(nflId)),
                                                 n_frames = length(unique(frame.id)),
                                                 has_ball = any(displayName == "football"),
                                                 has_snap = any(event == "ball_snap"),
                                                 has_qb = any(PositionAbbr == "QB")),
                                               by = .(playId, team)]
table(ex_team_play_summary$n_players)
#    1  10  11 
#  135   1 269 
# One player only has 10 players for the away team - which is odd...


# What about the snap of the ball - when the play actually starts - seems like
# something we definitely need..
table(ex_game_play_summary$has_snap)
# TRUE 
# 134 

# Ok which play is missing it:
ex_game_play_summary$playId[which(is.na(ex_game_play_summary$has_snap))]

# What happens in that play?
ex_game_pass_run_plays$playDescription[which(ex_game_pass_run_plays$playId == 701)[1]]
# [1] "(8:48) (Shotgun) Direct snap to T.Kelce.  T.Kelce up the middle to KC 23 for 4 yards (J.Richards, M.Brown)."
# Huh so the direct snap play is missing the ball snap? What does it have instead?
table(ex_game_pass_run_plays$event[which(ex_game_pass_run_plays$playId == 701)])
# first_contact           run   snap_direct        tackle 
#            22            22            22            22 
# Ah it has snap_direct
table(ex_game_pass_run_plays$event[which(ex_game_pass_run_plays$playId == 701 & ex_game_pass_run_plays$displayName == "football")])
table(ex_game_pass_run_plays$event[which(ex_game_pass_run_plays$displayName == "football")])
# Note that for the ball - there are no recordings as to the event

# Since every play has a QB - let's just filter on only frames for the QB in
# each play to get the frames:
table(ex_game_play_summary$has_qb)
# TRUE 
# 135 

ex_game_qb_frames <- ex_game_pass_run_plays[PositionAbbr == "QB",]
ex_game_qb_frames[, .N, by = .(playId)] %>%
  ggplot(aes(x = N)) +
  geom_histogram(color = "black", fill = "darkblue") +
  theme_bw()

ex_game_qb_frames[, .N, by = .(playId, frame.id)] %>%
  .[, N] %>%
  table()
# 1 
# 10166
# Each playId / frame.id combo only has 1 observation - so only 1 QB per play

# let's look at the distribution of the frame.id values for each of these events:

library(ggbeeswarm)
ex_game_qb_frames %>%
  .[!is.na(event), ] %>%
  ggplot(aes(x = event, y = frame.id)) +
  geom_quasirandom() +
  theme_bw() +
  coord_flip()
  
# Make a column to denote pass or run and compare:
ex_game_qb_frames %>%
  .[, play_type := ifelse(is.na(PassResult), "run", "pass")] %>%
  .[!is.na(event), ] %>%
  ggplot(aes(x = event, y = frame.id)) +
  geom_quasirandom() +
  theme_bw() +
  coord_flip() +
  facet_wrap(~play_type, ncol = 1, scales = "free_y")

ex_game_qb_frames %>%
  .[!is.na(event), event] %>%
  table()

ex_game_qb_frames[, .(missing_snap = !any(event %in% c("ball_snap", "snap_direct"))), by = .(playId)] %>%
  #.[,] %>%
  .[114,]


  example.play <- ex_game_pass_run_plays[playId == 3261,]
  
  ## Specific boundaries for a given play
  ymin <- max(round(min(example.play$x, na.rm = TRUE) - 10, -1), 0)
  ymax <- min(round(max(example.play$x, na.rm = TRUE) + 10, -1), 120)
  df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
  df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
  df.hash <- df.hash %>% filter(y < ymax, y > ymin)
  
  animate.play <- ggplot() +
    geom_point(data = example.play, aes(x = (xmax-y), y = x, 
                                        colour = team, group = nflId, pch = team, size = team)) + 
    geom_text(data = example.play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
              vjust = 0.36, size = 3.5) + 
    scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
    scale_shape_manual(values = c(19, 16, 19), guide = FALSE) +
    scale_colour_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
    annotate("text", x = df.hash$x[df.hash$x < 55/2], 
             y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
    annotate("text", x = df.hash$x[df.hash$x > 55/2], 
             y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
    annotate("segment", x = xmin, 
             y = seq(max(10, ymin), min(ymax, 110), by = 5), 
             xend =  xmax, 
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
             angle = 270, size = 4) + 
    annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
             angle = 90, size = 4) + 
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    ylim(ymin, ymax) + 
    coord_fixed() +  
    theme_nothing() + 
    transition_time(frame.id)  +
    ease_aes('linear') + 
    NULL
  
  ## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.play$frame.id))
animate(animate.play, fps = 10, nframe = play.length.ex)
# Nothing happens in this play just motion, what about the play after?

example.play <- ex_game_pass_run_plays[playId == 3444, ]

## Specific boundaries for a given play
ymin <- max(round(min(example.play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example.play$x, na.rm = TRUE) + 10, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

animate.play <- ggplot() +
  geom_point(data = example.play, aes(x = (xmax-y), y = x, 
                                      colour = team, group = nflId, pch = team, size = team)) + 
  geom_text(data = example.play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(19, 16, 19), guide = FALSE) +
  scale_colour_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  ylim(ymin, ymax) + 
  coord_fixed() +  
  theme_nothing() + 
  transition_time(frame.id)  +
  ease_aes('linear') + 
  NULL

## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.play$frame.id))
animate(animate.play, fps = 10, nframe = play.length.ex)

# OK so need to filter only on plays that have the ball snap - then adjust the
# frames so that every player frame has columns containing the ball location 
# information.

# Join the nflscrapR data using the example code from the FAQ:
nflscrapr_pbp_17_name <- "https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv"
nflscrapr_pbp_17 <- readr::read_csv(nflscrapr_pbp_17_name) %>% 
  as.data.table() 
nflscrapr_pbp_17 <- nflscrapr_pbp_17[, mget(c("game_id", "play_id",
                                         colnames(nflscrapr_pbp_17)[
                                           stringr::str_detect(colnames(nflscrapr_pbp_17),
                                                      "(player_id)|(player_name)")
                                         ]))]
setnames(nflscrapr_pbp_17, c("game_id", "play_id"), c("gameId", "playId"))
setkeyv(nflscrapr_pbp_17, c("gameId", "playId"))
setkeyv(ex_game_tracking_merged, c("gameId", "playId"))
ex_game_tracking_merged <- ex_game_tracking_merged %>%
  .[nflscrapr_pbp_17, nomatch = 0] 

brady_tracking_data <- ex_game_tracking_merged[displayName == "Tom Brady",]
# So the nflId is completely different from the GSIS id in nflscrapR - will
# have to use partial matching based on the names for the ball carriers.
  