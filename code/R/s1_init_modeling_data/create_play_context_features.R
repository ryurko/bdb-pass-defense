# PURPOSE: Create variables with additional context about play-level
#          information such as personnel packages and even about the state
#          of the game. Maybe info about score differential affects the
#          behavior of receivers post-catching the football...

library(tidyverse)


# Load play-level data ----------------------------------------------------

plays_data <- read_csv("data/input/plays.csv")


# Peak at personnel package info ------------------------------------------

# What are the possible personnel packages for both sides of the football:
table(plays_data$personnelO)
# 0 RB, 0 TE, 5 WR       0 RB, 1 TE, 0 WR,1 P,1 LS,2 DL,1 K
# 36                                        1
# 0 RB, 1 TE, 0 WR,1 P,3 LB,1 LS,1 DL,4 DB 0 RB, 1 TE, 1 WR,1 P,3 LB,1 LS,1 DL,3 DB
# 1                                        2
# 0 RB, 1 TE, 4 WR       0 RB, 2 TE, 0 WR,1 P,1 LS,1 DL,1 K
# 173                                        1
# 0 RB, 2 TE, 0 WR,1 P,3 LB,1 LS,4 DB                         0 RB, 2 TE, 3 WR
# 1                                       51
# 0 RB, 3 TE, 2 WR 1 RB, 0 TE, 0 WR,1 P,3 LB,1 LS,1 DL,4 DB
# 3                                        1
# 1 RB, 0 TE, 0 WR,1 P,4 LB,1 LS,3 DB                    1 RB, 0 TE, 3 WR,1 DB
# 1                                        1
# 1 RB, 0 TE, 3 WR,1 DL                         1 RB, 0 TE, 4 WR
# 1                                      273
# 1 RB, 1 TE, 1 WR,1 P,4 LB,1 LS,1 DL,1 DB                         1 RB, 1 TE, 2 WR
# 1                                        1
# 1 RB, 1 TE, 2 WR,1 DB                    1 RB, 1 TE, 2 WR,1 DL
# 1                                        7
# 1 RB, 1 TE, 2 WR,1 P,3 LB,1 LS,2 DB                         1 RB, 1 TE, 3 WR
# 1                                    13716
# 1 RB, 2 TE, 1 WR,1 DB                    1 RB, 2 TE, 1 WR,1 DL
# 3                                        5
# 1 RB, 2 TE, 1 WR,1 P,2 LB,1 LS,3 DB      1 RB, 2 TE, 1 WR,1 P,4 LB,1 LS,1 DB
# 1                                        1
# 1 RB, 2 TE, 2 WR                         1 RB, 3 TE, 1 WR
# 2737                                      406
# 1 RB, 4 TE, 0 WR                   2 QB, 0 RB, 1 TE, 3 WR
# 8                                        6
# 2 QB, 1 RB, 0 TE, 3 WR              2 QB, 1 RB, 1 TE, 1 WR,1 DL
# 7                                        1
# 2 QB, 1 RB, 1 TE, 2 WR                   2 QB, 1 RB, 2 TE, 1 WR
# 56                                        4
# 2 QB, 2 RB, 1 TE, 1 WR                   2 QB, 2 RB, 2 TE, 0 WR
# 6                                        1
# 2 RB, 0 TE, 3 WR                         2 RB, 1 TE, 2 WR
# 148                                     1083
# 2 RB, 2 TE, 0 WR,1 P,3 LB,1 LS,2 DB                         2 RB, 2 TE, 1 WR
# 1                                      165
# 2 RB, 3 TE, 0 WR                         3 RB, 0 TE, 2 WR
# 12                                       13
# 3 RB, 1 TE, 1 WR                         3 RB, 2 TE, 0 WR
# 19                                        2
# 4 RB, 1 TE, 0 WR              6 OL, 0 RB, 0 TE, 0 WR,4 DL
# 1                                        1
# 6 OL, 0 RB, 1 TE, 0 WR,1 P,1 LS,1 DL,1 K                   6 OL, 1 RB, 0 TE, 3 WR
# 2                                       29
# 6 OL, 1 RB, 1 TE, 1 WR,1 DL                   6 OL, 1 RB, 1 TE, 2 WR
# 1                                      111
# 6 OL, 1 RB, 2 TE, 0 WR,1 DL                   6 OL, 1 RB, 2 TE, 1 WR
# 4                                       48
# 6 OL, 1 RB, 3 TE, 0 WR                   6 OL, 2 RB, 0 TE, 2 WR
# 9                                       10
# 6 OL, 2 RB, 1 TE, 0 WR,1 DL                   6 OL, 2 RB, 1 TE, 1 WR
# 3                                       11
# 6 OL, 2 RB, 2 TE, 0 WR      7 OL, 0 RB, 1 TE, 0 WR,1 P,1 LS,1 K
# 21                                        1

# Okay that's a bit absurd... what about for defense:
table(plays_data$personnelD)
# 0 DL, 2 LB, 4 DB, 3 RB,2 WR 0 DL, 3 LB, 3 DB, 2 RB,2 TE,1 WR      0 DL, 3 LB, 4 DB, 3 RB,1 QB
# 1                                1                                1
# 0 DL, 4 LB, 4 DB, 1 RB,2 WR      0 DL, 4 LB, 4 DB, 1 TE,2 WR                 0 DL, 4 LB, 7 DB
# 1                                1                               32
# 0 DL, 5 LB, 6 DB                 0 DL, 6 LB, 5 DB                 1 DL, 2 LB, 7 DB
# 17                                6                                1
# 1 DL, 3 LB, 5 DB, 1 TE,1 WR           1 DL, 3 LB, 6 DB, 1 WR                 1 DL, 3 LB, 7 DB
# 1                                1                              102
# 1 DL, 4 LB, 3 DB, 1 RB,2 WR                 1 DL, 4 LB, 5 DB           1 DL, 4 LB, 5 DB, 1 RB
# 1                                1                                1
# 1 DL, 4 LB, 6 DB                 1 DL, 5 LB, 5 DB           2 DL, 2 LB, 6 DB, 1 WR
# 311                              266                                1
# 2 DL, 2 LB, 7 DB                 2 DL, 3 LB, 5 DB           2 DL, 3 LB, 5 DB, 1 WR
# 71                                2                                4
# 2 DL, 3 LB, 6 DB                 2 DL, 4 LB, 4 DB           2 DL, 4 LB, 4 DB, 1 RB
# 917                                1                                1
# 2 DL, 4 LB, 5 DB                 3 DL, 0 LB, 8 DB           3 DL, 1 LB, 6 DB, 1 TE
# 2109                                1                                2
# 3 DL, 1 LB, 6 DB, 1 WR                 3 DL, 1 LB, 7 DB                 3 DL, 2 LB, 5 DB
# 3                               82                                1
# 3 DL, 2 LB, 5 DB, 1 WR                 3 DL, 2 LB, 6 DB           3 DL, 3 LB, 4 DB, 1 WR
# 2                             1047                                1
# 3 DL, 3 LB, 5 DB                 3 DL, 4 LB, 4 DB                 3 DL, 5 LB, 3 DB
# 3103                              743                                1
# 4 DL, 0 LB, 7 DB                 4 DL, 1 LB, 6 DB                 4 DL, 2 LB, 4 DB
# 24                             1104                                2
# 4 DL, 2 LB, 5 DB           4 DL, 3 LB, 3 DB, 1 WR                 4 DL, 3 LB, 4 DB
# 6652                                1                             2407
# 4 DL, 4 LB, 2 DB                 4 DL, 4 LB, 3 DB                 4 DL, 5 LB, 2 DB
# 1                               23                                4
# 5 DL, 1 LB, 5 DB                 5 DL, 2 LB, 4 DB           5 DL, 3 LB, 2 DB, 1 OL
# 13                               93                                3
# 5 DL, 3 LB, 3 DB                 5 DL, 4 LB, 2 DB                 5 DL, 5 LB, 1 DB
# 14                                1                                1
# 6 DL, 1 LB, 4 DB                 6 DL, 2 LB, 3 DB                 6 DL, 3 LB, 2 DB
# 2                                3                               12
# 6 DL, 4 LB, 1 DB                 7 DL, 3 LB, 1 DB
# 12                                1

# So I think for both I should just make three separate columns, for offense:
# 1) # RBs 2) # WRs, 3) # TEs
# and then for defense:
# 1) # DLs 2) # LBs, 3) # DBs


# Create personnel package info variables for modeling --------------------

# In order to find how many of a certain position are in each play, just need
# to first split the variable using "," and then grab the elements with the
# particular positions, to return the number. It looks like the the main three
# for both offense and defense are always present even with 0

# Helper function to pull number from corresponding vector of personnel positions:
get_n_players_position <- function(personnel_string, type_position) {
  personnel_string %>%
    str_subset(type_position) %>%
    str_extract("[:digit:]+") %>%
    as.numeric()
}

personnel_package_info <-
  map_dfr(1:nrow(plays_data),
          function(play_i) {
            # Get the play row:
            play_row <- plays_data[play_i,]

            # Next split the strings for each:
            off_personnel_strings <-
              str_split(play_row$personnelO, pattern = ",") %>% unlist() %>%
              str_trim()
            def_personnel_strings <-
              str_split(play_row$personnelD, pattern = ",") %>% unlist() %>%
              str_trim()

            # Return a tibble with the game and play ids along with the number
            # of players corresponding to the positions of interest:
            tibble(gameId = play_row$gameId, playId = play_row$playId,
                   n_rb = get_n_players_position(off_personnel_strings, "RB"),
                   n_wr = get_n_players_position(off_personnel_strings, "WR"),
                   n_te = get_n_players_position(off_personnel_strings, "TE"),
                   n_dl = get_n_players_position(def_personnel_strings, "DL"),
                   n_lb = get_n_players_position(def_personnel_strings, "LB"),
                   n_db = get_n_players_position(def_personnel_strings, "DB"))

          })



# Next create the first down marker coordinate ----------------------------

# For each play - will find the x of the first down marker with respect to the
# target endzone, so that it is in the same context of other modeling information

# We can convert the field position based on the possession team and yardline
# side variables:
plays_data <- plays_data %>%
  mutate(yds_from_tgt_endzone =
           ifelse((possessionTeam != yardlineSide) |
                    # Plays at the 50 yard marker have NA for yardlineSide
                    (yardlineNumber == 50), yardlineNumber,
                  100 - yardlineNumber),
         adj_x_first_down = yds_from_tgt_endzone - yardsToGo)

# NOTE: could subtract 10 from absoluteYardlineNumber - but there are missings
# in there, so I prefer my way here


# Create score differential with respect to possession team ---------------

# Load game-level data
games_data <- read_csv("data/input/games.csv")

# Join this info to the plays data:
plays_data <- plays_data %>%
  left_join(games_data, by = "gameId") %>%
  # Next compute the score differential with respect to the possession team:
  mutate(posteam_score_diff = ifelse(possessionTeam == homeTeamAbbr,
                                     preSnapHomeScore - preSnapVisitorScore,
                                     preSnapVisitorScore - preSnapHomeScore))

summary(plays_data$posteam_score_diff)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# -44.000 -10.000  -3.000  -3.044   3.000  42.000     639
# NAs here are from the missing score rows



# Create seconds remaining in half and game -------------------------------

time_remaining_info <-
  map_dfr(1:nrow(plays_data),
          function(play_i) {
            play_row <- plays_data[play_i,]

            # Split the game clock string:
            game_clock_vector <- str_split(play_row$gameClock, ":") %>%
              unlist() %>% as.numeric()

            # Make a tibble with seconds remaining in quarter:
            tibble(gameId = play_row$gameId, playId = play_row$playId,
                   qtr_sec_remain = game_clock_vector[1] * 60 + game_clock_vector[2]) %>%
              # Add half and game time remaining:
              mutate(game_half = ifelse(play_row$quarter < 3, 1,
                                        ifelse(play_row$quarter != 5, 2, 3)),
                     half_sec_remain = ifelse(play_row$quarter %in% c(1, 3),
                                              qtr_sec_remain + 900,
                                              qtr_sec_remain),
                     game_sec_remain = ifelse(game_half == 1,
                                              half_sec_remain + 1800,
                                              half_sec_remain))

          })


# View summary of defenders in box and pass rushers -----------------------

summary(plays_data$defendersInTheBox)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#   1.000   6.000   6.000   6.036   7.000  11.000      62
summary(plays_data$numberOfPassRushers)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#   0.000   4.000   4.000   4.216   5.000  10.000     633
# Okay can use these in their current status - I imagine these will be pretty
# relevant, especially when thinking about modeling at release of throw


# Make indicators for formation and dropback ------------------------------

# View summary of formation variables:
table(plays_data$offenseFormation)
# EMPTY     I_FORM      JUMBO     PISTOL    SHOTGUN SINGLEBACK    WILDCAT
# 2428        915         51        251      12627       2790         36

# Dropback summary:
table(plays_data$typeDropback)
# DESIGNED_ROLLOUT_LEFT DESIGNED_ROLLOUT_RIGHT               SCRAMBLE  SCRAMBLE_ROLLOUT_LEFT
#                   141                    482                    677                    462
# SCRAMBLE_ROLLOUT_RIGHT            TRADITIONAL                UNKNOWN
#                   1096                  15645                     97

# Now create indicator variables for each of these:
formation_indicators <- plays_data %>%
  dplyr::select(gameId, playId, offenseFormation) %>%
  mutate(is_value = 1) %>%
  pivot_wider(names_from = offenseFormation,
              values_from = is_value, names_prefix = "is_",
              values_fill = 0) %>%
  # Remove the is_NA column:
  dplyr::select(-is_NA)

dropback_indicators <- plays_data %>%
  dplyr::select(gameId, playId, typeDropback) %>%
  mutate(is_value = 1) %>%
  pivot_wider(names_from = typeDropback,
              values_from = is_value, names_prefix = "is_",
              values_fill = 0) %>%
  dplyr::select(-is_NA)



# Join play level data together and save ----------------------------------

play_level_features_data <- plays_data %>%
  dplyr::select(gameId, playId, adj_x_first_down, posteam_score_diff,
                down, quarter, defendersInTheBox, numberOfPassRushers) %>%
  left_join(personnel_package_info, by = c("gameId", "playId")) %>%
  inner_join(time_remaining_info, by = c("gameId", "playId")) %>%
  inner_join(formation_indicators, by = c("gameId", "playId")) %>%
  inner_join(dropback_indicators, by = c("gameId", "playId"))

write_csv(play_level_features_data,
          "data/model_data/play_level_features.csv")





