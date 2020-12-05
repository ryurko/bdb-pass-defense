# File looks at getting information about the types of plays available to us
# from the Big Data Bowl:

library(data.table)
library(magrittr)

# Access the plays file:
plays_data <- fread("big_data_bowl_repo/Data/plays.csv")
nrow(plays_data)

# Now get the 2017 nflscrapR play-by-play data to join:
nflscrapr_pbp_17_name <- "https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv"
nflscrapr_pbp_17 <- readr::read_csv(nflscrapr_pbp_17_name) %>% 
  as.data.table() 
# Rename the play id and game id columns for the nflscrapR data, then create
# the keys used to join the play and nflscrapR data together:
setnames(nflscrapr_pbp_17, c("game_id", "play_id"), c("gameId", "playId"))
setkeyv(nflscrapr_pbp_17, c("gameId", "playId"))
setkeyv(plays_data, c("gameId", "playId"))

merged_plays <- plays_data %>%
  .[nflscrapr_pbp_17, nomatch = 0] 
nrow(merged_plays)

# Visualize the play_type distribution:
library(tidyverse)
merged_plays %>%
  ggplot(aes(play_type)) +
  geom_bar() +
  theme_bw()

# For ball carrier model - we only want the pass, run, kickoffs, and punts - for
# now we will ignore the no_plays == penalties:
model_plays <- merged_plays[play_type %in% c("kickoff", "pass", "punt",
                                             "run"),]
nrow(model_plays)
# [1] 13146

# For kickoffs and punts - we only want to use plays that had returns for the
# ball carrier model:
model_plays <- model_plays[touchback == 0 & kickoff_fair_catch == 0 & punt_fair_catch == 0,]
nrow(model_plays)
# [1] 12310

# Now for ball carrier model - will only use passing plays where either the 
# QB scrambled or it was a completion:
bc_model_plays <- model_plays[play_type %in% c("kickoff", "punt", "run") |
                                (play_type == "pass" & (complete_pass == 1 |
                                   sack == 1 | interception == 1))]
# Note that scrambles all have positive yardage and are runs - so sacks should be included
nrow(bc_model_plays)
# [1] 10155

# Dataset for pass decisions:
pd_model_plays <- model_plays[qb_dropback == 1,]
nrow(pd_model_plays)
# [1] 6925

# Catch prob models:
cp_model_plays <- pd_model_plays[pass_attempt == 1, ]
nrow(cp_model_plays)
# [1] 6679


