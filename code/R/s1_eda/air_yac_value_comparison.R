# PURPOSE: Provide justification for examining YAC in terms of its comparable
#          value to air yards

library(tidyverse)


# Read in 2018 play-by-play data from nflfastR ----------------------------

pbp_data <-
  readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds'))

# Filter to only complete passes ------------------------------------------

rec_data <- pbp_data %>%
  # Only use regular season as well
  filter(week <= 17, complete_pass == 1, penalty == 0)


# Compare air and YAC values ----------------------------------------------

# Display a comparison of the distributions for yards and epa between air yards
# yac value, first create the dataset with these summaries for plotting
rec_stats_data <- rec_data %>%
  unite(game_play_id, c("old_game_id", "play_id")) %>%
  dplyr::select(game_play_id, air_yards, yards_after_catch, air_epa, yac_epa) %>%
  rename(yac_yards = yards_after_catch) %>%
  pivot_longer(air_yards:yac_epa,
               names_to = c("type", ".value"),
               names_pattern = "(air|yac)_(yards|epa)") %>%
  pivot_longer(yards:epa, names_to = "stat", values_to = "value") %>%
  mutate(stat = fct_relevel(stat, "yards", "epa"),
         stat = fct_recode(stat, `Yards gained` = "yards",
                           `Expected points added` = "epa"),
         type = fct_recode(type, `Air` = "air", `YAC` = "yac"))

# Display the histograms with the vertical lines at
rec_stats_data %>%
  ggplot(aes(x = value, color = type, fill = type)) +
  geom_histogram(position = "identity", alpha = 0.25) +
  geom_vline(data = rec_stats_data %>% group_by(type, stat) %>%
               summarize(mean_val = mean(value, na.rm = TRUE)),
             aes(xintercept = mean_val, color = type),
             size = 1, linetype = "dashed") +
  scale_color_manual(values = c("black", "blue")) +
  scale_fill_manual(values = c("black", "blue")) +
  facet_wrap(~ stat, scales = "free", ncol = 1) +
  labs(x = "Value", y = "Count", color = "Source of value:",
       fill = "Source of value:",
       title = "Comparison of value from air and YAC for completions in 2018",
       subtitle = "Vertical dashed lines denoted mean values",
       caption = "Source: nflfastR") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        legend.position = "bottom", legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

rec_stats_data %>% group_by(type, stat) %>%
  summarize(mean_val = mean(value, na.rm = TRUE))
#     type  stat                  mean_val
#     <fct> <fct>                    <dbl>
#   1 Air   Yards gained             6.03
#   2 Air   Expected points added    0.174
#   3 YAC   Yards gained             5.30
#   4 YAC   Expected points added    0.654

# Old kaggle code for comparison ------------------------------------------

# library(tidyverse)
#
# options(warn=-1)
# options(repr.plot.width=15, repr.plot.height = 10)
#
# df_games <- read_csv("../input/nfl-big-data-bowl-2021/games.csv",
#                      col_types = cols())
#
# df_plays <- read_csv("../input/nfl-big-data-bowl-2021/plays.csv",
#                      col_types = cols())
#
# df_players <- read_csv("../input/nfl-big-data-bowl-2021/players.csv",
#                        col_types = cols())
#
# weeks <- seq(1, 17)
#
# df_tracking <- data.frame()
#
# for(w in weeks){
#
#   df_tracking_temp <- read_csv(paste0("../input/nfl-big-data-bowl-2021/week",w,".csv"),
#                                col_types = cols())
#
#   df_tracking <- bind_rows(df_tracking_temp, df_tracking)
#
# }
#
# df_tracking <- df_tracking %>%
#   mutate(x = ifelse(playDirection == "left", 120-x, x),
#          y = ifelse(playDirection == "left", 160/3 - y, y)) %>% mutate(key=paste0(gameId,"-",playId))
#
#
# df_completed = df_plays %>% filter(passResult == "C") %>% mutate(key=paste0(gameId,"-",playId))
# df_completed_tracking = df_tracking %>% filter(key %in% df_completed$key) %>%
#   filter(event == "pass_outcome_caught"| event == "tackle" | event == "out_of_bounds" | event == "touchdown") %>%
#   filter(team == "football") %>% mutate(x = ifelse(x>110, 110, x)) %>% left_join(df_completed[,c("key","playResult")],by="key")
#
# df_caught = df_completed_tracking[df_completed_tracking$event == "pass_outcome_caught",]
# df_end = df_completed_tracking[df_completed_tracking$event != "pass_outcome_caught",]
# colnames(df_caught)[which(colnames(df_caught) == "x")] = "xs"
#
#
# df_yac = df_caught %>% left_join(df_end[,c("key","x")]) %>% mutate(yacP = round(x-xs)) %>% mutate(airY = playResult - yacP)
#
# ### plots
#
# ggplot(df_yac,aes(yacP))+geom_density()+ labs(x="YAC for each completed pass",y="Density",
#                                               title="Density of YAC in each completed pass play",caption="YinzR (2020)")+
#   theme_bw(base_size=17)
# summary(df_yac$yacP)
#
# ggplot(df_yac,aes(airY))+geom_density()+ labs(x="Air yards for each completed pass",y="Density",
#                                               title="Density for the air yards in each completed pass play",caption="YinzR (2020)")+
#   theme_bw(base_size=17)
# summary(df_yac$airY)
#
#
