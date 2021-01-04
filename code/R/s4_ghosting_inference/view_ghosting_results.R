# PURPOSE: Examine the ghosting results at the player level

library(tidyverse)


# Load ghosting results ---------------------------------------------------

ghost_yac_distr_summary <-
  read_rds("data/ghosting_output/player_ghost_yac_distr_summary.rds")


# Summarize at player level -----------------------------------------------

player_ghost_summary <- ghost_yac_distr_summary %>%
  group_by(nfl_id, player_display_name) %>%
  summarize(n_plays = n(),
            yac_better_than_ghost = sum(as.numeric(delta_expected_yac < 0)) / n_plays,
            prob_td_better_than_ghost = sum(as.numeric(delta_prob_td < 0)) / n_plays,
            prob_first_down_better_than_ghost = sum(as.numeric(delta_prob_first_down < 0)) / n_plays,
            total_yac_diff = sum(delta_expected_yac, na.rm = TRUE),
            ave_yac_diff = mean(delta_expected_yac, na.rm = TRUE),
            total_prob_td_diff = sum(delta_prob_td, na.rm = TRUE),
            ave_prob_td_diff = mean(delta_prob_td, na.rm = TRUE),
            total_prob_first_down_diff = sum(delta_prob_first_down,
                                             na.rm = TRUE),
            ave_prob_first_down_diff = mean(delta_prob_first_down,
                                            na.rm = TRUE),
            total_prob_pos_yac_diff = sum(delta_prob_positive_yac,
                                          na.rm = TRUE),
            ave_prob_pos_yac_diff = mean(delta_prob_positive_yac,
                                         na.rm = TRUE)) %>%
  ungroup()


# Visual comparisons of player performance --------------------------------

# Display a summary of the players:
library(ggrepel)
player_ghost_summary %>%
  filter(n_plays >= 10) %>%
  # # Join their positions:
  # left_join(ghosting_data %>%
  #             dplyr::select(nflId, position) %>%
  #             group_by(nflId) %>%
  #             summarize(position = first(position)) %>%
  #             ungroup(), by = c("nfl_id" = "nflId")) %>%
  ggplot(aes(x = ave_yac_diff, y = ave_prob_td_diff)) +
  geom_point() +
  geom_label_repel(data = player_ghost_summary %>%
                     #filter(n_plays >= 50) %>%
                     arrange(total_yac_diff) %>%
                     slice(1:10), #%>%
                     # Join their positions:
                     # left_join(ghosting_data %>%
                     #             dplyr::select(nflId, position) %>%
                     #             group_by(nflId) %>%
                     #             summarize(position = first(position)) %>%
                     #             ungroup(), by = c("nfl_id" = "nflId")),
                   aes(label = player_display_name),
                   min.segment.length = 0, box.padding = 0.5) +
  theme_bw() +
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred")

player_ghost_summary %>%
  filter(n_plays >= 50) %>%
  # # Join their positions:
  # left_join(ghosting_data %>%
  #             dplyr::select(nflId, position) %>%
  #             group_by(nflId) %>%
  #             summarize(position = first(position)) %>%
  #             ungroup(), by = c("nfl_id" = "nflId")) %>%
  ggplot(aes(x = total_prob_first_down_diff, y = total_prob_td_diff)) +
  geom_point() +
  # geom_point(data = player_ghost_summary %>%
  #              filter(player_display_name == "Brian Poole"),
  #            size = 5, color = "darkred") +
  geom_label_repel(data = player_ghost_summary %>%
                     filter(n_plays >= 50) %>%
                     arrange(total_prob_first_down_diff) %>%
                     slice(1:10), #%>%
                   # Join their positions:
                   # left_join(ghosting_data %>%
                   #             dplyr::select(nflId, position) %>%
                   #             group_by(nflId) %>%
                   #             summarize(position = first(position)) %>%
                   #             ungroup(), by = c("nfl_id" = "nflId")),
                   aes(label = player_display_name),
                   min.segment.length = 0, box.padding = 0.5) +
  theme_bw() +
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred")



