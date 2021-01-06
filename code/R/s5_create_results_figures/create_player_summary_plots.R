# PURPOSE: Create player-level displays based on the ghosting results

library(tidyverse)
library(ggrepel)
library(patchwork)

# Load the summary of ghosting evaluation ---------------------------------

ghost_yac_distr_summary <-
  read_rds("data/ghosting_output/player_ghost_speed_dir_yac_distr_summary.rds")


# Load players data to join info ------------------------------------------

players_data <- read_csv("data/input/players.csv")



# Load team info ----------------------------------------------------------

# Access from nflfastR
team_rosters <-
  readRDS(url("https://github.com/guga31bb/nflfastR-data/raw/master/roster-data/roster.rds")) %>%
  filter(team.season == 2018)

team_rosters <- team_rosters %>%
  dplyr::select(teamPlayers.nflId, team.abbr) %>%
  rename(nfl_id = teamPlayers.nflId, team_abbr = team.abbr) %>%
  filter(nfl_id %in% ghost_yac_distr_summary$nfl_id)

# Create summary version of player performance ----------------------------

player_ghost_summary <- ghost_yac_distr_summary %>%
  group_by(nfl_id, player_display_name) %>%
  summarize(n_plays = n(),
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
  ungroup() %>%
  # Join position:
  dplyr::left_join(dplyr::select(players_data, nflId, position),
                   by = c("nfl_id" = "nflId")) %>%
  # Create a simplified position grouping:
  mutate(position_easy =
           case_when(
             position %in% c("CB", "DB") ~ "Cornerback",
             position == "DE" ~ "Defensive end",
             position %in% c("FS", "S", "SS") ~ "Safety",
             position %in% c("ILB", "LB", "MLB", "OLB") ~ "Linebacker",
             TRUE ~ "Other")
           ) %>%
  # Join team:
  dplyr::left_join(team_rosters, by = "nfl_id")

# Whats the position_easy count:
table(player_ghost_summary$position_easy)
# Cornerback Defensive end    Linebacker         Other        Safety
# 219            20           224             3           131

# Update the factor order by this:
player_ghost_summary <- player_ghost_summary %>%
  mutate(position_easy = fct_relevel(position_easy,
                                     "Linebacker", "Cornerback",
                                     "Safety", "Defensive end", "Other"))

# Create displays of totals -----------------------------------------------

# First a display a total yac diff against total prob of pos yac diff:
yac_change_plot <- player_ghost_summary %>%
  #filter(n_plays >= 50) %>%
  ggplot(aes(x = total_yac_diff, y = total_prob_pos_yac_diff,
             color = position_easy)) +
  geom_point() +
  geom_label_repel(data = player_ghost_summary %>%
                     #filter(n_plays >= 50) %>%
                     arrange(total_yac_diff) %>%
                     slice(1:10),
                   aes(label = player_display_name,
                       color = position_easy),
                   min.segment.length = 0, box.padding = 1,
                   show.legend = FALSE) +
  annotate(geom = "text", label = 'Positioned "better" than ghosts', x = -50, y = -15,
           color = "darkred") +
  theme_bw() +
  ggthemes::scale_color_colorblind() +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(x = "Sum of change in expected YAC",
       y = "Sum of change in Pr(YAC > 0)",
       color = "Position")

# Next a display of probability of TD and first down:
prob_change_plot <- player_ghost_summary %>%
  ggplot(aes(x = total_prob_first_down_diff, y = total_prob_td_diff,
             color = position_easy)) +
  geom_point() +
  geom_label_repel(data = player_ghost_summary %>%
                     arrange(total_prob_first_down_diff) %>%
                     slice(1:10),
                   aes(label = player_display_name,
                       color = position_easy),
                   min.segment.length = 0, box.padding = 1,
                   show.legend = FALSE) +
  theme_bw() +
  ggthemes::scale_color_colorblind() +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(x = "Sum of change in Pr(1st down)",
       y = "Sum of change in Pr(TD)",
       color = "Position")
yac_change_plot_new <- yac_change_plot + theme(legend.position = "none")


(yac_change_plot_new / prob_change_plot) +
  plot_annotation(title = "Comparison of player performance based on summaries of estimates for YAC distribution",
                  subtitle = "Top 10 players based on change in expected YAC and Pr(1st down) are labeled")




# What about a display with all but a certain position grayed out so the
# specific position types are displayed:
player_ghost_summary %>%
  filter(position_easy != "Linebacker") %>%
  ggplot(aes(x = total_prob_first_down_diff, y = total_prob_td_diff)) +
  geom_point(color = "gray") +
  geom_point(data = filter(player_ghost_summary,
                           position_easy == "Linebacker"),
             aes(color = position)) +
  geom_label_repel(data = player_ghost_summary %>%
                     filter(position_easy == "Linebacker") %>%
                     arrange(total_prob_first_down_diff) %>%
                     slice(1:10),
                   aes(label = player_display_name,
                       color = position),
                   min.segment.length = 0, box.padding = 1,
                   show.legend = FALSE) +
  theme_bw() +
  ggthemes::scale_color_colorblind() +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(x = "Sum change Pr(1st down)",
       y = "Sum change Pr(TD)",
       color = "Linebacker type")

# For safety:
player_ghost_summary %>%
  filter(position_easy != "Safety") %>%
  ggplot(aes(x = total_prob_first_down_diff, y = total_prob_td_diff)) +
  geom_point(color = "gray") +
  geom_point(data = filter(player_ghost_summary,
                           position_easy == "Safety"),
             aes(color = position)) +
  geom_label_repel(data = player_ghost_summary %>%
                     filter(position_easy == "Safety") %>%
                     arrange(total_prob_first_down_diff) %>%
                     slice(1:10),
                   aes(label = player_display_name,
                       color = position),
                   min.segment.length = 0, box.padding = 1,
                   show.legend = FALSE) +
  theme_bw() +
  ggthemes::scale_color_colorblind() +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(x = "Sum change Pr(1st down)",
       y = "Sum change Pr(TD)",
       color = "Safety type")



# Create tables of rankings -----------------------------------------------

library(gt)


# First make rank columns for each stat for overall and then by position:
player_ghost_summary <- player_ghost_summary %>%
  mutate(all_xyac_rank = min_rank(total_yac_diff),
         all_posyac_rank = min_rank(total_prob_pos_yac_diff),
         all_fd_rank = min_rank(total_prob_first_down_diff),
         all_td_rank = min_rank(total_prob_td_diff)) %>%
  group_by(position_easy) %>%
  mutate(pos_xyac_rank = min_rank(total_yac_diff),
         pos_posyac_rank = min_rank(total_prob_pos_yac_diff),
         pos_fd_rank = min_rank(total_prob_first_down_diff),
         pos_td_rank = min_rank(total_prob_td_diff)) %>%
  ungroup()


# Create table with top five players in each position ---------------------

player_ghost_summary %>%
  filter(pos_fd_rank <= 5,
         position_easy %in% c("Cornerback", "Safety",
                              "Linebacker")) %>%
  #mutate(position_easy = fct_rev(position_easy)) %>%
  dplyr::select(player_display_name, team_abbr, position_easy,
                n_plays, total_prob_first_down_diff,
                total_yac_diff,
                total_prob_pos_yac_diff,
                #total_prob_td_diff,
                all_fd_rank, pos_fd_rank,
                all_xyac_rank, all_posyac_rank, all_td_rank) %>%
  mutate(total_prob_first_down_diff = round(total_prob_first_down_diff,
                                            digits = 2),
         total_yac_diff = round(total_yac_diff, digits = 2),
         total_prob_pos_yac_diff = round(total_prob_pos_yac_diff,
                                         digits = 2)) %>%
  group_by(position_easy) %>%
  arrange(all_fd_rank) %>%
  dplyr::select(-pos_fd_rank) %>%
  rename(Player = player_display_name,
         Position = position_easy,
         Team = team_abbr,
         `#plays` = n_plays,
         `Overall rank Pr(1st down)` = all_fd_rank,
         `Sum change Pr(1st down)` = total_prob_first_down_diff,
         `Sum change Pr(YAC > 0)` = total_prob_pos_yac_diff,
         `Sum change expected YAC` = total_yac_diff,
         #`Sum Pr(TD) change` = total_prob_td_diff,
         `Overall rank Pr(TD)` = all_td_rank,
         `Overall rank expected YAC` = all_xyac_rank,
         `Overall rank Pr(YAC > 0)` = all_posyac_rank) %>%
  gt() %>%
  row_group_order(
    groups = c("Linebacker", "Cornerback",
               "Safety")
  ) %>%
  tab_header(title = "Top players based on overall reduction in Pr(1st down)",
             subtitle = "Top five players by position") %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = vars(`#plays`)
      )
    )
  ) %>%
  data_color(columns = vars(`Sum change Pr(1st down)`,
                            `Sum change Pr(YAC > 0)`,
                            `Sum change expected YAC`),
             colors = scales::col_numeric(
               palette = c("darkorange", "darkblue"),
               domain = NULL
             ))









