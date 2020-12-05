# Author: Ron Yurko
# Purpose: Compare the LOWO CV predictions for each of the models:

# ------------------------------------------------------------------------------

library(tidyverse)
library(caret)

# Create vector of the lowo prediction file names:
lowo_no_lag_models <- c("baseline",# "lasso",
                        "xgboost", #"xgboost_200",
                        "ff_twofifty", "ff_two100",
                        "ff_threefifty", "ff_three100",
                        "ff_fivefifty", "ff_five100",
                        "ff_tenfifty", "ff_ten100",
                        #"neuralnet", "lstm",
                        # "ff_tenfive", "ff_threefive", "ff_fivefive",
                        # "ff_threeten", "ff_fiveten", "ff_tenten",
                        # "ff_threetwenty", 
                        "lstm_twofifty", "adj_lasso")

# First go through each of these files and compute the overall RMSE for each:
all_model_lowo_preds <- map_dfr(lowo_no_lag_models,
                                function(model_fit) {
                                  # pred_list <- readRDS(paste0("src/modeling/lowo_model_datasets_0923/",
                                  #                             model_fit, "_test_predictions.rds"))
                                  pred_list <- readRDS(paste0("src/modeling/lowo_model_datasets_adj_xydir_1022/",
                                                              model_fit, "_test_predictions.rds"))
                                  # Now create a stacked version of this heldout
                                  # predictions and observed values:
                                  week_i <- 0
                                  stacked_preds <- map_dfr(pred_list,
                                                           function(week) {
                                                             week_i <- week_i + 1
                                                             week %>%
                                                               dplyr::select(field_x_change,
                                                                             pred_field_x_change,
                                                                             frames_from_end,
                                                                             n_frames,
                                                                             target_x,
                                                                             bc_x,
                                                                             playId,
                                                                             gameId,
                                                                             nflId,
                                                                             bc_sequence_id,
                                                                             bc_frame_id,
                                                                             voronoi_bc_area,
                                                                             defense1_dist_to_ball,
                                                                             voronoi_bc_close_adj,
                                                                             bc_s) %>%
                                                               mutate(lowo_week = week_i)
                                                           }) %>%
                                    mutate(model_type = model_fit,
                                           frames_from_start = bc_frame_id - 1)
                                })

# Compute overall RMSE:
all_model_lowo_preds %>%
  group_by(model_type) %>%
  summarize(rmse = caret::RMSE(pred_field_x_change, field_x_change))


# Compute weighted RMSE:
all_model_lowo_preds %>%
  filter(model_type %in% c("baseline", "adj_lasso", "xgboost",
                           "ff_twofifty", "lstm_twofifty")) %>%
  group_by(model_type, frames_from_start) %>%
  summarize(rmse = caret::RMSE(pred_field_x_change, field_x_change),
            n_points = n()) %>%
  ungroup() %>%
  group_by(model_type) %>%
  summarize(weighted_ave_rmse = weighted.mean(rmse, n_points))

all_model_lowo_preds %>%
  filter(model_type %in% c("baseline", "adj_lasso", "xgboost",
                           "ff_twofifty", "lstm_twofifty")) %>%
  group_by(model_type, frames_from_end) %>%
  summarize(rmse = caret::RMSE(pred_field_x_change, field_x_change),
            n_points = n()) %>%
  ungroup() %>%
  group_by(model_type) %>%
  summarize(weighted_ave_rmse = weighted.mean(rmse, n_points))


all_model_lowo_preds %>%
  filter(model_type %in% c("baseline", "adj_lasso", "xgboost",
                           "ff_twofifty", "lstm_twofifty")) %>%
  group_by(model_type, frames_from_start) %>%
  summarize(rmse = caret::RMSE(pred_field_x_change, field_x_change),
            n_points = n()) %>%
  ungroup() %>%
  mutate(model_type = fct_relevel(model_type,
                                  "baseline", "adj_lasso", "xgboost",
                                  "ff_twofifty", "lstm_twofifty"),
         model_type = fct_recode(model_type,
                                 !!!c(`Intercept-only` = "baseline",
                                      LASSO = "adj_lasso",
                                      XGBoost = "xgboost",
                                      `FNN` = "ff_twofifty",
                                      LSTM = "lstm_twofifty"))) %>%
  ggplot() +
  geom_bar(aes(x = frames_from_start,
               alpha = n_points, 
               y = rmse),
           stat = "identity",
           fill = "darkred") + 
  geom_text(data = {(
    all_model_lowo_preds %>%
      group_by(model_type) %>%
      summarize(rmse = caret::RMSE(pred_field_x_change, field_x_change)) %>%
      filter(model_type %in% c("baseline", "adj_lasso", "xgboost",
                               "ff_twofifty", "lstm_twofifty")) %>%
      mutate(model_type = fct_relevel(model_type,
                                      "baseline", "adj_lasso", "xgboost",
                                      "ff_twofifty", "lstm_twofifty"),
             model_type = fct_recode(model_type,
                                     !!!c(`Intercept-only` = "baseline",
                                          LASSO = "adj_lasso",
                                          XGBoost = "xgboost",
                                          `FNN` = "ff_twofifty",
                                          LSTM = "lstm_twofifty")),
             rmse_label = paste0("RMSE = ", round(rmse, digits = 2))))},
    x = 50, y = 15, aes(label = rmse_label), size = 8, color = "darkred") +
  labs(x = "Frames from start of ball-carrier sequence",
       y = "RMSE",
       fill = "Model",
       title = "LOWO CV RMSE across frames from start of ball-carrier sequence",
       alpha = "Number of frames") +
  theme_bw() +
  theme(legend.position = c(.95, .05),
        legend.justification = c(1, 0),
        strip.background = element_blank(),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14)) +
  facet_wrap(~model_type, ncol = 3) 

all_model_lowo_preds %>%
  filter(model_type %in% c("baseline", "adj_lasso", "xgboost",
                           "ff_twofifty", "lstm_twofifty")) %>%
  mutate(error = pred_field_x_change - field_x_change) %>%
  mutate(model_type = fct_relevel(model_type,
                                  "baseline", "adj_lasso", "xgboost",
                                  "ff_twofifty", "lstm_twofifty"),
         model_type = fct_recode(model_type,
                                 !!!c(`Intercept-only` = "baseline",
                                      LASSO = "adj_lasso",
                                      XGBoost = "xgboost",
                                      `FNN` = "ff_twofifty",
                                      LSTM = "lstm_twofifty"))) %>%
  ggplot(aes(x = frames_from_start, y = error)) +
  stat_summary(fun.y = mean, geom = "point", color = "darkred") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "darkred",
               fun.args = list(mult = 2)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Frames from start of ball-carrier sequence",
       y = "Error",
       title = "LOWO CV mean error +/ two standard errors across frames from start ball-carrier sequence") +
  theme_bw() +
  theme(legend.position = c(.95, .05),
        legend.justification = c(1, 0),
        strip.background = element_blank(),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14)) +
  facet_wrap(~model_type, ncol = 3) 


# Now proceed to generate the holdout expected points estimates:
# Load the nflscrapR data to join select play information for:
nflscrapr_pbp_17_data_url <- "https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv"
nflscrapr_pbp_17 <- readr::read_csv(nflscrapr_pbp_17_data_url)

# For the LSTM model results, join the nflscrapR information that is necessary 
# for calculating the EP and WP:
lstm_lowo_preds_joined_data <- all_model_lowo_preds %>%
  filter(model_type == "lstm_twofifty") %>%
  left_join(dplyr::select(nflscrapr_pbp_17,
                          game_id, play_id,
                          half_seconds_remaining, game_seconds_remaining,
                          score_differential, qtr,
                          posteam_timeouts_remaining,
                          defteam_timeouts_remaining,
                          yardline_100, down,
                          ydstogo, goal_to_go,
                          yards_gained, desc,
                          ep, wp, epa, wpa) %>%
              rename(start_ep = ep,
                     start_wp = wp,
                     total_play_epa = epa,
                     total_play_wpa = wpa,
                     actual_yards_gained = yards_gained),
            by = c("gameId" = "game_id", "playId" = "play_id")) %>%
  # Remove anything that is missing nflscrapR information:
  drop_na()

initialize_ep_variables <- .  %>%
  mutate(pred_bc_x = ifelse(target_x == 120,
                            bc_x + pred_field_x_change,
                            bc_x - pred_field_x_change),
         # Cap this between 10 and 110:
         pred_bc_x = ifelse(target_x == 120 & pred_bc_x >= 110, 110, pred_bc_x),
         pred_bc_x = ifelse(target_x == 0 & pred_bc_x <= 10, 10, pred_bc_x),
         # Or if they are already in the endzone just use the endzone since there's
         # no reason to generate the predictions at that point - it could cause 
         # weird behavior with delays in players slowing down:
         pred_bc_x = ifelse(target_x == 120 & bc_x >= 110, 110, pred_bc_x),
         pred_bc_x = ifelse(target_x == 0 & bc_x <= 10, 10, pred_bc_x),
         pred_yardline_100 = ifelse(target_x == 120,
                                    110 - pred_bc_x,
                                    pred_bc_x - 10),
         # Determine if a scoring event occurred 
         score_ind = ifelse(pred_yardline_100 <= 0, 1, 0),
         safety_ind = ifelse(pred_yardline_100 >= 100, 1, 0),
         # Cap it between 0 and 100:
         pred_yardline_100 = ifelse(pred_yardline_100 < 0, 0, pred_yardline_100),
         pred_yardline_100 = ifelse(pred_yardline_100 > 100, 100, pred_yardline_100)) %>%
  # Using this predicted yardline compute the change in the ydstogo, goal_to_go
  # and down variables:
  mutate(convert_ind = ifelse(pred_yardline_100 <= (yardline_100 - ydstogo),
                              1, 0),
         new_ydstogo = ifelse(convert_ind == 1,
                              10, ydstogo - (yardline_100 - pred_yardline_100)),
         new_down = ifelse(convert_ind == 1,
                           1, down + 1),
         turnover_ind = ifelse(new_down == 5,
                               1, 0),
         new_goal_to_go = ifelse(new_down == 1 & (pred_yardline_100 <= 10),
                                 1, goal_to_go),
         # If goal to go then new_ydstogo is just the pred_yardline_100:
         new_ydstogo = ifelse(new_goal_to_go == 1,
                              pred_yardline_100, new_ydstogo),
         score_ind = ifelse(pred_yardline_100 == 0, 1, 0),
         # If its a turnover modify the down and flip the yardline and yards to go:
         new_down = ifelse(turnover_ind == 1,
                           1, new_down),
         init_pred_yardline_100 = pred_yardline_100,
         pred_yardline_100 = ifelse(turnover_ind == 1, 100 - pred_yardline_100,
                                    pred_yardline_100),
         new_ydstogo = ifelse(turnover_ind == 1 & pred_yardline_100 >= 10,
                              10,
                              new_ydstogo),
         new_goal_to_go = ifelse(turnover_ind == 1 & pred_yardline_100 <= 10, 1,
                                 new_goal_to_go)) %>%
  # Use the bc_frame_id column to create the new time remaining columns:
  group_by(bc_sequence_id) %>%
  mutate(sequence_length = max(bc_frame_id)) %>%
  ungroup() %>%
  mutate(sequence_seconds_change = sequence_length / 10,
         new_half_seconds_remaining = ifelse(half_seconds_remaining - 
                                               sequence_seconds_change >= 0,
                                             half_seconds_remaining - 
                                               sequence_seconds_change,
                                             0),
         new_game_seconds_remaining = ifelse(game_seconds_remaining - 
                                               sequence_seconds_change >= 0,
                                             game_seconds_remaining - 
                                               sequence_seconds_change,
                                             0))

# Add these columns to both datasets:
lstm_lowo_preds_joined_data <- lstm_lowo_preds_joined_data %>%
  initialize_ep_variables

# Define a function that takes a dataset with the necessary columns and joins
# the EP and WP columns taking into account which ones are being changed
# based on the scoring events that occurred:
library(nflscrapR)
add_ep_wp_columns <- function(ep_frame_data) {
  ep_columns <- ep_frame_data[which(ep_frame_data$score_ind == 0 & ep_frame_data$safety_ind == 0),] %>% 
    calculate_expected_points( "new_half_seconds_remaining",
                               "pred_yardline_100", "new_down", "new_ydstogo", "new_goal_to_go")
  
  # Columns that are unique to the EP model code:
  ep_only_col_names <- colnames(ep_columns)[str_detect(colnames(ep_columns), 
                                                       "(_prob)|(^ep)")]
  ep_frame_data <- ep_frame_data %>%
    bind_cols(map_dfc(1:length(ep_only_col_names),
                      function(x) rep(0, nrow(ep_frame_data))))
  colnames(ep_frame_data)[(ncol(ep_frame_data) - length(ep_only_col_names) + 1):(ncol(ep_frame_data))] <- ep_only_col_names
  ep_frame_data[which(ep_frame_data$score_ind == 0 & ep_frame_data$safety_ind == 0), ep_only_col_names] <- ep_columns[, ep_only_col_names]
  # Now for the modify the TD and safety rows:
  ep_frame_data <- ep_frame_data %>%
    mutate(td_prob = ifelse(score_ind == 1, 1, td_prob),
           ep = ifelse(score_ind == 1, 7, ep),
           opp_safety_prob = ifelse(safety_ind == 1, 1,  opp_safety_prob),
           ep = ifelse(safety_ind == 1, -2, ep))
  
  # Finally return the dataset with the WP columns:
  ep_frame_data %>%
    calculate_win_probability("new_half_seconds_remaining", "new_game_seconds_remaining",
                              "score_differential", "qtr", "posteam_timeouts_remaining",
                              "defteam_timeouts_remaining", "ep")
  
}

# Now join the EP and WP columns to the dataset:
lstm_lowo_preds_joined_data <- add_ep_wp_columns(lstm_lowo_preds_joined_data)

#all_model_lowo_preds_joined_data <- read_csv("src/modeling/lowo_model_datasets/lowo_model_adj_xydir_ep_wp_preds_1023.csv")
# Okay now each frame in this dataset has EP and WP so the final thing is to calculate
# the EPA and WPA with respect to the current ball_carrier_sequence
compute_epa_wpa <- . %>%
  group_by(bc_sequence_id, model_type) %>%
  mutate(epa = ep - lag(ep),
         wpa = wp - lag(wp))

lstm_lowo_preds_joined_data <- lstm_lowo_preds_joined_data %>%
  compute_epa_wpa

# Save this file:
write_csv(lstm_lowo_preds_joined_data, "src/modeling/lowo_model_datasets/lstm_lowo_model_adj_xydir_ep_wp_preds_1023.csv")

play_above_handoff_summary <- lstm_lowo_preds_joined_data %>%
  #mutate(one_second_mark = ifelse(bc_frame_id == 10, 1, 0)) %>%
  group_by(gameId, playId) %>%
  summarize(player_id = first(nflId),
            pred_yards_gained_handoff = first(yardline_100) - first(init_pred_yardline_100),
            #pred_yards_gained_second = first(yardline_100) - init_pred_yardline_100[which(one_second_mark == 1)],
            actual_yards_gained = first(actual_yards_gained),
            pred_epa_handoff = first(ep) - first(start_ep),
            actual_epa = first(total_play_epa),
            pred_wpa_handoff = first(wp) - first(start_wp),
            actual_wpa = first(total_play_wpa),
            target_x = first(target_x))

# Join the fumble column from:
play_above_handoff_summary <- play_above_handoff_summary %>%
  left_join({(
    nflscrapr_pbp_17 %>%
      dplyr::select(game_id, play_id, fumble, down, ydstogo)
  )}, by = c("gameId" = "game_id", "playId" = "play_id"))

player_handoff_summary <- play_above_handoff_summary %>%
  # Remove fumbles:
  #filter(fumble == 0) %>%
  group_by(player_id) %>%
  summarize(ave_yds_above_handoff = mean(actual_yards_gained - pred_yards_gained_handoff,
                                         na.rm = TRUE),
            ave_epa_above_handoff = mean(actual_epa - pred_epa_handoff,
                                         na.rm = TRUE),
            ave_wpa_above_handoff = mean(actual_wpa - pred_wpa_handoff,
                                         na.rm = TRUE),
            sum_yds_above_handoff = sum(actual_yards_gained - pred_yards_gained_handoff,
                                        na.rm = TRUE),
            sum_epa_above_handoff = sum(actual_epa - pred_epa_handoff,
                                        na.rm = TRUE),
            sum_wpa_above_handoff = sum(actual_wpa - pred_wpa_handoff,
                                        na.rm = TRUE),
            total_yards_gained = sum(actual_yards_gained, na.rm = TRUE),
            ave_yards_gained = mean(actual_yards_gained, na.rm = TRUE),
            n_carries = n())

# Load the player data to join:
players_data <- read_csv("big_data_bowl_repo/Data/players.csv")

player_handoff_summary <- player_handoff_summary %>%
  left_join(players_data,
            by = c("player_id" = "nflId")) %>%
  arrange(desc(sum_yds_above_handoff))

# Plot players with at least 20 carries yards gained over expectation at
# handoff:
library(ggrepel)
player_handoff_summary %>%
  mutate(player_name = paste0(str_sub(FirstName, 1, 1), ". ", LastName)) %>%
  filter(n_carries >= 20) %>%
  ggplot(aes(y = ave_yards_gained, x = ave_yds_above_handoff, label = player_name,
             color = PositionAbbr)) +
  geom_point(alpha = 0.75) +
  geom_text_repel() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  scale_color_manual(values = c("darkgreen", "darkorange", "darkblue",
                                "black")) +
  #ggthemes::scale_color_colorblind() +
  labs(y = "Average yards gained per carry",
       x = "Average yards gained above expectation at handoff per carry",
       color = "Position",
       title = "Joint distribution of average yards gained per carry and yards gained above expectation at handoff per carry",
       subtitle = "Minimum of 20 carries in first six weeks of the 2017 NFL season"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

play_above_second_summary <- lstm_lowo_preds_joined_data %>%
  mutate(one_second_mark = ifelse(bc_frame_id == 10, 1, 0)) %>%
  group_by(gameId, playId) %>%
  filter(any(bc_frame_id >= 10)) %>%
  summarize(player_id = first(nflId),
            pred_yards_gained_second = first(yardline_100) - init_pred_yardline_100[which(one_second_mark == 1)],
            actual_yards_gained = first(actual_yards_gained),
            target_x = first(target_x))

# Join the fumble column from:
play_above_second_summary <- play_above_second_summary %>%
  left_join({(
    nflscrapr_pbp_17 %>%
      dplyr::select(game_id, play_id, fumble, down, ydstogo)
  )}, by = c("gameId" = "game_id", "playId" = "play_id"))

player_above_second_summary <- play_above_second_summary %>%
  # Remove fumbles:
  #filter(fumble == 0) %>%
  group_by(player_id) %>%
  summarize(ave_yds_above_second = mean(actual_yards_gained - pred_yards_gained_second,
                                         na.rm = TRUE),
            total_yards_gained = sum(actual_yards_gained, na.rm = TRUE))

# Join the handoff data ave_yards_gained:
player_above_second_summary <- player_above_second_summary %>%
  left_join(dplyr::select(player_handoff_summary, player_id, ave_yards_gained, n_carries, ave_yds_above_handoff),
            by = "player_id")

player_above_second_summary <- player_above_second_summary %>%
  left_join(players_data,
            by = c("player_id" = "nflId")) %>%
  arrange(desc(ave_yds_above_second))

# Plot players with at least 20 carries yards gained over expectation at
# handoff:
library(ggrepel)
player_above_second_summary %>%
  mutate(player_name = paste0(str_sub(FirstName, 1, 1), ". ", LastName)) %>%
  filter(n_carries >= 20) %>%
  ggplot(aes(x = ave_yds_above_handoff, y = ave_yds_above_second, label = player_name,
             color = PositionAbbr)) +
  geom_point(alpha = 0.75) +
  geom_text_repel() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  scale_color_manual(values = c("darkgreen", "darkorange", "darkblue",
                                "black")) +
  #ggthemes::scale_color_colorblind() +
  labs(x = "Average yards gained above expectation at handoff",
       y = "Average yards gained above expectation at 1 second into carry",
       color = "Position",
       title = "Joint distribution of average yards above expectation per carry at handoff and 1 second into carry",
       subtitle = "Minimum of 20 carries in first six weeks of the 2017 NFL season"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")
