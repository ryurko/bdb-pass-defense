# PURPOSE: Create an initial version of the RFCDE model to use within the discussion
#          section of the paper, pointing towards the direction of future work
#          that is necessary to implement continuous-time EP/WP models with
#          our framework. After fitting the model (with default settings) then
#          create figures displaying density curves on the field. Then finally
#          integrate over the associated expected points and win probability 
#          values with their respective density estimates to create EP and WP
#          curves.

library(tidyverse)
library(cowplot)
library(RFCDE)

# Load the previously constructed model dataset:
model_dataset_adj_xy <- read_csv("src/modeling/lowo_model_datasets/model_dataset_no_lag_adj_xydir_1022.csv")

# Create the new vector of covariate names:
adj_covariate_names <- 
  str_subset(colnames(model_dataset_adj_xy),
             "((_dis)|(_s)|(_dist_to_ball)|(_change)|(_adj)|(_target_endzone)|(_wrt_bc_diff)|(_area)|(area_in_front)|(_bubble)|(_area_in_front_bc_only))$")
adj_covariate_names <- adj_covariate_names[-which(adj_covariate_names == "field_x_change")]

# Get the data covariates
model_data_cov <- as.matrix(model_dataset_adj_xy[,adj_covariate_names])

# Create scaled version:
scaled_model_data_cov <- scale(model_data_cov)
model_data_y <- model_dataset_adj_xy$field_x_change

# Fit the model with the preset defaults for now:
init_rfcde_fit <- RFCDE(scaled_model_data_cov, model_data_y)

# So saving as RDS does not work unfortunately...
#saveRDS(init_rfcde_fit, "src/modeling/init_rfcde_fit_adj_xydir.rds")
#saveRDS(init_rfcde_fit, "src/modeling/upd_rfcde_fit_adj_xydir.rds")

#init_rfcde_fit <- readRDS("src/modeling/init_rfcde_fit_adj_xydir.rds")
#init_rfcde_fit <- readRDS("src/modeling/upd_rfcde_fit_adj_xydir.rds")

# Next - need to generate the example figures using the RF CDE for the Patterson
# TD run play.

# Load the tracking dataset for the example run:

lstm_lowo_preds_joined_data <- read_csv("src/modeling/lowo_model_datasets/lstm_lowo_model_adj_xydir_ep_wp_preds_1023.csv")
# Find the Patterson TD run
long30_td_runs <- lstm_lowo_preds_joined_data %>%
  filter(score_ind == 1,
         actual_yards_gained >= 30) %>%
  distinct(gameId, playId, desc)

# Load the tracking data for this game (Patterson run is the 25th row of this data):
ex_file_tracking <- paste0("big_data_bowl_repo/Data/tracking_gameId_", 
                           long30_td_runs$gameId[25], ".csv")
tracking_example <- read_csv(ex_file_tracking)

# Now join the ep_frame_data:
tracking_example <- tracking_example %>%
  left_join({(lstm_lowo_preds_joined_data %>%
                inner_join(dplyr::select(model_dataset_adj_xy, gameId, playId, bc_frame_id, frame.id),
                           by = c("gameId", "playId", "bc_frame_id")))}, 
            by = c("gameId", "playId", "frame.id"))

# Only use the play of interest:
tracking_example <- tracking_example %>%
  filter(playId == long30_td_runs$playId[25])


# The following code grabs snapshots of the play
# Make an indicator denoting the ball carrier so they are a separate color:
tracking_example <- tracking_example %>%
  mutate(is_bc = ifelse(nflId.x == nflId.y & !is.na(nflId.y) & !is.na(nflId.x),
                        "bc", "other"),
         team = ifelse(is_bc == "other", team,
                       is_bc)) 

# What is the game and play id for this play?
ex_game_id <- tracking_example$gameId[1]
ex_play_id <- tracking_example$playId[1]

# Find which rows of the modeling dataset are this play:
patterson_play_rows_i <- which(model_dataset_adj_xy$gameId == ex_game_id &
                                 model_dataset_adj_xy$playId == ex_play_id)
# What's the maximum possible distance traveled for this run:
max(model_dataset_adj_xy$field_x_change[patterson_play_rows_i])
# [1] 52.27
# So for ease - let's set-up a grid of -5 to 55 at 1 yard increments
z_predict_grid <- seq(-5, 55, by = 1)
# THIS SHOULD BE ADJUSTED DEPENDING ON THE PLAY

# Set-up a dataset with lines at every point 0 to 1 with density values
#bandwidth <- 0.2
#n_grid <- 100
#z_grid <- seq(-10, 60, length.out = 70)

# Next loop through the Patterson play rows, generating the CDE predictions for frame
patterson_play_density_values <- 
  map_dfr(patterson_play_rows_i, 
          function(i) {
            x_test <- scaled_model_data_cov[i, ]
            density <- predict(init_rfcde_fit, x_test, "CDE", z_predict_grid)
            data.frame(pred_bc_x = model_dataset_adj_xy$bc_x[i] - z_predict_grid - 10, # since target_x == 0
                       pred_bc_density = as.numeric(density)) %>%
            mutate(pred_bc_x = ifelse(pred_bc_x < 0, 0, pred_bc_x) + 10,
                   frame.id = model_dataset_adj_xy$frame.id[i]) %>%
            group_by(frame.id, pred_bc_x) %>%
            summarize(pred_bc_density = sum(pred_bc_density, na.rm = TRUE)) %>%
            ungroup()
            })

# Join this to the tracking example data as a new version
tracking_example_rfcde <- tracking_example %>%
  dplyr::select(-pred_bc_x) %>%
  left_join(patterson_play_density_values, by = "frame.id")
write_csv(tracking_example_rfcde,
          "data/examples/patterson_td_run_rfcde_data.csv")
tracking_example_rfcde <- 
  read_csv("data/examples/patterson_td_run_rfcde_data.csv")

# Use the Lopez code:
# General field boundaries
xmin <- 0
xmax <- 160/3
hash_right <- 38.35
hash_left <- 12
hash_width <- 3.3

## Specific boundaries for a given play
ymin <- 0 #max(round(min(tracking_example2$x, na.rm = TRUE) - 10, -1), 0)
ymax <- 120 #min(round(max(tracking_example2$x, na.rm = TRUE) + 10, -1), 120)
df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
df_hash <- df_hash %>% filter(y < ymax, y > ymin)

# Let's first make an animated version to see it works:
library(gganimate)
animate_play_rfcde <- ggplot() +
  annotate("text", y = df_hash$x[df_hash$x < 55/2], 
           x = df_hash$y[df_hash$x < 55/2], label = "|", vjust = -0.3, hjust = 0.4) + 
  annotate("text", y = df_hash$x[df_hash$x > 55/2], 
           x = df_hash$y[df_hash$x > 55/2], label = "|", vjust = 1, hjust = 0.4) + 
  annotate("segment", y = xmin, 
           x = seq(max(10, ymin), min(ymax, 110), by = 5), 
           yend =  xmax, 
           xend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", y = rep(hash_left, 11), x = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 0, size = 4) + 
  annotate("text", y = rep((xmax - hash_left), 11), x = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 180, size = 4) + 
  annotate("segment", y = c(xmin, xmin, xmax, xmax), 
           x = c(ymin, ymax, ymax, ymin), 
           yend = c(xmin, xmax, xmax, xmin), 
           xend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  geom_segment(data = tracking_example_rfcde,
               aes(x = pred_bc_x, xend = pred_bc_x, 
                   y = xmin, yend = xmax,
                   alpha = pred_bc_density),
               size = 2, color = "red") +
  # geom_segment(data = tracking_example2,
  #              aes(x = pred_bc_x, xend = pred_bc_x, y = xmin, yend = xmax),
  #              color = "red", size = 2) +
  geom_point(data = filter(tracking_example_rfcde), alpha = 0.75,
             aes(y = (y), x = x, colour = team, fill = team, alpha = team,
                 group = nflId.x, pch = team, size = team)) +
  scale_size_manual(values = c(6, 3,
                               8, 6), guide = FALSE) +
  scale_shape_manual(values = c(21, 16,
                                21, 21), guide = FALSE) +
  scale_colour_manual(values = c("white", "#654321",
                                 "black", "white"), guide = FALSE) +
  scale_fill_manual(values = c("darkorange", "#654321",
                               "black", "blue")) +
  # scale_alpha_manual(values = c(0.75, 0.75, 
  #                               1, 0.75), guide = FALSE) + 
  scale_alpha_continuous(range = c(0.001, .5)) +
  #scale_color_gradient(low = "white", high = "red") +
  xlim(ymin, ymax) + 
  coord_fixed() +  
  theme_nothing() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  transition_time(frame.id)  +
  ease_aes('linear') #+ 
#NULL
play_length <- length(unique(tracking_example_rfcde$frame.id))
animate(animate_play_rfcde, fps = 10, nframe = play_length)
play_rfcde_gif <- animate(animate_play_rfcde, fps = 10, nframe = play_length,
                          width = 600, height = 250)

# Looks great! 

# Ok let's take snapshots of these - such as the first contact frame id:
contact_frame_id <- tracking_example %>%
  filter(event == "first_contact") %>%
  pull(frame.id) %>%
  .[1]

contact_rfcde_plot <- ggplot() +
  annotate("text", y = df_hash$x[df_hash$x < 55/2], 
                                 x = df_hash$y[df_hash$x < 55/2], label = "|", vjust = -0.3, hjust = 0.4) + 
  annotate("text", y = df_hash$x[df_hash$x > 55/2], 
           x = df_hash$y[df_hash$x > 55/2], label = "|", vjust = 1, hjust = 0.4) + 
  annotate("segment", y = xmin, 
           x = seq(max(10, ymin), min(ymax, 110), by = 5), 
           yend =  xmax, 
           xend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", y = rep(hash_left, 11), x = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 0, size = 4) + 
  annotate("text", y = rep((xmax - hash_left), 11), x = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 180, size = 4) + 
  annotate("segment", y = c(xmin, xmin, xmax, xmax), 
           x = c(ymin, ymax, ymax, ymin), 
           yend = c(xmin, xmax, xmax, xmin), 
           xend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  geom_segment(data = filter(tracking_example_rfcde, frame.id == contact_frame_id),
               aes(x = pred_bc_x, xend = pred_bc_x, 
                   y = xmin, yend = xmax,
                   alpha = pred_bc_density),
               size = 2, color = "red") +
  geom_point(data = filter(tracking_example_rfcde, frame.id == contact_frame_id), alpha = 0.75,
             aes(y = (y), x = x, colour = team, fill = team, alpha = team,
                 group = nflId.x, pch = team, size = team)) +
  scale_size_manual(values = c(6, 3,
                               8, 6), guide = FALSE) +
  scale_shape_manual(values = c(21, 16,
                                21, 21), guide = FALSE) +
  scale_colour_manual(values = c("white", "#654321",
                                 "black", "white"), guide = FALSE) +
  scale_fill_manual(values = c("darkorange", "#654321",
                               "black", "blue")) +
  scale_alpha_continuous(range = c(0.001, .5)) +
  xlim(ymin, ymax) + 
  coord_fixed() +  
  theme_nothing() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


# Now make the density curve of the field at this point as well:
rfcde_density_curve <- ggplot() +
  geom_line(data = filter(patterson_play_density_values, frame.id == contact_frame_id),
            aes(x = pred_bc_x -10, y = (pred_bc_density)),
            color = "red") +
  theme_minimal() +
  labs(x = "Yards from target endzone",
       y = "Conditional density estimate") +
  scale_x_continuous(limits = c(-10, 120),
                     breaks = seq(0, 100, by = 10)) 
# Stack the two to make a single plot
plot_grid(contact_rfcde_plot, rfcde_density_curve,
          ncol = 1)

# Next - need to compute the expected points and win probability for each yardline,
# to then integrate over to compute RF-CDE EP and WP values to make a smooth chart.

library(nflscrapR)
predict_ep_wp <- function(pred_bc_x_vector, init_context_df, time_adj_sec){
  ep_frame_data <- map_dfr(pred_bc_x_vector,
                           function(pred_bc_x) {
                             init_patterson_td_run_context %>%
                               mutate(pred_yardline_100 = ifelse(target_x == 120,
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
                               mutate(new_half_seconds_remaining = ifelse(half_seconds_remaining - 
                                                                            time_adj_sec >= 0,
                                                                          half_seconds_remaining - 
                                                                            time_adj_sec,
                                                                          0),
                                      new_game_seconds_remaining = ifelse(game_seconds_remaining - 
                                                                            time_adj_sec >= 0,
                                                                          game_seconds_remaining - 
                                                                            time_adj_sec,
                                                                          0))
                           })
    
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
                              "defteam_timeouts_remaining", "ep") %>%
    dplyr::select(ep, wp) %>%
    return
}

# Make the patterson run initial context data frame:
not_na_rows <- which(!is.na(tracking_example$target_x))
init_patterson_td_run_context <- 
  data.frame(target_x = tracking_example$target_x[not_na_rows[1]],
             yardline_100 = tracking_example$yardline_100[not_na_rows[1]],
             ydstogo = tracking_example$ydstogo[not_na_rows[1]],
             down = tracking_example$down[not_na_rows[1]],
             half_seconds_remaining = tracking_example$half_seconds_remaining[not_na_rows[1]],
             game_seconds_remaining = tracking_example$game_seconds_remaining[not_na_rows[1]],
             goal_to_go = tracking_example$goal_to_go[not_na_rows[1]],
             score_differential = tracking_example$score_differential[not_na_rows[1]],
             qtr = tracking_example$qtr[not_na_rows[1]], 
             posteam_timeouts_remaining = tracking_example$posteam_timeouts_remaining[not_na_rows[1]],
             defteam_timeouts_remaining = tracking_example$defteam_timeouts_remaining[not_na_rows[1]])

patterson_time_adj_sec <- max(tracking_example$bc_frame_id, na.rm = TRUE) / 10

patterson_play_density_values <- patterson_play_density_values %>%
  bind_cols(predict_ep_wp(patterson_play_density_values$pred_bc_x, 
                          init_patterson_td_run_context,
                          patterson_time_adj_sec))

write_csv(patterson_play_density_values,
          "data/examples/patterson_td_run_rfcde_ep_wp_data.csv")

    
# Next summarize each frame to compute the expected value for EP and WP - just
# normalize the densities to sum up to 1 for ease of calculation:

patterson_play_within_play_value <- patterson_play_density_values %>%
  group_by(frame.id) %>%
  mutate(total_density = sum(pred_bc_density)) %>%
  summarize(exp_ep = sum(pred_bc_density / total_density * ep),
            exp_wp = sum(pred_bc_density / total_density * wp))

# join these columns:
tracking_example_ep_wp <- tracking_example %>%
  left_join(patterson_play_within_play_value, by = "frame.id")

# Now just need to join this to the data with the events to create the EP/WP
# charts over the course of the TD run:
ep_play_plot_data <- tracking_example_ep_wp %>%
  filter(nflId.x == tracking_example_ep_wp$nflId.y[which(!is.na(tracking_example_ep_wp$nflId.y))[1]]) %>%
  mutate(event = ifelse(is.na(event), "", event)) %>%
  # Plug in the starting EP values for all previous moments
  mutate(exp_ep = ifelse(is.na(exp_ep) & frame.id <= tracking_example_ep_wp$frame.id[which(!is.na(tracking_example_ep_wp$nflId.y))[1]],
                         tracking_example_ep_wp$start_ep[which(!is.na(tracking_example_ep_wp$nflId.y))[1]],
                     exp_ep),
         exp_ep = ifelse(is.na(exp_ep) & frame.id >= tracking_example_ep_wp$frame.id[which(!is.na(tracking_example_ep_wp$event == "touchdown"))[1]],
                     #max(tracking_example_ep_wp$ep, na.rm = TRUE),
                     7,
                     exp_ep))
library(ggrepel)
ep_play_plot <-  ep_play_plot_data %>%
  ggplot(aes(x = frame.id, y = exp_ep)) +
  geom_point(data = filter(ep_play_plot_data,
                           event != "")) +
  geom_text_repel(aes(label = event), colour = "black", size = 3) + 
  geom_line(color = "darkblue") +
  geom_hline(yintercept = 0,
             linetype = "dotted", color = "darkred", size = 1) +
  theme_bw() + 
  labs(x = "Time into play (10Hz)",
       y = "Expected points",
       title = "Change in expected points during Cordarrelle Patterson's 47 yard TD run")

wp_play_plot_data <- tracking_example_ep_wp %>%
  filter(nflId.x == tracking_example_ep_wp$nflId.y[which(!is.na(tracking_example_ep_wp$nflId.y))[1]]) %>%
  mutate(event = ifelse(is.na(event), "", event)) %>%
  # Plug in the starting EP values for all previous moments
  mutate(exp_wp = ifelse(is.na(exp_wp) & frame.id <= tracking_example_ep_wp$frame.id[which(!is.na(tracking_example_ep_wp$nflId.y))[1]],
                     tracking_example_ep_wp$start_wp[which(!is.na(tracking_example_ep_wp$nflId.y))[1]],
                     exp_wp),
         exp_wp = ifelse(is.na(exp_wp) & frame.id >= tracking_example_ep_wp$frame.id[which(!is.na(tracking_example_ep_wp$event == "touchdown"))[1]],
                     max(tracking_example_ep_wp$exp_wp, na.rm = TRUE),
                     exp_wp))
wp_play_plot <- wp_play_plot_data %>%
  ggplot(aes(x = frame.id, y = exp_wp)) +
  geom_point(data = filter(wp_play_plot_data,
                           event != "")) +
  geom_text_repel(aes(label = event), colour = "black", size = 3) + 
  geom_line(color = "darkblue") +
  geom_hline(yintercept = 0.5,
             linetype = "dotted", color = "darkred", size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() + 
  labs(x = "Time into play (10Hz)",
       y = "Win probability",
       title = "Change in win probability during Cordarrelle Patterson's 47 yard TD run")

plot_grid(ep_play_plot, wp_play_plot,
          ncol = 1, labels = c("(A)", "(B)"), align = "hv")





# Fournette TD run --------------------------------------------------------



# Load the tracking data for this game (Patterson run is the 25th row of this data):
ex_file_tracking2 <- paste0("big_data_bowl_repo/Data/tracking_gameId_", 
                           long30_td_runs$gameId[24], ".csv")
tracking_example2 <- read_csv(ex_file_tracking2)

# Now join the ep_frame_data:
tracking_example2 <- tracking_example2 %>%
  left_join({(lstm_lowo_preds_joined_data %>%
                inner_join(dplyr::select(model_dataset_adj_xy, gameId, playId, bc_frame_id, frame.id),
                           by = c("gameId", "playId", "bc_frame_id")))}, 
            by = c("gameId", "playId", "frame.id"))

# Only use the play of interest:
tracking_example2 <- tracking_example2 %>%
  filter(playId == long30_td_runs$playId[24])


# The following code grabs snapshots of the play
# Make an indicator denoting the ball carrier so they are a separate color:
tracking_example2 <- tracking_example2 %>%
  mutate(is_bc = ifelse(nflId.x == nflId.y & !is.na(nflId.y) & !is.na(nflId.x),
                        "bc", "other"),
         team = ifelse(is_bc == "other", team,
                       is_bc)) 

# What is the game and play id for this play?
ex_game_id2 <- tracking_example2$gameId[1]
ex_play_id2 <- tracking_example2$playId[1]

# Find which rows of the modeling dataset are this play:
lf_play_rows_i <- which(model_dataset_adj_xy$gameId == ex_game_id2 &
                                 model_dataset_adj_xy$playId == ex_play_id2)
# What's the maximum possible distance traveled for this run:
max(model_dataset_adj_xy$field_x_change[lf_play_rows_i])
# [1] 79.67
# So for ease - let's set-up a grid of -5 to 80 at 1 yard increments
z_predict_grid2 <- seq(-5, 80, by = 1)
# THIS SHOULD BE ADJUSTED DEPENDING ON THE PLAY


# Next loop through the Fournette play rows, generating the CDE predictions for frame
lf_play_density_values <- 
  map_dfr(lf_play_rows_i, 
          function(i) {
            x_test <- scaled_model_data_cov[i, ]
            density <- predict(init_rfcde_fit, x_test, "CDE", z_predict_grid2)
            data.frame(pred_bc_x = model_dataset_adj_xy$bc_x[i] - z_predict_grid2 - 10, # since target_x == 0
                       pred_bc_density = as.numeric(density)) %>%
              mutate(pred_bc_x = ifelse(pred_bc_x < 0, 0, pred_bc_x) + 10,
                     frame.id = model_dataset_adj_xy$frame.id[i]) %>%
              group_by(frame.id, pred_bc_x) %>%
              summarize(pred_bc_density = sum(pred_bc_density, na.rm = TRUE)) %>%
              ungroup()
          })

# Join this to the tracking example data as a new version
tracking_example_rfcde2 <- tracking_example2 %>%
  dplyr::select(-pred_bc_x) %>%
  left_join(lf_play_density_values, by = "frame.id")
write_csv(tracking_example_rfcde2,
          "data/examples/fournette_td_run_rfcde_data.csv")


# Use the Lopez code:
# General field boundaries
xmin <- 0
xmax <- 160/3
hash_right <- 38.35
hash_left <- 12
hash_width <- 3.3

## Specific boundaries for a given play
ymin <- 0 #max(round(min(tracking_example2$x, na.rm = TRUE) - 10, -1), 0)
ymax <- 120 #min(round(max(tracking_example2$x, na.rm = TRUE) + 10, -1), 120)
df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
df_hash <- df_hash %>% filter(y < ymax, y > ymin)

animate_play_rfcde2 <- ggplot() +
  annotate("text", y = df_hash$x[df_hash$x < 55/2], 
           x = df_hash$y[df_hash$x < 55/2], label = "|", vjust = -0.3, hjust = 0.4) + 
  annotate("text", y = df_hash$x[df_hash$x > 55/2], 
           x = df_hash$y[df_hash$x > 55/2], label = "|", vjust = 1, hjust = 0.4) + 
  annotate("segment", y = xmin, 
           x = seq(max(10, ymin), min(ymax, 110), by = 5), 
           yend =  xmax, 
           xend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", y = rep(hash_left, 11), x = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 0, size = 4) + 
  annotate("text", y = rep((xmax - hash_left), 11), x = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 180, size = 4) + 
  annotate("segment", y = c(xmin, xmin, xmax, xmax), 
           x = c(ymin, ymax, ymax, ymin), 
           yend = c(xmin, xmax, xmax, xmin), 
           xend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  geom_segment(data = tracking_example_rfcde2,
               aes(x = pred_bc_x, xend = pred_bc_x, 
                   y = xmin, yend = xmax,
                   alpha = pred_bc_density),
               size = 2, color = "red") +
  geom_point(data = filter(tracking_example_rfcde2), alpha = 0.75,
             aes(y = (y), x = x, colour = team, fill = team, alpha = team,
                 group = nflId.x, pch = team, size = team)) +
  scale_size_manual(values = c(6, 3,
                               8, 6), guide = FALSE) +
  scale_shape_manual(values = c(21, 16,
                                21, 21), guide = FALSE) +
  scale_colour_manual(values = c("white", "#654321",
                                 "black", "white"), guide = FALSE) +
  scale_fill_manual(values = c("darkorange", "#654321",
                               "black", "blue")) +
  scale_alpha_continuous(range = c(0.001, .5)) +
  xlim(ymin, ymax) + 
  coord_fixed() +  
  theme_nothing() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  transition_time(frame.id)  +
  ease_aes('linear') 
play_length2 <- length(unique(tracking_example_rfcde2$frame.id))
animate(animate_play_rfcde2, fps = 10, nframe = play_length2)
play_rfcde_gif2 <- animate(animate_play_rfcde2, fps = 10, nframe = play_length2,
                          width = 600, height = 250)

    
# Make the patterson run initial context data frame:
not_na_rows2 <- which(!is.na(tracking_example2$target_x))
init_lf_td_run_context <- 
  data.frame(target_x = tracking_example2$target_x[not_na_rows2[1]],
             yardline_100 = tracking_example2$yardline_100[not_na_rows2[1]],
             ydstogo = tracking_example2$ydstogo[not_na_rows2[1]],
             down = tracking_example2$down[not_na_rows2[1]],
             half_seconds_remaining = tracking_example2$half_seconds_remaining[not_na_rows2[1]],
             game_seconds_remaining = tracking_example2$game_seconds_remaining[not_na_rows2[1]],
             goal_to_go = tracking_example2$goal_to_go[not_na_rows2[1]],
             score_differential = tracking_example2$score_differential[not_na_rows2[1]],
             qtr = tracking_example2$qtr[not_na_rows2[1]], 
             posteam_timeouts_remaining = tracking_example2$posteam_timeouts_remaining[not_na_rows2[1]],
             defteam_timeouts_remaining = tracking_example2$defteam_timeouts_remaining[not_na_rows2[1]])

lf_time_adj_sec <- max(tracking_example2$bc_frame_id, na.rm = TRUE) / 10

lf_play_density_values <- lf_play_density_values %>%
  bind_cols(predict_ep_wp(lf_play_density_values$pred_bc_x, 
                          init_lf_td_run_context,
                          lf_time_adj_sec))

# Save this dataset
write_csv(lf_play_density_values,
          "data/examples/fournette_td_run_rfcde_ep_wp_data.csv")


# Next summarize each frame to compute the expected value for EP and WP - just
# normalize the densities to sum up to 1 for ease of calculation:

lf_play_within_play_value <- lf_play_density_values %>%
  group_by(frame.id) %>%
  mutate(total_density = sum(pred_bc_density)) %>%
  summarize(exp_ep = sum(pred_bc_density / total_density * ep),
            exp_wp = sum(pred_bc_density / total_density * wp))

# join these columns:
tracking_example_ep_wp2 <- tracking_example2 %>%
  left_join(lf_play_within_play_value, by = "frame.id")

# Now just need to join this to the data with the events to create the EP/WP
# charts over the course of the TD run:
ep_play_plot_data2 <- tracking_example_ep_wp2 %>%
  filter(nflId.x == tracking_example_ep_wp2$nflId.y[which(!is.na(tracking_example_ep_wp2$nflId.y))[1]]) %>%
  mutate(event = ifelse(is.na(event), "", event)) %>%
  # Plug in the starting EP values for all previous moments
  mutate(exp_ep = ifelse(is.na(exp_ep) & frame.id <= tracking_example_ep_wp2$frame.id[which(!is.na(tracking_example_ep_wp2$nflId.y))[1]],
                         tracking_example_ep_wp2$start_ep[which(!is.na(tracking_example_ep_wp2$nflId.y))[1]],
                         exp_ep),
         exp_ep = ifelse(is.na(exp_ep) & frame.id >= tracking_example_ep_wp2$frame.id[which(!is.na(tracking_example_ep_wp2$event == "touchdown"))[1]],
                         7,
                         exp_ep))
ep_play_plot2 <-  ep_play_plot_data2 %>%
  ggplot(aes(x = frame.id, y = exp_ep)) +
  geom_point(data = filter(ep_play_plot_data2,
                           event != "")) +
  geom_text_repel(aes(label = event), colour = "black", size = 3) + 
  geom_line(color = "darkblue") +
  geom_hline(yintercept = 0,
             linetype = "dotted", color = "darkred", size = 1) +
  theme_bw() + 
  labs(x = "Time into play (10Hz)",
       y = "Expected points",
       title = "Change in expected points during Fournette's 75 yard TD run")

wp_play_plot_data2 <- tracking_example_ep_wp2 %>%
  filter(nflId.x == tracking_example_ep_wp2$nflId.y[which(!is.na(tracking_example_ep_wp2$nflId.y))[1]]) %>%
  mutate(event = ifelse(is.na(event), "", event)) %>%
  # Plug in the starting EP values for all previous moments
  mutate(exp_wp = ifelse(is.na(exp_wp) & frame.id <= tracking_example_ep_wp2$frame.id[which(!is.na(tracking_example_ep_wp2$nflId.y))[1]],
                         tracking_example_ep_wp2$start_wp[which(!is.na(tracking_example_ep_wp2$nflId.y))[1]],
                         exp_wp),
         exp_wp = ifelse(is.na(exp_wp) & frame.id >= tracking_example_ep_wp2$frame.id[which(!is.na(tracking_example_ep_wp2$event == "touchdown"))[1]],
                         max(tracking_example_ep_wp2$exp_wp, na.rm = TRUE),
                         exp_wp))
wp_play_plot2 <- wp_play_plot_data2 %>%
  ggplot(aes(x = frame.id, y = exp_wp)) +
  geom_point(data = filter(wp_play_plot_data2,
                           event != "")) +
  geom_text_repel(aes(label = event), colour = "black", size = 3) + 
  geom_line(color = "darkblue") +
  geom_hline(yintercept = 0.5,
             linetype = "dotted", color = "darkred", size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() + 
  labs(x = "Time into play (10Hz)",
       y = "Win probability",
       title = "Change in win probability during Fournette's 47 yard TD run")

plot_grid(ep_play_plot, wp_play_plot,
          ncol = 1, labels = c("(A)", "(B)"), align = "hv")


