# PURPOSE: Create a figure summarizing the LOWO CV performance across the
#          baseline and RFCDE models with different sets of covariates

library(tidyverse)
library(ggbeeswarm)
library(patchwork)

# Load the LOWO CV results ------------------------------------------------

# Baseline:
lowo_cv_summary_np <-
  read_csv("data/model_output/lowo_cv_results/plugin_npcde_crps_summary.csv")

# RFCDE:
lowo_cv_summary_rfcde <-
  read_csv("data/model_output/lowo_cv_results/rfcde_crps_summary_with_abs_val.csv")


# View the results --------------------------------------------------------

# Want the order of the results to go from baseline npcde, to reciever and play
# context, then with incremental number of players included - with defense only
# versus defense and offense side by side

# First update the np results
lowo_cv_summary_np <- lowo_cv_summary_np %>%
  mutate(n_close_players = -1,
         type = "Baseline")
# Next make a version of just the receiver and play-level context:
lowo_cv_summary_rec_play <- lowo_cv_summary_rfcde %>%
  filter(n_close_players == 2) %>%
  mutate(n_close_players = 0,
         type = "Receiver and play-level")
# Now a version separating defender and offense included:
lowo_cv_summary_def_off <- lowo_cv_summary_rfcde %>%
  filter(n_close_players > 2) %>%
  mutate(n_close_players = n_close_players - 2,
         type = ifelse(n_close_players < 5, "defense",
                       "defense and offense"),
         n_close_players = ifelse(n_close_players < 5, n_close_players,
                                  n_close_players - 4))

# Now generate the results:
crps_plot <- lowo_cv_summary_np %>%
  bind_rows(lowo_cv_summary_rec_play) %>%
  ggplot(aes(x = n_close_players, y = test_crps)) +
  geom_beeswarm(color = "black", alpha = 0.8) +
  stat_summary(fun = "mean", color = "red", size = 3, geom = "point") +
  geom_beeswarm(data = lowo_cv_summary_def_off,
                aes(x = n_close_players, y = test_crps, color = type),
                dodge.width = .75, alpha = 0.8) +
  geom_point(data = {
    lowo_cv_summary_def_off %>%
      group_by(n_close_players, type) %>%
      summarize(mean_crps = mean(test_crps))
  }, aes(x = n_close_players, y = mean_crps), color = "red", size = 3,
  position = position_dodge2(width = .75)) +
  scale_color_manual(values = c("darkblue", "darkorange")) +
  scale_x_continuous(breaks = -1:4, labels = c("Baseline", as.character(0:4))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Number of additional players included by distance",
       y = "Holdout CRPS",
       color = "Types of additional players:")

rmse_plot <- lowo_cv_summary_np %>%
  bind_rows(lowo_cv_summary_rec_play) %>%
  ggplot(aes(x = n_close_players, y = test_yac_rmse)) +
  geom_beeswarm(color = "black", alpha = 0.8) +
  stat_summary(fun = "mean", color = "red", size = 3, geom = "point") +
  geom_beeswarm(data = lowo_cv_summary_def_off,
                aes(x = n_close_players, y = test_yac_rmse, color = type),
                dodge.width = .75, alpha = 0.8) +
  geom_point(data = {
    lowo_cv_summary_def_off %>%
      group_by(n_close_players, type) %>%
      summarize(mean_rmse = mean(test_yac_rmse))
  }, aes(x = n_close_players, y = mean_rmse), color = "red", size = 3,
  position = position_dodge2(width = .75)) +
  scale_color_manual(values = c("darkblue", "darkorange")) +
  scale_x_continuous(breaks = -1:4, labels = c("Baseline", as.character(0:4))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Number of additional players included by distance",
       y = "Holdout RMSE",
       color = "Types of additional players:")

new_crps_plot <- crps_plot + theme(legend.position = "none",
                                   axis.text.x = element_blank(),
                                   axis.title.x = element_blank(),
                                   axis.ticks.x = element_blank())

(new_crps_plot / rmse_plot) +
  plot_annotation(title = "Observe best holdout performance using RFCDE with two closest defenders",
                  subtitle = "Individual points denote holdout weeks; red points denote mean values")


