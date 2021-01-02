# PURPOSE: View the results for the LOWO CV RFCDE CRPS results

library(tidyverse)


# Load output -------------------------------------------------------------

lowo_cv_summary <-
  read_csv("data/model_output/lowo_cv_results/play_context_rfcde_crps_summary.csv")

lowo_cv_summary_new <-
  read_csv("data/model_output/lowo_cv_results/qb_play_context_rfcde_crps_summary.csv")

lowo_cv_summary_np <-
  read_csv("data/model_output/lowo_cv_results/plugin_npcde_crps_summary.csv")

lowo_cv_summary_no_abs <-
  read_csv("data/model_output/lowo_cv_results/rfcde_crps_summary_no_abs_val.csv")

lowo_cv_summary_with_abs <-
  read_csv("data/model_output/lowo_cv_results/rfcde_crps_summary_with_abs_val.csv")


# View the results --------------------------------------------------------

library(ggbeeswarm)
lowo_cv_summary %>%
  mutate(n_close_players = n_close_players - 1) %>%
  ggplot(aes(x = n_close_players, y = test_crps)) +
  geom_beeswarm(color = "darkblue") +
  stat_summary(fun = "mean", color = "darkorange", size = 3, geom = "point") +
  theme_bw() +
  labs(x = "Number of defense / offense players included by distance",
       y = "Holdout CRPS")

lowo_cv_summary %>%
  mutate(n_close_players = n_close_players - 1) %>%
  group_by(n_close_players) %>%
  summarize(med_crps = median(test_crps),
            mean_crps = mean(test_crps))

lowo_cv_summary_new %>%
  mutate(n_close_players = n_close_players - 1) %>%
  ggplot(aes(x = n_close_players, y = test_crps)) +
  geom_beeswarm(color = "darkblue") +
  stat_summary(fun = "mean", color = "darkorange", size = 3, geom = "point") +
  theme_bw() +
  labs(x = "Number of defense / offense players included by distance",
       y = "Holdout CRPS")

lowo_cv_summary_new %>%
  mutate(n_close_players = n_close_players - 1) %>%
  group_by(n_close_players) %>%
  summarize(med_crps = median(test_crps),
            mean_crps = mean(test_crps))

lowo_cv_summary_new %>%
  mutate(n_close_players = n_close_players - 1) %>%
  ggplot(aes(x = n_close_players, y = test_yac_rmse)) +
  geom_beeswarm(color = "darkblue") +
  stat_summary(fun = "mean", color = "darkorange", size = 3, geom = "point") +
  theme_bw() +
  labs(x = "Number of defense / offense players included by distance",
       y = "Holdout RMSE")


lowo_cv_summary_np %>%
  mutate(n_close_players = 0) %>%
  bind_rows(lowo_cv_summary_new) %>%
  mutate(n_close_players = n_close_players - 1) %>%
  ggplot(aes(x = n_close_players, y = test_crps)) +
  geom_beeswarm(color = "darkblue") +
  stat_summary(fun = "mean", color = "darkorange", size = 3, geom = "point") +
  theme_bw() +
  labs(x = "Number of defense / offense players included by distance",
       y = "Holdout CRPS")

lowo_cv_summary_np %>%
  mutate(n_close_players = 0) %>%
  bind_rows(lowo_cv_summary_new) %>%
  mutate(n_close_players = n_close_players - 1) %>%
  ggplot(aes(x = n_close_players, y = test_yac_rmse)) +
  geom_beeswarm(color = "darkblue") +
  stat_summary(fun = "mean", color = "darkorange", size = 3, geom = "point") +
  theme_bw() +
  labs(x = "Number of defense / offense players included by distance",
       y = "Holdout YAC RMSE")


lowo_cv_summary_np %>%
  mutate(n_close_players = 0) %>%
  bind_rows(lowo_cv_summary_no_abs) %>%
  mutate(n_close_players = n_close_players - 2) %>%
  ggplot(aes(x = n_close_players, y = test_crps)) +
  geom_beeswarm(color = "darkblue") +
  stat_summary(fun = "mean", color = "darkorange", size = 3, geom = "point") +
  theme_bw() +
  labs(x = "Number of defense / offense players included by distance",
       y = "Holdout CRPS")

lowo_cv_summary_np %>%
  mutate(n_close_players = 0) %>%
  bind_rows(lowo_cv_summary_no_abs) %>%
  mutate(n_close_players = n_close_players - 2) %>%
  ggplot(aes(x = n_close_players, y = test_yac_rmse)) +
  geom_beeswarm(color = "darkblue") +
  stat_summary(fun = "mean", color = "darkorange", size = 3, geom = "point") +
  theme_bw() +
  labs(x = "Number of defense / offense players included by distance",
       y = "Holdout YAC RMSE")


lowo_cv_summary_np %>%
  mutate(n_close_players = 0) %>%
  bind_rows(lowo_cv_summary_with_abs) %>%
  mutate(n_close_players = n_close_players - 2) %>%
  ggplot(aes(x = n_close_players, y = test_crps)) +
  geom_beeswarm(color = "darkblue") +
  stat_summary(fun = "mean", color = "darkorange", size = 3, geom = "point") +
  theme_bw() +
  labs(x = "Number of defense / offense players included by distance",
       y = "Holdout CRPS")

lowo_cv_summary_np %>%
  mutate(n_close_players = 0) %>%
  bind_rows(lowo_cv_summary_with_abs) %>%
  mutate(n_close_players = n_close_players - 2) %>%
  ggplot(aes(x = n_close_players, y = test_yac_rmse)) +
  geom_beeswarm(color = "darkblue") +
  stat_summary(fun = "mean", color = "darkorange", size = 3, geom = "point") +
  theme_bw() +
  labs(x = "Number of defense / offense players included by distance",
       y = "Holdout YAC RMSE")




