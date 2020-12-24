# PURPOSE: View the results for the LOWO CV RFCDE CRPS results

library(tidyverse)


# Load output -------------------------------------------------------------

lowo_cv_summary <-
  read_csv("data/model_output/lowo_cv_results/play_context_rfcde_crps_summary.csv")


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
  summarize(med_crps = median(test_crps))

