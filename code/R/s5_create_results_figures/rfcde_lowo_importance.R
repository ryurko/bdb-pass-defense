# PURPOSE: View the top variables in the RFCDE models based on importance

library(tidyverse)


# Load variable importance results ----------------------------------------

rfcde_lowo_importance <-
  read_rds("data/model_output/lowo_rfcde_var_imp_loss.rds")


# Create a single display averaging across weeks --------------------------

rfcde_lowo_importance %>%
  group_by(variable_name) %>%
  summarize(mean_imp = mean(value)) %>%
  ungroup() %>%
  arrange(desc(mean_imp)) %>%
  slice(1:10) %>%
  mutate(variable_name = fct_reorder(variable_name, mean_imp)) %>%
  ggplot(aes(x = variable_name, y = mean_imp)) +
  geom_bar(stat = "identity", fill = "darkblue", color = "darkblue",
           width = .9) +
  theme_bw() +
  coord_flip() +
  labs(x = "Feature", y = "Average importance across LOWO RFCDE models",
       title = "Top ten variables by average importance across 17 LOWO RFCDE models")
