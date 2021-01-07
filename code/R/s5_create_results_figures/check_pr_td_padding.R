# PURPOSE: Check differences from using padding for touchdown probability

library(tidyverse)


# Load two sets of predicted play distributions ---------------------------

# Without padding:
init_rfcde_pred_yac_data <-
  read_rds("data/model_output/lowo_rfcde_pred_yac_data.rds")

# With padding:
pad_rfcde_pred_yac_data <-
  read_rds("data/model_output/lowo_rfcde_pred_yac_data_w_padding.rds")


# Load distribution summaries ---------------------------------------------

init_yac_distr_summary <-
  read_rds("data/model_output/lowo_pred_yac_distr_summary.rds")

pad_yac_distr_summary <-
  read_rds("data/model_output/lowo_pred_yac_distr_summary_w_padding.rds")

# What do the touchdown probabilities look like:
summary(init_yac_distr_summary$prob_td)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.0000000 0.0000000 0.0000000 0.0031800 0.0000068 0.6118010
summary(pad_yac_distr_summary$prob_td)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.0000000 0.0000000 0.0000477 0.0114089 0.0007318 0.9104487
