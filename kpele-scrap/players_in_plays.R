library(tidyverse)
library(plyr)

df <- list.files(pattern = ".rds") %>% map(readRDS) %>%  bind_rows() %>% mutate(key = paste0(as.character(df$gameId),"-",as.character(df$playId)))
#x = ddply(df,~key,summarise, no_pl = length(unique(nflId)))

offense <- c("QB","OLB","RB","TE","WR","FB","HB")
defense <- c("CB","ILB","SS","DB","LB","MLB","FS","S","DT","DE","DL","NT")

df$team_unit = ifelse(df$position %in% offense, "offense","defense")

x = ddply(df,~key,summarise, no_pl = paste0(as.character(length(unique(nflId[team_unit == "offense"]))),"-",as.character(length(unique(nflId[team_unit == "defense"])))))
