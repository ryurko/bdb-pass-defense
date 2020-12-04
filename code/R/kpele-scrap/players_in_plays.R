library(tidyverse)
library(plyr)

df <- list.files(pattern = ".rds") %>% map(readRDS) %>%  bind_rows() #%>% mutate(key = paste0(as.character(df$gameId),"-",as.character(df$playId)))
df$key = paste0(as.character(df$gameId),"-",as.character(df$playId))

offense <- c("QB","OLB","RB","TE","WR","FB","HB")
defense <- c("CB","ILB","SS","DB","LB","MLB","FS","S","DT","DE","DL","NT")

df$team_unit = ifelse(df$position %in% offense, "offense","defense")

#x = ddply(df,~key,summarise, no_pl = paste0(as.character(length(unique(nflId[team_unit == "offense"]))),"-",as.character(length(unique(nflId[team_unit == "defense"])))))

# filter plays with complete pass 

df_completed_pass_plays = df[(df$key %in% unique(df[df$event == 'pass_outcome_caught',]$key) &  ((df$event == "pass_outcome_caught") | (df$event== "pass_forward"))),]

x = ddply(df_completed_pass_plays,~key,summarise, no_pl = paste0(as.character(length(unique(nflId[team_unit == "offense"]))),"-",as.character(length(unique(nflId[team_unit == "defense"])))))
