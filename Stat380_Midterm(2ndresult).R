rm(list=ls())
#library(tidyverse)
library(dplyr)
library(tidyr)
#library(data.table)


sub_results <- read.csv("example_sub.csv", stringsAsFactors = FALSE) 
rankings <- read.csv("massey.csv",stringsAsFactors = FALSE)%>%
  spread(key = SystemName, value = OrdinalRank)
teams <- read.csv("teams2019.csv", stringsAsFactors = FALSE)
tourney <- read.csv("tourney.csv", stringsAsFactors = FALSE)
season <- read.csv("season.csv", stringsAsFactors = FALSE)

colnames(rankings)[2] <- "DayNum"

rankings <- rankings[rankings$DayNum == 133, ] 


rankings$Mean <- rowMeans(rankings[, 3:ncol(rankings)], na.rm=TRUE)

rankings <- rankings[,c("Season","TeamID","Mean")]


train <- season %>%
  select(Season, WTeamID, LTeamID) %>% 
  mutate(team_id_diff = WTeamID - LTeamID,
         Team1 = if_else(team_id_diff < 0, WTeamID, LTeamID),
         Team2 = if_else(team_id_diff > 0, WTeamID, LTeamID),
         result = if_else(WTeamID == Team1, 1, 0)) %>%
  select(Season, Team1, Team2, result) %>%
  subset(Season >= 2003)


train <- train %>% 
  left_join(rankings, by = c("Season", "Team1" = "TeamID")) %>%
  left_join(rankings, by = c("Season",  "Team2" = "TeamID"))


train <- subset(train, Season < 2014)


#linearMod <- lm(result ~ Mean.x + Mean.y,data = train)


fit <- glm(result ~ Mean.x + Mean.y,
           data = train, 
           family = "binomial")

sub_results <- read.csv("example_sub.csv", stringsAsFactors = FALSE)  %>% 
  select(id) %>% 
  separate(id, sep = "_", into = c("Season","id", "Team1", "Team2"), convert = TRUE) %>%
  left_join(rankings, by = c("Team1" = "TeamID")) %>%
  left_join(rankings, by = c("Team2" = "TeamID")) %>% filter(Season == Season.y)



sub_results$Pred <- predict(fit, sub_results, type = "response")


submit <- sub_results %>% 
  select(Season.x, id, Team1, Team2, Pred) %>%
  unite("id", Season.x, id, Team1, Team2, sep = "_") %>% 
  group_by(id) %>% summarise(result = mean(Pred)) %>% 
  write.csv("submit.csv", row.names = FALSE)

#2019_122_1410_1465
#2019_124_1308_1465
