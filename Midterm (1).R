library(tidyverse)
library(mosaic)
library(DataComputing)


teamConferences <- read_csv("MTeamConferences.csv")
results <- read_csv("MNCAATourneyCompactResults.csv")

results <- results %>%
  mutate(
    ID = if_else(
      condition = WTeamID > LTeamID,
      true = str_c(Season, LTeamID, WTeamID, sep = "_"),
      false = str_c(Season, WTeamID, LTeamID, sep = "_")
    )
  )

head(results)
joined <- results %>%
  full_join(, by = c("ID" = "ID")) %>%
  separate(ID, into = c("year", "team1", "team2"), sep = "_", remove = FALSE) %>%
  mutate(
    is_training = is.na(is_submission_row),
    outcome = if_else(
      condition = WTeamID > LTeamID,
      true = 0,
      false = 1
    ),
    year = as.integer(year),
    team1 = as.integer(team1),
    team2 = as.integer(team2)
  )

head(joined_df)

seed <- read_csv('MNCAATourneySeeds.csv')
