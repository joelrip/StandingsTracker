#Site building steps
#Downloads and formats game data for USL and MLS from api-football.com, runs through StandingsTracker function, saves csvs in long format

#Import scores from FiveThirtyEight (DATA UPDATE DISCONTINUED JUNE 2023)
#AllScores <- read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv", stringsAsFactors = F)
#AllScores$date <- as.POSIXct(AllScores$date)
#USLScores <- AllScores[which(AllScores$league_id == 2160), ] 
#MLSScores <- AllScores[which(AllScores$league_id == 1951), ] 

#Setup
library(tidyverse)
library(httr)
library(dotenv)

#Import USL League 1 scores from api-sports.io
api_usl_one <- GET(
  "https://v3.football.api-sports.io/fixtures?season=2023&league=489",
  add_headers(
    'x-rapidapi-host' = "v3.football.api-sports.io",
    'x-rapidapi-key' = Sys.getenv("API_SPORTS_API_KEY")
  )
)
usl_one_fixtures <- content(api_usl_one)

USLOneScores <- usl_one_fixtures$response %>% {
  tibble(date = map_chr(., c("fixture", "date")),
         league_id = map_int(., c("league", "id")),
         league = map_chr(., c("league", "name")),
         team1 = map_chr(., c("teams", "home", "name")),
         team2 = map_chr(., c("teams", "away", "name")),
         score1 = map(., c("goals", "home")),
         score2 = map(., c("goals", "away"))
  ) %>%
    mutate(date = as.Date(date),
           score1 = unlist(map(score1, ~ifelse(is.null(.x), NA, .x))),
           score2 = unlist(map(score2, ~ifelse(is.null(.x), NA, .x)))
    ) %>%
    arrange(date)
}

#Check data
table(USLOneScores$team1)
table(USLOneScores$team2)

#Run through formatting function
USLOnePointsLong = StandingsTracker(USLOneScores, NA, 32, 6, 6)

#Save
write.csv(USLOnePointsLong, "USLOnePointsLong.csv", row.names = F)

#Import USL Championship scores from api-sports.io
api_usl <- GET(
  "https://v3.football.api-sports.io/fixtures?season=2023&league=255",
  add_headers(
    'x-rapidapi-host' = "v3.football.api-sports.io",
    'x-rapidapi-key' = Sys.getenv("API_SPORTS_API_KEY")
  )
)
usl_fixtures <- content(api_usl)

USLScores <- usl_fixtures$response %>% {
  tibble(date = map_chr(., c("fixture", "date")),
         league_id = map_int(., c("league", "id")),
         league = map_chr(., c("league", "name")),
         team1 = map_chr(., c("teams", "home", "name")),
         team2 = map_chr(., c("teams", "away", "name")),
         score1 = map(., c("goals", "home")),
         score2 = map(., c("goals", "away"))
  ) %>%
  mutate(date = as.Date(date),
         score1 = unlist(map(score1, ~ifelse(is.null(.x), NA, .x))),
         score2 = unlist(map(score2, ~ifelse(is.null(.x), NA, .x)))
  ) %>%
  arrange(date)
}

#Check data
table(USLScores$team1)
table(USLScores$team2)

#Run through formatting function
USLWest = c("Sacramento Republic", "New Mexico United", "Orange County SC", "Colorado Springs",
            "El Paso Locomotive", "San Antonio", "Las Vegas Lights", "Phoenix Rising",
            "Rio Grande Valley", "San Diego Loyal", "Oakland Roots", "Monterey Bay") #correct for 2023
#USLScores <- USLScores[which(USLScores$date > "2023-01-01"),-1]
USLPointsLong = StandingsTracker(USLScores, USLWest, 34, 8, 4)

#Save
write.csv(USLPointsLong, "USLPointsLong.csv", row.names = F)

#Import MLS scores from api-sports.io
api_mls <- GET(
  "https://v3.football.api-sports.io/fixtures?season=2023&league=253",
  add_headers(
    'x-rapidapi-host' = "v3.football.api-sports.io",
    'x-rapidapi-key' = Sys.getenv("API_SPORTS_API_KEY")
  )
)
mls_fixtures <- content(api_mls)

MLSScores <- mls_fixtures$response %>% {
  tibble(date = map_chr(., c("fixture", "date")),
         league_id = map_int(., c("league", "id")),
         league = map_chr(., c("league", "name")),
         team1 = map_chr(., c("teams", "home", "name")),
         team2 = map_chr(., c("teams", "away", "name")),
         score1 = map(., c("goals", "home")),
         score2 = map(., c("goals", "away"))
  ) %>%
  mutate(date = as.Date(date),
         score1 = unlist(map(score1, ~ifelse(is.null(.x), NA, .x))),
         score2 = unlist(map(score2, ~ifelse(is.null(.x), NA, .x)))
  ) %>%
  arrange(date)
}

#Check data
table(MLSScores$team1)
table(MLSScores$team2)

#Run through formatting function
MLSWest = c("Vancouver Whitecaps", "Portland Timbers", "Seattle Sounders", "Sporting Kansas City", "Houston Dynamo",
            "FC Dallas", "San Jose Earthquakes", "Real Salt Lake", "Los Angeles Galaxy", "Minnesota United FC", "Colorado Rapids",
            "Los Angeles FC", "Austin", "St. Louis City") #correct for 2023
#MLSScores <- MLSScores[which(MLSScores$date > "2023-01-01"),-1]
MLSPointsLong = StandingsTracker(MLSScores, MLSWest, 34, 9, 5)

#Save
write.csv(MLSPointsLong, "MLSPointsLong.csv", row.names = F)

