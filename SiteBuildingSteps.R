#Site building steps
#Downloads and formats game data for USL and MLS from Fivethirtyeight, runs through StandingsTracker function, saves csvs in long format

#Import scores from FiveThirtyEight
AllScores <- read.csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv", stringsAsFactors = F)
AllScores$date <- as.POSIXct(AllScores$date)
USLScores <- AllScores[which(AllScores$league_id == 2160), ] 
MLSScores <- AllScores[which(AllScores$league_id == 1951), ] 

#Check data
table(USLScores$team1)
table(USLScores$team2)

#Run through formatting function
USLWest = c("Sacramento Republic FC", "New Mexico United", "Orange County SC", "Colorado Springs Switchbacks FC",
            "El Paso Locomotive FC", "San Antonio FC", "Las Vegas Lights FC", "Arizona United",
            "Rio Grande Valley FC Toros", "San Diego Loyal SC", "Oakland Roots", "Monterey Bay") #correct for 2023
USLScores <- USLScores[which(USLScores$date > "2023-01-01"),-1]
USLPointsLong = StandingsTracker(USLScores, USLWest, 34, 8, 4)

#Save
write.csv(USLPointsLong, "USLPointsLong.csv", row.names = F)

#Check data
table(MLSScores$team1)
table(MLSScores$team2)

#Run through formatting function
MLSWest = c("Vancouver Whitecaps", "Portland Timbers", "Seattle Sounders FC", "Sporting Kansas City", "Houston Dynamo",
            "FC Dallas", "San Jose Earthquakes", "Real Salt Lake", "Los Angeles Galaxy", "Minnesota United FC", "Colorado Rapids",
            "Los Angeles FC", "Austin FC", "St. Louis CITY SC") #correct for 2023
MLSScores <- MLSScores[which(MLSScores$date > "2023-01-01"),-1]
MLSPointsLong = StandingsTracker(MLSScores, MLSWest, 34, 9, 5)

#Save
write.csv(MLSPointsLong, "MLSPointsLong.csv", row.names = F)
