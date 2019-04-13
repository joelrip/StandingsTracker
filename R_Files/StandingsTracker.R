##Standings tracker script

#ScoresTable is a table of scores in WhoScored.com format
#west is a vector of the names of the teams in the western conference
#games is the number of games in the regular season
#inNum is the number of teams in each conference that make the playoffs (or are promoted or go on to a larger tournament)
#outNum is the number of teams in each conference that don't make the playoffs (or are relegated)

#ScoresTable <- USLScores
#west <- USLWest
#games = 34
#inNum = 10
#outNum = 8

StandingsTracker <- function(ScoresTable, west, games, inNum, outNum) {
  
  require(tidyr)
  
  #Format dates, calculate points per game
  ScoresTable = ScoresTable[,c(1,4,5,15,16)]
  ScoresTable = ScoresTable[which(!is.na(ScoresTable$score1)),]
  #ScoresTable$JustDate = substr(ScoresTable$Date, regexpr(",", ScoresTable$Date) + 2, nchar(ScoresTable$Date))
  #ScoresTable$DateTime = as.POSIXct(paste(ScoresTable$JustDate, ScoresTable$Time), tz = "GMT", format = "%b %e %Y %H:%M")
  #ScoresTable$CADateTime = format(ScoresTable$DateTime, tz = "America/Los_Angeles")
  #ScoresTable$HGoals = as.numeric(substr(ScoresTable$Score, 1, regexpr(":", ScoresTable$Score) - 1))
  #ScoresTable$AGoals = as.numeric(substr(ScoresTable$Score, regexpr(":", ScoresTable$Score) + 1, nchar(ScoresTable$Score)))
  ScoresTable$HPoints = NA
  ScoresTable$APoints = NA
  for (item in 1:nrow(ScoresTable)) {
    if (ScoresTable$score1[item] > ScoresTable$score2[item]) {
      ScoresTable$HPoints[item] = 3
      ScoresTable$APoints[item] = 0
    } else if (ScoresTable$score2[item] > ScoresTable$score1[item]) {
      ScoresTable$APoints[item] = 3
      ScoresTable$HPoints[item] = 0
    } else {
      ScoresTable$APoints[item] = 1
      ScoresTable$HPoints[item] = 1
    }
  }
  
  #convert to cumulative points table
  teams = c(ScoresTable$team1, ScoresTable$team2)
  teams = unique(teams[order(teams)])
  PointsTable = data.frame("Teams" = teams, "Game0" = 0)
  for (item in 1:games) {
    PointsTable = cbind(PointsTable, NA)
  }
  headers = as.vector("Game1")
  for (item in 2:games) {
    headers = append(headers, paste0("Game", item))
  }
  colnames(PointsTable) = c("Teams", "Game0", headers)
  for (item in 1:nrow(ScoresTable)) {
    team1 = ScoresTable$team1[item]
    team2 = ScoresTable$team2[item]
    homeRow = which(PointsTable$Teams == team1)
    for (column in 3:(games + 2)) {
      if (is.na(PointsTable[homeRow, column])) {
        if (column == 2) {
          PointsTable[homeRow, column] = ScoresTable$HPoints[item]
          break
        } else {
          PointsTable[homeRow, column] = ScoresTable$HPoints[item] + PointsTable[homeRow, column -1]
          break
        }
      }
    }
    awayRow = which(PointsTable$Teams == team2)
    for (column in 3:(games + 2)) {
      if (is.na(PointsTable[awayRow, column])) {
        if (column == 2) {
          PointsTable[awayRow, column] = ScoresTable$APoints[item]
          break
        } else {
          PointsTable[awayRow, column] = ScoresTable$APoints[item] + PointsTable[awayRow, column -1]
          break
        }
      }
    }
  }
  
  #format for plotting, divide by conference
  LastGame = paste0("Game", games)
  PointsLong = gather(PointsTable, Game, Points, Game0:LastGame)
  PointsLong$Game = as.numeric(substr(as.character(PointsLong$Game), 5, nchar(as.character(PointsLong$Game))))
  PointsLong$Teams = as.character(PointsLong$Teams)
  PointsLong$Conference = "East"
  for (item in 1:nrow(PointsLong)) {
    if (PointsLong$Teams[item] %in% west) {
      PointsLong$Conference[item] = "West"
    }
  }
  PointsLong$Teams[which(PointsLong$Teams == "Colorado Springs Switchbacks FC")] = "CS Switchbacks FC"
  
  ###Figure out playoffs positioning
  PointsLong$MaxPoints = PointsLong$Points
  PointsLong$MinPoints = PointsLong$Points
  PointsLong$MaxFinal = NA
  for (item in 1:nrow(PointsLong)) {
    if (is.na(PointsLong$Points[item])) {
      PointsLong$MaxPoints[item] = max(PointsLong$Points[which(PointsLong$Teams == PointsLong$Teams[item])], na.rm = T) +
        (3 * (PointsLong$Game[item] - max(PointsLong$Game[which(PointsLong$Teams == PointsLong$Teams[item] & !is.na(PointsLong$Points))], na.rm = T)))
      PointsLong$MinPoints[item] = max(PointsLong$Points[which(PointsLong$Teams == PointsLong$Teams[item])], na.rm = T)
    }
    PointsLong$MaxFinal[item] = PointsLong$MaxPoints[item] + (3 * (games - PointsLong$Game[item]))
  }
  for (item in 0:games) {
    MaxScoresW = sort(PointsLong$MaxFinal[which(PointsLong$Game == item & PointsLong$Conference == "West")], decreasing = T)
    InThresholdW = MaxScoresW[inNum + 1] + 1
    MinScoresW = sort(PointsLong$MinPoints[which(PointsLong$Game == item & PointsLong$Conference == "West")])
    OutThresholdW = (MinScoresW[outNum + 1] - 1) - (3 * (games - item))
    PointsLong = rbind(PointsLong, list("Thresholds", item, NA, "West", NA, OutThresholdW, InThresholdW))

    MaxScoresE = sort(PointsLong$MaxFinal[which(PointsLong$Game == item & PointsLong$Conference == "East")], decreasing = T)
    InThresholdE = MaxScoresE[inNum + 1] + 1
    MinScoresE = sort(PointsLong$MinPoints[which(PointsLong$Game == item & PointsLong$Conference == "East")])
    OutThresholdE = (MinScoresE[outNum + 1] - 1) - (3 * (games - item))
    PointsLong = rbind(PointsLong, list("Thresholds", item, NA, "East", NA, OutThresholdE, InThresholdE))
  }
  
  #Add columns to PointsLong to precalculate values for charts
  PointsLong$PointsLessGames = PointsLong$Points - PointsLong$Game
  PointsLong$MaxPointsLessGames = PointsLong$MaxPoints - PointsLong$Game
  PointsLong$MinPointsLessGames = PointsLong$MinPoints - PointsLong$Game
  PointsLong$MaxFinalLessGames = PointsLong$MaxFinal - PointsLong$Game
  
  return(PointsLong)
}