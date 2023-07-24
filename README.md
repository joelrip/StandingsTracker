# StandingsTracker
Charting the 2023 US club soccer season.

Data imported using the api-football.com API.

Using SiteBuildingSteps.R, data subsetted into USL and MLS games from the current season, Western Conference teams identified, then run through StandingsTracker.R to produce MLSPointsLong.csv, USLPointsLong.csv, and USLOnePointsLong.csv.

Site rendered from index.Rmd using rmarkdown::render_site()
