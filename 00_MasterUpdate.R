# Update data and re-deploy app

# setwd('C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_master')
year <- 2018

source("01_SummarizeMinutesPlayed.R")
source('01_ASAShotCharts.R')
source("01_VerticalLineups.R")
source("01_ASAPassCharts.R")

source('02_CompressPlayerxGoals.R')
source('02_CompressTeamxGoals.R')
source('02_CompressPlayerPassing.R')
source('02_CompressTeamPassing.R')
source('02_CompressKeeperxGoals.R')
source("02_CompressPlayerTouches.R")

# Construct predictive model dataset
# source('03_TeamPMData.R')
# source('03_TeamPredictiveModelFunction.R')

if(length(grep("_master", getwd())) > 0){
  source("03_CreateHTMLTables.R")
  #source("03_CreateMLSStandings_HTML.R")
}
gc()

source('IgnoreList/ConnectOnline.R')
Y