# Update data and re-deploy app

# setwd('C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_master')
year <- 2018

source('01_ASAShotCharts.R')
source("01_VerticalLineups.R")
source("01_ASAPassCharts.R")

source("02_MergeMinutesWithTeam.R")
source('02_CompressPlayerxGoals.R')
source('02_CompressTeamxGoals.R')
source('02_CompressPlayerPassing.R')
source('02_CompressTeamPassing.R')
# Construct predictive model dataset
# source('03_TeamPMData.R')
# source('03_TeamPredictiveModelFunction.R')
source('02_CompressKeeperxGoals.R')

source("03_CreateHTMLTables.R")
gc()

source('IgnoreList/ConnectOnline.R')
Y