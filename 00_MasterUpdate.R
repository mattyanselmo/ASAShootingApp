# Update data and re-deploy app

# setwd('C:/Users/Matthias.Kullowatz/Dropbox/ASA Blog Data/ShootingShinyApp')

source('01_ASAShotCharts.R')
#source('02_AddMinutesToShotCharts.R')
source('02_CompressPlayerxGoals.R')
source('02_CompressTeamxGoals.R')
# Construct predictive model dataset
# source('03_TeamPMData.R')
# source('03_TeamPredictiveModelFunction.R')
source('02_CompressKeeperxGoals.R')
source('IgnoreList/ConnectOnline.R')
