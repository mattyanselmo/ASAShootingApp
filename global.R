library(dplyr)
library(shiny)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(ggplot2)
library(plotly)
library(ggrepel)

playerxgoals <- readRDS('IgnoreList/xGoalsByPlayer.rds') 
# Create made-up players
# %>% 
#   bind_rows(data.frame(date = as.Date("2018-04-01"),
#                        team = "LAG",
#                        shooter = "God Zlatan",
#                        type = "Other",
#                        shots = 5,
#                        ontarget = 6,
#                        unassisted = 7,
#                        meddist = 150,
#                        headers = 3,
#                        goals = 8, 
#                        xG = 0.1,
#                        xG_gk = 8,
#                        `G-xG` = 7.9,
#                        keypasses = 0,
#                        ontarget.pass = 0, 
#                        meddist.pass = NA,
#                        headers.pass = 0,
#                        assists = -10,
#                        xA = -10,
#                        `A-xA` = 0,
#                        player = "God Zlatan",
#                        Season = 2018,
#                        gameID = 666,
#                        check.names = F))

minutesPlayed <- readRDS('IgnoreList/MinutesByGameID_forapp.rds')
# minutesPlayedPassing <- readRDS("IgnoreList/MinutesBySeason.rds")
teamxgoals <- readRDS('IgnoreList/xGoalsByTeam.rds')
xgbygame <- readRDS('IgnoreList/xGoalsByTeam_byGame.rds')
keeperxgoals <- readRDS('IgnoreList/xGoalsByKeeper.rds')
conferences <- read.csv('teamsbyconferencebyseason.csv')
glossary <- read.csv('Glossary.csv')
playerpos <- readRDS("IgnoreList/playerpositions_byseason.rds")
playerpassing <- readRDS("IgnoreList/xPassingByPlayer.rds")
# %>%
#   bind_rows(data.frame(passer = "God Zlatan",
#                        year = 2018,
#                        team = "LAG",
#                        third = c("Def", "Mid", "Att"),
#                        N = 1,
#                        successes = -10,
#                        exp = -10,
#                        Position = "Heaven",
#                        Distance = 0,
#                        Vert.Dist = 0,
#                        touchpct = 1,
#                        minutes = 666,
#                        touches = 1))

teampassing.offense <- readRDS("IgnoreList/xPassingByTeamOffense.rds")
teampassing.defense <- readRDS("IgnoreList/xPassingByTeamDefense.rds")
playerchaindata <- readRDS("IgnoreList/PlayerxGChainData.rds")
playoffsseeding_west <- readRDS("IgnoreList/CurrentSimulationResults_playoffseeding_west.rds")
playoffsseeding_east <- readRDS("IgnoreList/CurrentSimulationResults_playoffseeding_east.rds")
cupchances <- readRDS(paste0("AppData/", sort(grep(".rds", grep("MLSCupSimulationResults", list.files("AppData"), value = T), value = T), decreasing = T)[1]))
salary.data <- readRDS("AppData/SalaryData.rds")

#pred.data <- readRDS('IgnoreList/TeamPredictionsData_week35.rds')
#load('IgnoreList/UnivariatePoissonModels.Rdata')

ggtheme <- theme(legend.position = "none",
                 axis.text=element_text(size = 12),
                 axis.title=element_text(size = 16, face = "bold"))

lm_eqn <- function(df, x, y){
  m <- lm(formula(paste0(y, " ~ ", x)), df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

lm_eqn2 <- function(df, x, y){
  m <- lm(formula(paste0(y, " ~ ", x)), df);
  a <- format(coef(m)[1], digits = 3) 
  b <- format(coef(m)[2], digits = 3) 
  r <- format(summary(m)$r.squared^0.5, digits = 3)
  paste0("y = ", b, "x + ", a, " ; r = ", r)                 
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

source('ShooterxGoalsFunction.R')
source('ShooterxGoalsFunction_perminute.R')
source('TeamxGoalsFunction.R')
source("TeamxGoalsFunction_splits.R")
source('KeeperxGoalsFunction.R')
source('KeeperxGoalsFunction_perminute.R')
source("PasserxPassesFunction.R")
source("PasserxPassesFunction_perminute.R")
source("TeamxPassesFunction.R")
source("xGChainPlayerFunction.R")
source("SalaryFunction_Player.R")
source("SalaryFunction_Team.R")
