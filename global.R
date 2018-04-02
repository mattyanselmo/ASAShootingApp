library(dplyr)
library(shiny)
library(DT)
library(shinyjs)
library(ggplot2)

playerxgoals <- readRDS('IgnoreList/xGoalsByPlayer.rds') %>% 
  bind_rows(data.frame(date = as.Date("2018-04-01"),
                       team = "LAG",
                       shooter = "God Zlatan",
                       type = "Other",
                       shots = 5,
                       ontarget = 6,
                       unassisted = 7,
                       meddist = 150,
                       headers = 3,
                       goals = 8, 
                       xG = 0.1,
                       xG_gk = 8,
                       `G-xG` = 7.9,
                       keypasses = 0,
                       ontarget.pass = 0, 
                       meddist.pass = NA,
                       headers.pass = 0,
                       assists = -10,
                       xA = -10,
                       `A-xA` = 0,
                       player = "God Zlatan",
                       Season = 2018,
                       gameID = 666,
                       check.names = F))

minutesPlayed <- readRDS('IgnoreList/MinutesByGameID.rds')
# minutesPlayedPassing <- readRDS("IgnoreList/MinutesBySeason.rds")
teamxgoals <- readRDS('IgnoreList/xGoalsByTeam.rds')
xgbygame <- readRDS('IgnoreList/xGoalsByTeam_byGame.rds')
keeperxgoals <- readRDS('IgnoreList/xGoalsByKeeper.rds')
conferences <- read.csv('teamsbyconferencebyseason.csv')
glossary <- read.csv('Glossary.csv')

playerpassing <- readRDS("IgnoreList/xPassingByPlayer.rds") %>%
  bind_rows(data.frame(passer = "God Zlatan",
                       year = 2018,
                       team = "LAG",
                       third = c("Def", "Mid", "Att"),
                       N = 1,
                       successes = -10,
                       exp = -10,
                       Position = "Heaven",
                       Distance = 0,
                       Vert.Dist = 0,
                       touchpct = 1,
                       minutes = 666,
                       touches = 1))

teampassing.offense <- readRDS("IgnoreList/xPassingByTeamOffense.rds")
teampassing.defense <- readRDS("IgnoreList/xPassingByTeamDefense.rds")

#pred.data <- readRDS('IgnoreList/TeamPredictionsData_week35.rds')
#load('IgnoreList/UnivariatePoissonModels.Rdata')

lm_eqn <- function(df, x, y){
  m <- lm(formula(paste0(y, ' ~ ', x)), df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

source('ShooterxGoalsFunction.R')
source('ShooterxGoalsFunction_perminute.R')
source('TeamxGoalsFunction.R')
source('KeeperxGoalsFunction.R')
source('KeeperxGoalsFunction_perminute.R')
source("PasserxPassesFunction.R")
source("TeamxPassesFunction.R")