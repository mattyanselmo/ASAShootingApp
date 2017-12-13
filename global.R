library(dplyr)
library(shiny)
library(DT)
library(shinyjs)
library(ggplot2)

playerxgoals <- readRDS('IgnoreList/xGoalsByPlayer.rds')
minutesPlayed <- readRDS('IgnoreList/MinutesByGameID.rds')
teamxgoals <- readRDS('IgnoreList/xGoalsByTeam.rds')
xgbygame <- readRDS('IgnoreList/xGoalsByTeam_byGame.rds')
keeperxgoals <- readRDS('IgnoreList/xGoalsByKeeper.rds')
conferences <- read.csv('teamsbyconferencebyseason.csv')
glossary <- read.csv('Glossary.csv')
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
source('TeamxGoalsFunction.R')
source('KeeperxGoalsFunction.R')
