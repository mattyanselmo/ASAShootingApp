library(dplyr)
library(shiny)
library(DT)
library(shinyjs)

playerxgoals <- readRDS('xGoalsByPlayer.rds')
teamxgoals <- readRDS('xGoalsByTeam.rds')
xgbygame <- readRDS('xGoalsByTeam_byGame.rds')
keeperxgoals <- readRDS('xGoalsByKeeper.rds')
conferences <- read.csv('teamsbyconferencebyseason.csv')

source('ShooterxGoalsFunction.R')
source('TeamxGoalsFunction.R')
source('KeeperxGoalsFunction.R')
