library(dplyr)
library(shiny)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(ggplot2)
library(plotly)
library(ggrepel)
library(mgcv)
library(zoo)
library(reshape2)
library(stringr)

# For per-minute stats, positions, and other "new" info, input the first year we have that:
seasonFilter <- 2015

playerxgoals <- readRDS('AppData/xGoalsByPlayer.rds') 
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

minutesPlayed <- readRDS('AppData/MinutesByGameID_forapp.rds') %>%
  filter(Season >= seasonFilter) %>%
  mutate(minutes = as.integer(minutes))
teamxgoals <- readRDS('AppData/xGoalsByTeam.rds')
teamxgoals.adj <- readRDS("AppData/xGoalsByTeam_HomeAdjusted.rds")
xgbygame <- readRDS('AppData/xGoalsByTeam_byGame.rds')
keeperxgoals <- readRDS('AppData/xGoalsByKeeper.rds')
conferences <- read.csv('AppData/teamsbyconferencebyseason.csv')
glossary <- read.csv('AppData/Glossary.csv')
playerpos <- readRDS("AppData/playerpositions_byseason_2.rds") %>%
  filter(year >= seasonFilter)
playerpassing <- readRDS("AppData/xPassingByPlayer.rds") %>%
  filter(Season >= seasonFilter)
winexptable <- readRDS("AppData/WinExpectancy_fromGoalTimes.RDS")

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

teampassing.offense <- readRDS("AppData/xPassingByTeamOffense.rds") %>%
  filter(year >= seasonFilter)
teampassing.defense <- readRDS("AppData/xPassingByTeamDefense.rds") %>%
  filter(year >= seasonFilter)
playerchaindata <- readRDS("AppData/PlayerxGChainData.rds") %>%
  filter(Season >= seasonFilter)

current.year <- max(playerxgoals$Season)
simfiles <- grep("CurrentSimulationResults", list.files("AppData"), value = T)
cupfiles <- grep("MLSCupSimulationResults", list.files("AppData"), value = T)
weeklyfiles <- grep("WeeklyMatchPredictions", list.files("AppData"), value = T)

simfiles.weeks <- str_extract(simfiles, "(?<=_week)[0-9]*")
simfiles.years <- str_extract(simfiles, "(?<=_year)[0-9]*")

cupfiles.weeks <- str_extract(cupfiles, "(?<=_week)[0-9]*")
cupfiles.years <- str_extract(cupfiles, "(?<=_year)[0-9]*")

weeklyfiles.weeks <- str_extract(weeklyfiles, "(?<=_week)[0-9]*")
weeklyfiles.years <- str_extract(weeklyfiles, "(?<=_year)[0-9]*")


# Need to include weekly files lookups, to get playoff predictions in app
current.week <- max(as.numeric(simfiles.weeks)[simfiles.years == as.character(current.year)], na.rm = T)
current.week.cup <- max(as.numeric(cupfiles.weeks)[cupfiles.years == as.character(current.year)], na.rm = T) 
current.week.weekly <- max(as.numeric(weeklyfiles.weeks)[weeklyfiles.years == as.character(current.year)], na.rm = T) 

weeklypreds <- readRDS(paste0("AppData/WeeklyMatchPredictions_week", current.week.weekly, "_year", current.year, ".rds"))

playoffsseeding_west <- readRDS(paste0("AppData/CurrentSimulationResults_playoffseeding_west_week",
                                      current.week,
                                      "_year", current.year, ".rds"))
playoffsseeding_west_last <- readRDS(paste0("AppData/CurrentSimulationResults_playoffseeding_west_week",
                                       current.week - 1,
                                       "_year", current.year, ".rds"))

playoffsseeding_east <- readRDS(paste0("AppData/CurrentSimulationResults_playoffseeding_east_week",
                                       current.week,
                                       "_year", current.year, ".rds"))
playoffsseeding_east_last <- readRDS(paste0("AppData/CurrentSimulationResults_playoffseeding_east_week",
                                            current.week - 1,
                                            "_year", current.year, ".rds"))

cupchances <- readRDS(paste0("AppData/MLSCupSimulationResults_week", 
                             current.week.cup,
                             "_year",
                             current.year,
                             ".rds"))
cupchances_last <- readRDS(paste0("AppData/MLSCupSimulationResults_week", 
                             current.week.cup - 1,
                             "_year",
                             current.year,
                             ".rds"))

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

source('Functions/ShooterxGoalsFunction.R')
source('Functions/ShooterxGoalsFunction_perminute.R')
source('Functions/TeamxGoalsFunction.R')
source("Functions/TeamxGoalsFunction_splits.R")
source('Functions/KeeperxGoalsFunction.R')
source('Functions/KeeperxGoalsFunction_perminute.R')
source("Functions/PasserxPassesFunction.R")
source("Functions/PasserxPassesFunction_perminute.R")
source("Functions/TeamxPassesFunction.R")
source("Functions/xGChainPlayerFunction.R")
source("Functions/SalaryFunction_Player.R")
source("Functions/SalaryFunction_Team.R")
source("Functions/PlotWinProbCharts.R")
load("AppData/WinExpectancyModels.Rdata")
