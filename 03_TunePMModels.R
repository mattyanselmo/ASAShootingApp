# Predictive models
library(dplyr)
# year <- 2018
current.season <- year

# Load schedule ####
sched <- read.csv("IgnoreList/MLS_2018_Schedule.csv")
teamabbr <- read.csv("TeamNameLinks_MLSSchedule.csv")
sched <- sched %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y"),
         Home = trimws(Home),
         Away = trimws(Away)) %>%
  left_join(teamabbr, c("Home" = "Original")) %>%
  mutate(Home = Convert) %>%
  select(-Convert) %>%
  left_join(teamabbr, c("Away" = "Original")) %>%
  mutate(Away = Convert) %>%
  select(-Convert)

training.dat <- readRDS('IgnoreList/TeamPMData.rds')

# Create current standings
conferences <- read.csv('teamsbyconferencebyseason.csv')
teamxgoals <- readRDS('IgnoreList/xGoalsByTeam.rds')
source("TeamxGoalsFunction_withWins.R")
standings <- teamxgoals.func.withwins(teamxgoals = teamxgoals,
                                      date1 = as.Date('2000-01-01'),
                                      date2 = as.Date('9999-12-31'),
                                      season = 2018,
                                      even = F,
                                      pattern = 'All',
                                      pergame = F,
                                      advanced = F,
                                      venue = c('Home', 'Away'),
                                      byseasons = T,
                                      plot = F) %>%
  select(Team, Games, GF, GA, Wins, Pts, Conf)

saveRDS(standings, "IgnoreList/CurrentStandings.rds")
saveRDS(sched %>% 
          filter(Date > max(training.dat$date)) %>% 
          unique(), 
        file = "IgnoreList/RemainingSchedule.rds")


max.week <- max(training.dat$week[training.dat$Season == max(training.dat$Season)])
weeks <- c(max(max.week - 15, 10), max.week)

# Build bivariate Poisson model ####
form.home <- formula(GF_home ~ xGF_season_team_home + 
                       I(GF_season_home - xGF_season_team_home) +
                       xGA_season_team_away +
                       I(GA_season_away - xGA_season_team_away))

form.away <- formula(GF_away ~ xGF_season_team_away + 
                       I(GF_season_away - xGF_season_team_away) +
                       xGA_season_team_home +
                       I(GA_season_home - xGA_season_team_home))

model.home <- glm(form.home,
                  data = dat.pred %>% filter(week >= weeks[1], week <= weeks[2]),
                  family = poisson,
                  weights = week^2)

model.away <- glm(form.away,
                  data = dat.pred %>% filter(week >= weeks[1], week <= weeks[2]),
                  family = poisson,
                  weights = week^2)

# Save files ####
save(model.home, model.away, file = 'IgnoreList/UnivariatePoissonModels.Rdata')
