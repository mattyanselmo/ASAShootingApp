# Playoff simulation
# Will be included in season simulation at a later time

library(dplyr)
# year <- 2018
sched <- readRDS("IgnoreList/RemainingSchedule.rds")
standings <- readRDS("IgnoreList/CurrentStandings.rds") %>%
  arrange(desc(Pts), desc(Wins), desc(GF - GA), desc(GF))
pred.data <- readRDS("IgnoreList/TeamPredictionsData.rds")
load('IgnoreList/UnivariatePoissonModels.Rdata')
source("03_TeamPredictiveModelFunction.R")
source("Functions/PlayoffSimulationFunctions.R")

# Edit these each round to "seed" the simulation
east <- (standings %>% filter(Conf == "east"))$Team[1:6] # original seeding
west <- (standings %>% filter(Conf == "west"))$Team[1:6] # original seeding
overall <- standings$Team

N <- 10000
results <- data.frame(Team = c(), Round = c(), Run = c())
for(run in 1:N){
  east.sim <- c("NYRB", "CLB", "ATL", "NYC")
  west.sim <- c("SKC", "RSL", "SEA", "POR")
  # east.sim <- (standings %>% filter(Conf == "east"))$Team[1:6] # teams left
  # west.sim <- (standings %>% filter(Conf == "west"))$Team[1:6] # teams left
  
  # Eastern conference
  rnd.east <- ifelse(length(east.sim) == 6, 1,
                     ifelse(length(east.sim) == 4, 2,
                            ifelse(length(east.sim) == 2, 3, 4)))
  while(rnd.east < 4){
    if(rnd.east == 1){
      sim <- sim.round(teams = east[c(3, 6, 4, 5)], roundtype = "knockout")
      east.sim <- c(east[1], intersect(east, sim)[2], east[2], intersect(east, sim)[1])
    }
    else{
      east.sim <- intersect(east, sim.round(teams = east.sim, roundtype = "aggregate"))
    }
    results <- bind_rows(results, 
                         data.frame(Team = east.sim, 
                                    Round = rnd.east + 1, 
                                    Run = run, 
                                    stringsAsFactors = F))
    rnd.east <- rnd.east + 1
  }
  
  # Western conference
  rnd.west <- ifelse(length(west.sim) == 6, 1,
                     ifelse(length(west.sim) == 4, 2,
                            ifelse(length(west.sim) == 2, 3, 4)))
  while(rnd.west < 4){
    if(rnd.west == 1){
      sim <- sim.round(teams = west[c(3, 6, 4, 5)], roundtype = "knockout")
      west.sim <- c(west[1], intersect(west, sim)[2], west[2], intersect(west, sim)[1])
    }
    else{
      west.sim <- intersect(west, sim.round(teams = west.sim, roundtype = "aggregate"))
    }
    results <- bind_rows(results, 
                         data.frame(Team = west.sim, 
                                    Round = rnd.west + 1, 
                                    Run = run, 
                                    stringsAsFactors = F))
    rnd.west <- rnd.west + 1
  }
  
  # Finals
  results <- bind_rows(results, 
                       data.frame(Team = sim.round(teams = intersect(overall, c(east.sim, west.sim)), roundtype = "aggregate"),
                                  Round = 5,
                                  Run = run,
                                  stringsAsFactors = F))
}

table(results %>% filter(Round == 5) %>% select(Team))/N

(sim.summ <- results %>%
    group_by(Team) %>%
    summarize(`Conf Semis` = 1,
              #`Conf Semis` = sum(Round == 2)/N,
              `Conf Finals` = sum(Round == 3)/N,
              Finals = sum(Round == 4)/N,
              Champs = sum(Round == 5)/N) %>%
    arrange(desc(Champs), desc(Finals)))

write.csv(sim.summ, paste0("AppData/MLSCupSimulationResults - ", Sys.Date(), ".csv"))
saveRDS(sim.summ, paste0("AppData/MLSCupSimulationResults - ", Sys.Date(), ".rds"))
