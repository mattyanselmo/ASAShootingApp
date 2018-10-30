# Predict Function ####
# library(dplyr)

# Sample function inputs 
# pred.data <- readRDS("IgnoreList/TeamPredictionsData.rds")
# load('IgnoreList/UnivariatePoissonModels.Rdata')
# team.home = "FCD"
# team.away = "POR"
# season = 2018

pm.function <- function(pred.data, model.home, model.away, team.home, team.away, season = 2018, time = 1){
  
  temp.home <- pred.data %>%
    filter(team == team.home, Season == season)
  names(temp.home) <- paste0(names(temp.home), '_home')
  
  temp.away <- pred.data %>%
    filter(team == team.away, Season == season)
  names(temp.away) <- paste0(names(temp.away), '_away')
  
  predict.game <- data.frame(temp.home, temp.away)
  
  lambda.home <- predict(model.home, 
                         predict.game, 
                         type = 'response') * time
  
  lambda.away <- predict(model.away, 
                         predict.game, 
                         type = 'response') * time
  
  dist.home <- sapply(0:10, function(x) dpois(x, lambda = lambda.home))
  dist.away <- sapply(0:10, function(x) dpois(x, lambda = lambda.away))
  
  rho <- -0.05
  temp <-  dist.home %o% dist.away
  temp[1, 1] <- temp[1, 1] * (1 - lambda.home*lambda.away*rho)
  temp[1, 2] <- temp[1, 2] * (1 + lambda.home*rho)
  temp[2, 1] <- temp[2, 1] * (1 + lambda.away*rho)
  temp[2, 2] <- temp[2, 2] * (1 - rho)
  score.mat <- temp / sum(temp)
  
  #####
  prob.draw <- sum(diag(score.mat))
  prob.away <- sum(score.mat[upper.tri(score.mat)])
  prob.home <- sum(score.mat[lower.tri(score.mat)])
  
  return(list(score.mat = score.mat, probs = c(prob.home, prob.away, prob.draw)))
  
}

#  pm.function(pred.data, model.home, model.away, 'SEA', 'VAN', season = 2018)
