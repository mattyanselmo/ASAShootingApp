# Predict Function ####

pred.data <- readRDS('IgnoreList/TeamPredictionsData_game34.rds')
mult.model <- readRDS('IgnoreList/MultinomModel_weeks25-35.rds')

# mod.data.recent should be each teams most recent updates to xGoals
pm.function <- function(pred.data, hteam, ateam){

  if(logit){
    
  } else{
    #Poisson stuff
  }


}

resultsglm <- data.frame()
resultsgbm <- data.frame()
for(season in unique(mod.data$Season)){
  glm.temp <- glm(final > 0 ~ 
                    I(xGF_season_team_home - xGA_season_team_home) +
                    I(xGF_season_team_away - xGA_season_team_away),
                  data = mod.data %>% filter(Season != season),
                  family = binomial)
  resultsglm <- rbind(resultsglm, data.frame(Season = season, 
                                             final = mod.data %>% 
                                               filter(Season == season) %>%
                                               mutate(final = ifelse(final > 0, 1, 0)) %>%
                                               select(final),
                                             prediction = predict(glm.temp,
                                                                  newdata = mod.data %>%
                                                                    filter(Season == season),
                                                                  type = 'response')))
  gbm.temp <- gbm(final > 0 ~ 
                    xGF_season_team_home + xGA_season_team_home +
                    xGF_season_team_away + xGA_season_team_away,
                  distribution = 'bernoulli',
                  data = mod.data %>% filter(Season != season),
                  shrinkage = 0.005,
                  interaction.depth = 2,
                  n.trees = 250,
                  n.minobsinnode = 20,
                  train.fraction = 0.5)
  
  resultsgbm <- rbind(resultsgbm, data.frame(Season = season, 
                                             final = mod.data %>% 
                                               filter(Season == season) %>%
                                               mutate(final = ifelse(final > 0, 1, 0)) %>%
                                               select(final),
                                             prediction = predict(gbm.temp,
                                                                  n.trees = gbm.perf(gbm.temp, plot.it = F),
                                                                  newdata = mod.data %>%
                                                                    filter(Season == season),
                                                                  type = 'response')))
  
}

-sum((resultsglm %>% 
        mutate(loglik = ifelse(final == 1, log(prediction), log(1 - prediction))))$loglik)
-sum((resultsgbm %>% 
        mutate(loglik = ifelse(final == 1, log(prediction), log(1 - prediction))))$loglik)

