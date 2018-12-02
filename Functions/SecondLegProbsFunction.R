year <- 2018
max.week <- 35
source('03_TeamPMData.R')
source("03_TunePMModels.R")
source("03_TeamPMDataThruCurrentWeek.R")
source("03_TeamPredictiveModelFunction.R")

mat <- pm.function(pred.data %>% mutate(Season = year), model.home, model.away, "ATL", "POR", season = year)$score.mat
score <- c(0, 0)

deficit = score[2] - score[1]
homeprob <- sum(sapply(1:11, function(x) sum(mat[1:11 - x > deficit | (1:11 - x == deficit & x <= score[1]),x])))
awayprob <- sum(sapply(1:11, function(x) sum(mat[1:11 - x < deficit | (1:11 - x == deficit & x > score[1] + 1),x])))
tieprob <- 1 - awayprob - homeprob

homeprob + 0.55*tieprob

scores <- matrix(c(rep(0:5, 6), rep(0:5, rep(6, 6))), ncol = 2)
probs <- apply(scores, 1, FUN = function(score){
  deficit = score[2] - score[1]
  homeprob <- sum(sapply(1:11, function(x) sum(mat[1:11 - x > deficit | (1:11 - x == deficit & x <= score[1]),x])))
  awayprob <- sum(sapply(1:11, function(x) sum(mat[1:11 - x < deficit | (1:11 - x == deficit & x > score[1] + 1),x])))
  tieprob <- 1 - awayprob - homeprob
  
  homeprob + 0.55*tieprob
})

library(reshape2)
xtabs(formula = probs ~ NYRB + ATL, data = data.frame(scores, probs) %>% select(probs, NYRB = X1, ATL = X2))
