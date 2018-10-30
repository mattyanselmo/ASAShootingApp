# Playoff simulation functions
sim.knockout <- function(home, away){
  score.mat <- pm.function(pred.data = pred.data,
                           model.home = model.home,
                           model.away = model.away,
                           team.home = home,
                           team.away = away,
                           season = year)$score.mat
  cell <- which(cumsum(score.mat) > runif(1))[1]
  scores <- c(cell %% 11 - 1, (cell - 1) %/% 11)
  
  if(scores[1] > scores[2]){
    home
  } 
  else if(scores[1] < scores[2]){
    away
  }
  else{
    score.mat <- pm.function(pred.data = pred.data,
                             model.home = model.home,
                             model.away = model.away,
                             team.home = home,
                             team.away = away,
                             season = year,
                             time = 1/3)$score.mat
    cell <- which(cumsum(score.mat) > runif(1))[1]
    scores <- c(cell %% 11 - 1, (cell - 1) %/% 11)
    if(scores[1] > scores[2]){
      home
    }
    else if(scores[1] < scores[2]){
      away
    }
    else{
      ifelse(runif(1) < 0.5, home, away)
    }
  }
}

sim.aggregate <- function(home, away){
  # Game 1
  score.mat <- pm.function(pred.data = pred.data,
                           model.home = model.home,
                           model.away = model.away,
                           team.home = away,
                           team.away = home,
                           season = year)$score.mat
  
  cell <- which(cumsum(score.mat) > runif(1))[1]
  scores1 <- c(cell %% 11 - 1, (cell - 1) %/% 11)
  
  # Game 2
  score.mat <- pm.function(pred.data = pred.data,
                           model.home = model.home,
                           model.away = model.away,
                           team.home = home,
                           team.away = away,
                           season = year)$score.mat
  cell <- which(cumsum(score.mat) > runif(1))[1]
  scores2 <- c(cell %% 11 - 1, (cell - 1) %/% 11)
  
  scorestot <- scores1[2:1] + scores2
  
  if(scorestot[1] > scorestot[2]){
    home
  }
  else if(scorestot[1] < scorestot[2]){
    away
  }
  else{
    if(scores1[2] > scores2[2]){
      home
    }
    else if(scores1[2] < scores2[2]){
      away
    }
    else{
      score.mat <- pm.function(pred.data = pred.data,
                               model.home = model.home,
                               model.away = model.away,
                               team.home = home,
                               team.away = away,
                               season = year,
                               time = 1/3)$score.mat
      cell <- which(cumsum(score.mat) > runif(1))[1]
      scores3 <- c(cell %% 11 - 1, (cell - 1) %/% 11)
      if(scores3[1] > scores3[2]){
        home
      }
      else if(scores3[1] < scores3[2]){
        away
      }
      else{
        ifelse(runif(1) < 0.5, home, away)
      }
    }
  }
}

sim.round <- function(teams, roundtype = "knockout"){
  if(roundtype == "knockout"){
    sapply(seq(1, length(teams), 2), function(x) sim.knockout(home = teams[x], away = teams[x+1]))
  }
  else if(roundtype == "aggregate"){
    sapply(seq(1, length(teams), 2), function(x) sim.aggregate(home = teams[x], away = teams[x+1]))
  }
}