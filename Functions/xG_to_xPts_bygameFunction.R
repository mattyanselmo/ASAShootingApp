# library(dplyr)
  
# # Sample inputs
# team1.xg <- c(0.1, 0.1, 0.1, 0.1, 0.1)
# team2.xg <- c(0, 0.05, 0.1, 0.15, 0.2)
# sims = 1000

xpts.func <- function(team1.xg,
                      team2.xg,
                      sims = 1000){
  
  pts <- c()
  for(i in 1:sims){
    G1 = sum(runif(length(team1.xg)) < team1.xg) 
    G2 = sum(runif(length(team2.xg)) < team2.xg) 
    pts <- c(pts, ifelse(G1 > G2, 3, ifelse(G1 == G2, 1, 0)))
  }
  c(mean(pts), mean(ifelse(pts == 3, 0, ifelse(pts == 1, 1, 3))), sd(pts))
}

# # Function testing
# xg1 <- rep(0.1, 5)
# xg2 <- seq(0, 0.2, length.out = 5)
# xg3 <- rep(0.1, 20)
# xg4 <- seq(0, 0.2, length.out = 20)
# 
# xpts.func(xg1, xg2, 1000)
# xpts.func(xg3, xg4, 1000)
