mat <- pm.function(pred.data, model.home, model.away, "SKC", "RSL")$score.mat
score <- c(1, 1)

deficit = score[2] - score[1]
homeprob <- sum(sapply(1:11, function(x) sum(mat[1:11 - x > deficit | (1:11 - x == deficit & x <= score[1]),x])))
awayprob <- sum(sapply(1:11, function(x) sum(mat[1:11 - x < deficit | (1:11 - x == deficit & x > score[1] + 1),x])))
tieprob <- 1 - awayprob - homeprob

homeprob + 0.55*tieprob