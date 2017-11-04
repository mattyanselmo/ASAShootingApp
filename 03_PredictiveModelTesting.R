# Predictive models

pred.data <- readRDS('IgnoreList/TeamPredictionsData_game34.rds')
dat.pred <- readRDS('IgnoreList/TeamPMData.rds')

weeks <- c(25, 35)
form <- formula(as.factor(outcome) ~ I(xGF_season_team_home - xGA_season_team_home) +
                  I(xGF_season_team_away - xGA_season_team_away) +
                  I(GF_season_home - GA_season_home - (xGF_season_team_home - xGA_season_team_home)) +
                  I(GF_season_away - GA_season_away - (xGF_season_team_away - xGA_season_team_away)))

form2 <- formula(as.factor(outcome) ~ 
                   xGF_season_team_home + xGA_season_team_home +
                   xGF_season_team_away + xGA_season_team_away +
                   xGF_last10_team_home + xGA_last10_team_home +
                   xGF_last10_team_away + xGA_last10_team_away +
                   # xGF_season_gk_home + xGA_season_gk_home +
                   # xGF_season_gk_away + xGA_season_gk_away +
                   # xGF_last10_gk_home + xGA_last10_gk_home +
                   # xGF_last10_gk_away + xGA_last10_gk_away +
                   GF_season_home + GA_season_home  +
                   GF_season_away + GA_season_away
                 #GF_last10_home + GA_last10_home  +
                 #GF_last10_away + GA_last10_away
)

# Ordinal model ####
ord.model <- polr(formula = form, 
                  data = dat.pred %>% filter(Season <= 2015, week >= weeks[1], week <= weeks[2]))

# Multinomial model ####
mult.model <- multinom(formula = form,
                       data = dat.pred %>% filter(Season <= 2015, week >= weeks[1], week <= weeks[2]))
# GBM ####
gbm.model <- gbm(formula = form,
                 data = dat.pred %>% filter(Season <= 2015, week >= weeks[1], week <= weeks[2]),
                 distribution = 'multinomial',
                 n.trees = 1000,
                 interaction.depth = 4,
                 n.minobsinnode = 15,
                 shrinkage = 0.001,
                 cv.folds = 5)

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
                  data = dat.pred %>% filter(Season <= 2015, week >= weeks[1], week <= weeks[2]),
                  family = poisson)

model.away <- glm(form.away,
                  data = dat.pred %>% filter(Season <= 2015, week >= weeks[1], week <= weeks[2]),
                  family = poisson)

rho = -0.06
lambda.home <- predict(model.home, 
                       dat.pred %>% 
                         filter(Season > 2015, 
                                week >= weeks[1], 
                                week <= weeks[2]), 
                       type = 'response')

lambda.away <- predict(model.away, 
                       dat.pred %>% 
                         filter(Season > 2015, 
                                week >= weeks[1], 
                                week <= weeks[2]), 
                       type = 'response')
dist.home <- sapply(0:10, function(x) dpois(x, lambda = lambda.home))
dist.away <- sapply(0:10, function(x) dpois(x, lambda = lambda.away))

score.mats <- lapply(1:nrow(dist.home), function(i){
  temp <-  dist.home[i,] %o% dist.away[i,]
  temp[1, 1] <- temp[1, 1] * (1 - lambda.home[i]*lambda.away[i]*rho)
  temp[1, 2] <- temp[1, 2] * (1 + lambda.home[i]*rho)
  temp[2, 1] <- temp[2, 1] * (1 + lambda.away[i]*rho)
  temp[2, 2] <- temp[2, 2] * (1 - rho)
  temp
})

#####
prob.draw <- sapply(score.mats, function(x){
  sum(diag(x))
})
prob.away <- sapply(score.mats, function(x){
  sum(x[upper.tri(x)])
})
prob.home <- sapply(score.mats, function(x){
  sum(x[lower.tri(x)])
})
# Validation ####
val <- data.frame(predict(mult.model, 
                          dat.pred %>% 
                            filter(Season > 2015, week >= weeks[1] & week <= weeks[2]), 
                          type = 'probs'), 
                  predict(ord.model, 
                          dat.pred %>% 
                            filter(Season > 2015, week >= weeks[1] & week <= weeks[2]), 
                          type = 'probs'),
                  predict(gbm.model,
                          dat.pred %>%
                            filter(Season > 2015, week >= weeks[1] & week <= weeks[2]),
                          type = 'response')[,,1],
                  prob.away, prob.draw, prob.home,
                  dat.pred %>% 
                    filter(Season > 2015, week >= weeks[1], week <= weeks[2]) %>% 
                    select(final) %>% 
                    mutate(final = ifelse(final < 0, 1, ifelse(final == 0, 2, 3))))

# Classification error
mean(apply(val[,1:3], 1, which.max) == val$final)
mean(apply(val[,4:6], 1, which.max) == val$final)
mean(apply(val[,7:9], 1, which.max) == val$final)
mean(apply(val[,10:12], 1, which.max) == val$final)


# Entropy error
-sum(apply(val[,c(1:3, 13)], 1, function(x) log(x[x[4]])))
-sum(apply(val[,c(4:6, 13)], 1, function(x) log(x[x[4]])))
-sum(apply(val[,c(7:9, 13)], 1, function(x) log(x[x[4]])))
-sum(apply(val[,c(10:12, 13)], 1, function(x) log(x[x[4]])))

# Refit Poisson Model
model.home <- glm(form.home,
                  data = dat.pred %>% filter(week >= weeks[1], week <= weeks[2]),
                  family = poisson)

model.away <- glm(form.away,
                  data = dat.pred %>% filter(week >= weeks[1], week <= weeks[2]),
                  family = poisson)

# Tune

# Save files ####
save(model.home, model.away, file = 'IgnoreList/UnivariatePoissonModels.Rdata')
