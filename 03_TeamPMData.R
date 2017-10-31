dat <- readRDS('IgnoreList/AllShotsData2011-2017.rds')
library(MASS)
library(dplyr)
library(tidyr)
library(gbm)
library(nnet)

dat.for <- dat %>%
  mutate(Season = format(date, '%Y'),
         xG_keeper = ifelse(is.na(xGKeeper), 0, xGKeeper)) %>%
  group_by(Season) %>%
  mutate(week = floor((as.numeric(date) - min(as.numeric(date)))/7) + 1) %>%
  group_by(Season, team, date) %>%
  summarize(week = week[1],
            hteam = hteam[1],
            ateam = ateam[1],
            xGF_team = sum(xGTeam),
            xGF_shooter = sum(xGShooter),
            xGF_gk = sum(xG_keeper),
            GF = sum(result == 'Goal'),
            final = final[1]) %>%
  ungroup() %>% 
  group_by(Season, team) %>%
  arrange(date) %>%
  mutate(xGF_season_team = lag(cumsum(xGF_team)),
         xGF_season_shooter = lag(cumsum(xGF_shooter)),
         xGF_season_gk = lag(cumsum(xGF_gk)),
         xGF_last10_team = sapply(sapply(1:n(), function(x) xGF_team[pmax(0, x - 10):(x - 1)]), mean),
         xGF_last10_gk = sapply(sapply(1:n(), function(x) xGF_gk[pmax(0, x - 10):(x - 1)]), mean),
         xGF_last10_shooter = sapply(sapply(1:n(), function(x) xGF_shooter[pmax(0, x - 10):(x - 1)]), mean),
         GF_season = lag(cumsum(GF)),
         GF_last10 = sapply(sapply(1:n(), function(x) GF[pmax(0, x - 10):(x - 1)]), mean)) %>%
  ungroup()

dat.against <- dat %>%
  mutate(Season = format(date, '%Y'),
         xG_keeper = ifelse(is.na(xGKeeper), 0, xGKeeper)) %>%
  group_by(Season) %>%
  mutate(week = floor((as.numeric(date) - min(as.numeric(date)))/7) + 1) %>%
  group_by(Season, team.1, date) %>%
  summarize(week = week[1],
            xGA_team = sum(xGTeam),
            xGA_shooter = sum(xGShooter),
            xGA_gk = sum(xG_keeper),
            GA = sum(result == 'Goal')) %>%
  ungroup() %>% 
  group_by(Season, team.1) %>%
  arrange(date) %>%
  mutate(xGA_season_team = lag(cumsum(xGA_team)),
         xGA_season_shooter = lag(cumsum(xGA_shooter)),
         xGA_season_gk = lag(cumsum(xGA_gk)),
         xGA_last10_team = sapply(sapply(1:n(), function(x) xGA_team[pmax(0, x - 10):(x - 1)]), mean),
         xGA_last10_gk = sapply(sapply(1:n(), function(x) xGF_gk[pmax(0, x - 10):(x - 1)]), mean),
         xGA_last10_shooter = sapply(sapply(1:n(), function(x) xGF_shooter[pmax(0, x - 10):(x - 1)]), mean),
         GA_season = lag(cumsum(GA)),
         GA_last10 = sapply(sapply(1:n(), function(x) GA[pmax(0, x - 10):(x - 1)]), sum)) %>%
  ungroup()

dat.pred <- dat.for %>%
  full_join(dat.against, by = c('team' = 'team.1', 'Season', 'date', 'week'), suffix = c('_for', '_against'))

# Make dataset for generating predictions
pred.data <- dat.pred %>% 
  group_by(Season, team) %>% 
  arrange(date) %>% 
  mutate(gamesplayed = sapply(1:n(), function(x) sum(team == team[x]  & date < date[x]))) %>%
  summarize_at(.vars = vars(c(week, xGF_team:gamesplayed)), 
               .funs = function(x) tail(x, 1))

dat.pred <- dat.pred %>%
  filter(team == hteam) %>%
  group_by(Season) %>%
  mutate(gamesplayedH = sapply(1:n(), function(x) sum((hteam == hteam[x] | ateam == hteam[x]) & date < date[x])),
         gamesplayedA = sapply(1:n(), function(x) sum((hteam == ateam[x] | ateam == ateam[x]) & date < date[x]))) %>%
  ungroup() %>%
  select(hteam, ateam, date, gamesplayedH, gamesplayedA) %>%
  left_join(dat.pred %>% 
              select(Season, team, date, xGF_team:GA_last10), 
            by = c('hteam' = 'team', 'date')) %>%
  left_join(dat.pred %>%
              select(team, date, xGF_team:GA_last10),
            by = c('ateam' = 'team', 'date'),
            suffix = c('_home', '_away')) %>%
  filter(!is.na(hteam)) %>%
  group_by(Season) %>%
  mutate(week = floor(as.numeric(date - min(date))/7) + 1) %>%
  ungroup()

dat.pred <- dat.pred %>%
  mutate_at(.vars = names(dat.pred)[!(names(dat.pred) %in% c(grep('season', names(dat.pred), value = T),
                                                             grep('last10', names(dat.pred), value = T)))],
            .funs = function(x) ifelse(is.na(x), 0, x)) %>%
  mutate_at(.vars = grep('home', grep('season', names(dat.pred), value = T), value = T),
            .funs = funs(./gamesplayedH)) %>%
  mutate_at(.vars = grep('away', grep('season', names(dat.pred), value = T), value = T),
            .funs = funs(./gamesplayedA)) %>%
  group_by(Season, hteam) %>%
  arrange(date) %>%
  fill_(grep('home', grep('season', names(test), value = T), value = T), .direction = 'up') %>%
  ungroup() %>%
  group_by(Season, ateam) %>%
  arrange(date) %>%
  fill_(grep('away', grep('season', names(test), value = T), value = T), .direction = 'up') %>%
  ungroup() %>%
  mutate(final = final_home,
         outcome = ifelse(final < 0, 0, ifelse(final == 0, 1, 3))) %>%
  select(-c(final_home, final_away)) %>%
  mutate(Season = as.numeric(Season))

# Multinomial logistic model ####
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
                   GF_season_home + GA_season_home  +
                   GF_season_away + GA_season_away +
                   GF_last10_home + GA_last10_home  +
                   GF_last10_away + GA_last10_away)

ord.model <- polr(formula = form, 
                   data = dat.pred %>% filter(Season <= 2015, week >= weeks[1], week <= weeks[2]))

mult.model <- multinom(formula = form,
                       data = dat.pred %>% filter(Season <= 2015, week >= weeks[1], week <= weeks[2]))

gbm.model <- gbm(formula = form,
                 data = dat.pred %>% filter(Season <= 2015, week >= weeks[1], week <= weeks[2]),
                 distribution = 'multinomial',
                 n.trees = 1000,
                 interaction.depth = 4,
                 n.minobsinnode = 15,
                 shrinkage = 0.001,
                 cv.folds = 10)

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
                dat.pred %>% 
                  filter(Season > 2015, week >= weeks[1], week <= weeks[2]) %>% 
                  select(final) %>% 
                  mutate(final = ifelse(final < 0, 1, ifelse(final == 0, 2, 3))))

# Classification error
mean(apply(val[,1:3], 1, which.max) == val$final)
mean(apply(val[,4:6], 1, which.max) == val$final)
mean(apply(val[,7:9], 1, which.max) == val$final)


# Entropy error
-sum(apply(val[,c(1:3, 10)], 1, function(x) log(x[x[4]])))
-sum(apply(val[,c(4:6, 10)], 1, function(x) log(x[x[4]])))
-sum(apply(val[,c(7:9, 10)], 1, function(x) log(x[x[4]])))


# Save files ####
saveRDS(dat.pred, 'IgnoreList/TeamPMData.rds')
saveRDS(gbm.model, 'IgnoreList/GBMModel_weeks25-35.rds')
saveRDS(pred.data,'IgnoreList/TeamPredictionsData_game34.rds')
saveRDS(mult.model, 'IgnoreList/MultinomModel_weeks25-35.rds')

# Build bivariate Poisson model
form.home <- formula(GF_home ~ xGF_season_team_home + 
                     I(GF_season_home - xGF_season_team_home) +
                     xGA_season_team_away +
                     I(GA_season_away - xGA_season_team_away))

form.away <- formula(GF_away ~ xGF_season_team_away + 
                       I(GF_season_away - xGF_season_team_away) +
                       xGA_season_team_home +
                       I(GA_season_home - xGA_season_team_home))
rho = 0.01
model.home <- glm(form.home,
                  data = dat.pred %>% filter(week >= weeks[1], week <= weeks[2]),
                  family = poisson)

model.away <- glm(form.away,
                  data = dat.pred %>% filter(week >= weeks[1], week <= weeks[2]),
                  family = poisson)

hteam.lambda <- predict(model, data.frame(home.adv = 1, team = hteam, opponent = ateam), type = 'response')
ateam.lambda <- predict(model, data.frame(home.adv = 0, team = ateam, opponent = hteam), type = 'response')
hteam.dist <- dpois(0:8, lambda = hteam.lambda)
ateam.dist <- dpois(0:8, lambda = ateam.lambda)
score.matrix <- hteam.dist %o% ateam.dist
## do the adjustments for independence
score.matrix[1,1] <- score.matrix[1,1]*(1-hteam.lambda*ateam.lambda*rho)
score.matrix[1,2] <- score.matrix[1,2]*(1+hteam.lambda*rho)
score.matrix[2,1] <- score.matrix[2,1]*(1+ateam.lambda*rho)
score.matrix[2,2] <- score.matrix[2,2]*(1-rho)
#####
draw.prob <- sum(diag(score.matrix))
away.prob <- sum(score.matrix[upper.tri(score.matrix)])
home.prob <- sum(score.matrix[lower.tri(score.matrix)])

