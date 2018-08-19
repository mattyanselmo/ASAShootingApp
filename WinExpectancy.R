# Gather win expectancy data
library(dplyr)

# Convert passing data to win expectancy table ####
passing <- readRDS("IgnoreList/AllPassingData.rds")
winexp <- passing %>%
  filter(!(minute > 46 & half == 1)) %>%
  # mutate(minute.temp = unlist(sapply(strsplit(as.character(time), ":"), function(x) as.numeric(x[1]))),
  #        second.temp = unlist(sapply(strsplit(as.character(time), ":"), function(x) as.numeric(x[2]))),
  #        minute = minute.temp + second.temp/60) %>%
  group_by(gameID, half) %>%
  arrange(minute) %>% 
  filter(row_number() %in% sapply(seq(0, ifelse(max(minute) < 95, 45, 50), 5) + 45*(half[1] - 1), function(x) which.min(abs(minute - x)))) %>%
  mutate(minute = ifelse(half == 1, pmin(45, round(minute*2, -1)/2), pmax(46, round(minute*2, -1)/2))) %>%
  ungroup() %>%
  select(gameID, half, minute, hscore, ascore, hfinal, afinal, playerdiff, home) %>%
  mutate(Hpts = 3*(hfinal > afinal) + (hfinal == afinal),
         Apts = 3*(hfinal < afinal) + (hfinal == afinal),
         playerdiff = ifelse(home == 1, playerdiff, -playerdiff),
         playerdiff = ifelse(playerdiff > 0, "Home",
                             ifelse(playerdiff == 0, "Neutral", "Away")))

winexpgrouped <- winexp %>%
  group_by(minute,
           scorediff = hscore - ascore,
           playerdiff) %>%
  summarize(N = n(),
            ActWin = mean(hfinal > afinal),
            ActTie = mean(hfinal == afinal),
            ActLose = mean(hfinal < afinal)) %>%
  ungroup()

library(splines)
logitW <- glm(Hpts == 3 ~ I(hscore - ascore)*ns(minute, 5) + playerdiff + minute:playerdiff, 
             data = winexp, 
             family = binomial)

logitT <- glm(Hpts == 1 ~ I(hscore - ascore)*ns(minute, 5) + playerdiff + minute:playerdiff, 
              data = winexp %>% filter(Hpts < 3), 
              family = binomial)

tab <- expand.grid(Minute = seq(0, 95, 5), HomeLead = -3:3, playerdiff = c("Home", "Neutral", "Away")) %>%
  mutate(hscore = 3,
         ascore = hscore - HomeLead,
         minute = Minute) %>% 
  left_join(winexpgrouped %>% select(minute, scorediff, playerdiff, N, ActWin, ActTie, ActLose),
            by = c("Minute" = "minute", "HomeLead" = "scorediff", "playerdiff")) %>%
  mutate(Games = ifelse(is.na(N), 0, N)) %>%
  select(-N)

tab[["Win"]] <- predict(logitW, tab, type = "response")
tab[["Tie"]] <- (1 - tab$Win)*predict(logitT, tab, type = "response")
tab[["Lose"]] <- 1 - tab$Win - tab$Tie

# Append shooting data ####
shooting <- shooting %>% filter(year >= 2015)
shooting$time <- ifelse(shooting$half == 1, pmin(45, shooting$time), pmax(46, shooting$time))

# Why is this 176400 in length?
HomexG <- sapply(1:nrow(tab), 
              function(x){
                gameids <- unique(winexp$gameID[winexp$minute == tab$Minute[x] &
                                                    winexp$hscore - winexp$ascore == tab$HomeLead[x] &
                                                  winexp$playerdiff == tab$playerdiff[x]])
                sum((shooting %>% 
                    filter(gameID %in% gameids,
                           time >= tab$Minute[x],
                           team == hteam))$xGShooter)/tab$Games[x]
              })

AwayxG <- sapply(1:nrow(tab), 
                 function(x){
                   gameids <- unique(winexp$gameID[winexp$minute == tab$Minute[x] &
                                                     winexp$hscore - winexp$ascore == tab$HomeLead[x] &
                                                     winexp$playerdiff == tab$playerdiff[x]])
                   sum((shooting %>% 
                          filter(gameID %in% gameids,
                                 time >= tab$Minute[x],
                                 team == ateam))$xGShooter)/tab$Games[x]
                 })

tab <- data.frame(tab, HomexG, AwayxG) %>%
  mutate(xGD = HomexG - AwayxG) %>%
  select(-minute)


write.csv(tab %>% select(-hscore, -ascore) %>% mutate(xPts = 3*Win + Tie), 
          "WinExpectancy.csv",
          row.names = F)

library(ggplot2)
library(reshape2)

tab %>% 
  filter(Minute == 90, playerdiff == "Neutral") %>%
  melt(measure.vars = c("Win", "Tie", "Lose")) %>%
  ggplot(aes(x = HomeLead)) + 
  geom_line(aes(y = value, color = variable), size = 1.5) +
  xlab("Home lead") + ylab("Outcome probability") + ggtitle("Home outcome probability (at minute 45)") +
  scale_color_discrete("Outcome") + ylim(c(0, 1))

tab %>% 
  filter(HomeLead == 0, playerdiff == "Neutral") %>%
  melt(measure.vars = c("Win", "Tie", "Lose")) %>%
  ggplot(aes(x = Minute)) + 
  geom_line(aes(y = value, color = variable), size = 1.5) +
  xlab("Minute") + ylab("Outcome probability") + ggtitle("Home outcome probability (tie game)") +
  scale_color_discrete("Outcome") + ylim(c(0, 1))