# Gather win expectancy data
library(dplyr)

passing <- readRDS("IgnoreList/AllPassingData.rds")

winexp <- passing %>%
  mutate(minute.temp = unlist(sapply(strsplit(as.character(time), ":"), function(x) as.numeric(x[1]))),
         second.temp = unlist(sapply(strsplit(as.character(time), ":"), function(x) as.numeric(x[2]))),
         minute = minute.temp + second.temp/60) %>%
  group_by(gameID, half) %>%
  arrange(minute) %>% 
  filter(row_number() %in% sapply(seq(0, 50, 2.5) + 45*(half[1] - 1), function(x) which.min(abs(minute - x)))) %>%
  ungroup() %>%
  select(gameID, half, minute, hscore, ascore, hfinal, afinal, playerdiff) %>%
  mutate(Hpts = 3*(hfinal > afinal) + (hfinal == afinal),
         Apts = 3*(hfinal < afinal) + (hfinal == afinal),
         playerdiff = -playerdiff)

winexpgrouped <- winexp %>%
  group_by(minute = round(minute), scorediff = hscore - ascore, playerdiff) %>%
  summarize(N = n(),
            ActWin = mean(hfinal > afinal),
            ActTie = mean(hfinal == afinal),
            ActLose = mean(hfinal < afinal))

library(splines)
logitW <- glm(Hpts == 3 ~ I(hscore - ascore)*ns(minute, 5) + playerdiff, 
             data = winexp, 
             family = binomial)

logitT <- glm(Hpts == 1 ~ I(hscore - ascore)*ns(minute, 5) + playerdiff, 
              data = winexp %>% filter(Hpts < 3), 
              family = binomial)

tab <- expand.grid(Minute = seq(0, 95, 5), HomeLead = -3:3, PlayerAdvantage = c("Home", "Neutral", "Away")) %>%
  mutate(hscore = 3,
         ascore = hscore - HomeLead,
         playerdiff = ifelse(PlayerAdvantage == "Home", 1,
                             ifelse(PlayerAdvantage == "Neutral", 0, -1)),
         minute = Minute)

tab[["Win"]] <- predict(logitW, tab, type = "response")
tab[["Tie"]] <- (1 - tab$Win)*predict(logitT, tab, type = "response")
tab[["Lose"]] <- 1 - tab$Win - tab$Tie

write.csv(tab %>% select(-hscore, -ascore) %>% mutate(xPts = 3*Win + Tie), 
          "WinExpectancy.csv",
          row.names = F)

library(ggplot2)
library(reshape2)

tab %>% 
  filter(Minute == 90, PlayerAdvantage == "Neutral") %>%
  melt(measure.vars = c("Win", "Tie", "Lose")) %>%
  ggplot(aes(x = HomeLead)) + 
  geom_line(aes(y = value, color = variable), size = 1.5) +
  xlab("Home lead") + ylab("Outcome probability") + ggtitle("Home outcome probability (at minute 45)") +
  scale_color_discrete("Outcome")

tab %>% 
  filter(HomeLead == 0, PlayerAdvantage == "Neutral") %>%
  melt(measure.vars = c("Win", "Tie", "Lose")) %>%
  ggplot(aes(x = Minute)) + 
  geom_line(aes(y = value, color = variable), size = 1.5) +
  xlab("Home lead") + ylab("Outcome probability") + ggtitle("Home outcome probability (at minute 45)") +
  scale_color_discrete("Outcome")