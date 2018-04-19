# Review xPass model

library(dplyr)
library(gbm)
library(stringr)
library(ggplot2)
library(scales)

passes <- readRDS("IgnoreList/AllPassingData.rds")
smod <- readRDS("IgnoreList/xPassModel.rds")
smod16 <- readRDS("IgnoreList/xPassModel_2016.rds")
passes[["success.pred.16"]] <- predict(smod16, passes, type = "response", n.trees = 1000)

# Partial plots ####
plot(smod, i.var = "angle", return.grid = T, type = "response", continuous.resolution = 25) %>%
  mutate(angle = -180*angle/pi) %>%
  filter(abs(angle) <= 160) %>%
  ggplot(aes(x = angle, y = y)) +
  geom_line(size = 2) +
  xlab("Angle (degrees)") + ylab("Completion probability") + ggtitle("Predicted pass success by angle") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + 
  labs(caption = "0 degrees implies forward; positive angles imply the passer's right") +
  theme(plot.caption = element_text(hjust = 0.5))

xy.tab <- plot(smod, i.var = c("x", "y"), return.grid = T, type = "response", continuous.resolution = 12)
names(xy.tab) <- c("x", "y", "pred")
xy.tab %>%
  ggplot(aes(x = x, y = y)) + 
  geom_tile(aes(fill = pred)) + 
  scale_fill_gradient2(low = "red", mid = "yellow", high = "forestgreen", midpoint = .7) +
  xlab("X coordinate (yards from own goal)") + ylab("Y coordinate (yards from right sideline)") +
  ggtitle("Predicted pass success by field position")

# Playerdiff
# Make table of indicators with observed/predicted rates

# Validation ####

# By position
pos.ae <- passes %>% 
  filter(year >= 2017) %>%
  group_by(season.pos) %>%
  summarize(Passes = n(),
            Act = sum(success)/Passes,
            Exp = sum(success.pred.16)/Passes,
            `A-E` = Act - Exp)

pos.ae %>%
  ggplot(aes(x = season.pos, y = `A-E`)) +
  geom_bar(stat = "identity") +
  geom_line(aes(y = 0)) +
  xlab("Position") +
  ggtitle("Actual vs. expected passing by player position")


# By gamestate
gs.ae <- passes %>% 
  filter(year >= 2017) %>%
  group_by(Gamestate = pmax(-4, pmin(4, ifelse(home == 1, hscore - ascore, ascore - hscore)))) %>%
  summarize(Passes = n(),
            Act = sum(success)/Passes,
            Exp = sum(success.pred.16)/Passes,
            `A-E` = Act - Exp)

gs.ae %>%
  ggplot(mapping = aes(Gamestate, `A-E`)) +
  geom_line(size = 2) +
  geom_line(aes(y = 0)) +
  ggtitle("Actual vs. expected passing by gamestate")

# By coordinates
xy.ae <- passes %>%
  filter(year >= 2017) %>%
  group_by(x = cut(x, 4), y = cut(y, 4)) %>%
  summarize(Passes = n(),
            Act = sum(success)/Passes,
            Exp = sum(success.pred.16)/Passes,
            `A-E` = Act - Exp)

xy.ae %>%
  ggplot(aes(x = x, y = y)) + 
  geom_tile(aes(fill = `A-E`)) + 
  scale_fill_gradient2(low = "red", mid = "forestgreen", high = "red", midpoint = 0) +
  xlab("X coordinate (yards from own goal)") + ylab("Y coordinate (yards from right sideline)") +
  ggtitle("Actual vs. expected passing by zone")

chi.test.stat <- sum((xy.ae$Act - xy.ae$Exp)^2*xy.ae$Passes/xy.ae$Exp)
(p.value <- pchisq(chi.test.stat, 16, lower.tail = F))

# By field zone
# By general direction/field zone