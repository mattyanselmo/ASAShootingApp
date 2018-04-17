# Review xPass model

library(dplyr)
library(gbm)
library(stringr)
library(ggplot2)
library(scales)

passes <- readRDS("IgnoreList/AllPassingData.rds")
smod <- readRDS("IgnoreList/xPassModel.rds")
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

# By field zone
# By general direction/field zone