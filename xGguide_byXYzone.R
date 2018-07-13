source("CreatePitch.R")
library(dplyr)
load("IgnoreList/UpdatedModels_2018-02-03.Rdata")

ymax = 7315 #80 yards
xmax = 10516 #115 yards

plot.data <- expand.grid(x = seq(115/36, 35*115/36, 115/18),
                         y = seq(80/18, 17*80/18, 80/9)) %>%
  mutate(X = 115 - x,
         Y = y - 40,
    distance = sqrt(X^2 + Y^2),
         theta = atan(X/Y)*180/pi,
         slope1 = -abs(Y)/X,
         slope2 = X/(abs(Y) + 4),
         xpoint = -4*(slope1 + slope2)/(slope2 - slope1),
         ypoint = 115 - slope2*(xpoint + 4),
         available = sqrt((4 - xpoint)^2 + (115 - ypoint)^2),
         patternOfPlay.model = "Regular",
         year = 2017,
         bodypart = "Right foot",
         through = 0, 
         cross = 0)

plot.data$xG = predict(xgoal.model, plot.data, type = "response")

plot.data <- plot.data %>%
  mutate(xmin = (x - 115/36)*91.44,
         maxX = (x + 115/36)*91.44,
         ymin = (y - 80/18)*91.44,
         maxY = (y + 80/18)*91.44)

quantile.vals <- c(0, quantile(plot.data$xG, seq(0, 1, 0.05)), 1)
colors <- colorRampPalette(c("red", "gold", "forestgreen"))(23)


# Shot success plot
p <- createPitch(line_colour = "black", goal_colour = "black")+
  geom_rect(data = plot.data %>% filter(x > 115/2), aes(xmin = xmin, xmax = maxX, ymin = ymin, ymax = maxY, fill = xG), alpha = 0.7) +
  scale_fill_gradientn(colors = colors, values = quantile.vals, breaks = quantile.vals, guide = F) +
  ggtitle("xG Model scoring rates by zone") +
  annotate("text", x = plot.data$maxX - 100, y = plot.data$maxY - 350, label = paste0(formatC(round(100*abs(plot.data$xG), 2), digits = 1, format = "f"), "%"), hjust = 1, vjust = 1)+
  theme(plot.title = element_text(size=20, hjust = .1, vjust = -3))
p + xlim(c(5250, 10516))

p <- createPitch(line_colour = "black", goal_colour = "black")+
  geom_rect(data = plot.data, aes(xmin = xmin, xmax = maxX, ymin = ymin, ymax = maxY, fill = xG), alpha = 0.7) +
  scale_fill_gradientn(colors = colors, values = quantile.vals, breaks = quantile.vals, guide = F) +
  ggtitle("xG Model scoring rates by zone") +
  annotate("text", x = plot.data$maxX - 100, y = plot.data$maxY - 350, label = paste0(formatC(round(100*abs(plot.data$xG), 2), digits = 1, format = "f"), "%"), hjust = 1, vjust = 1)+
  theme(plot.title = element_text(size=20, hjust = .1, vjust = -3))
p + xlim(c(0, 10516))


ggsave(paste0('./AE_passing_map.png'), plot=p, height=(ymax+500)/1000, width=(xmax+1500)/1000, dpi=300, units='in' )