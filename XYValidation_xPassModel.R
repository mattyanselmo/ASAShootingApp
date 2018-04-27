source("CreatePitch.R")
library(dplyr)

ymax = 7315 #80 yards
xmax = 10516 #115 yards

passes <- readRDS("IgnoreList/AllPassingData.rds")
validation <- passes %>%
  group_by(x = cut(x, 6), y = cut(y, 6)) %>%
  summarize(Passes = n(),
            Act = sum(success)/Passes,
            Exp = sum(success.pred)/Passes,
            A.E = Act - Exp)

x.define=unlist(strsplit(as.character(validation$x), ","))
x.define=gsub("[^0-9\\.]", "", x.define) 
validation$xmin=as.numeric(x.define[seq(1, length(x.define), by=2)])*91.44
validation$xmin[1:3]=0
validation$maxX=as.numeric(x.define[seq(2, length(x.define), by=2)])*91.44

y.define=unlist(strsplit(as.character(validation$y), ","))
y.define=gsub("[^0-9\\.]", "", y.define) 
validation$ymin=as.numeric(y.define[seq(1, length(y.define), by=2)])*91.44
validation$ymin[c(1,5,9,13)] <- 0
validation$maxY <- as.numeric(y.define[seq(2, length(y.define), by=2)])*91.44
validation$maxY[validation$maxY > ymax] <- ymax

# Pass success plot
p <- createPitch(line_colour = "black", goal_colour = "black")+
  geom_rect(data = validation, aes(xmin = xmin, xmax = maxX, ymin = ymin, ymax = maxY, fill = Exp), alpha = 0.9) +
  scale_fill_gradient2("%", low = "red", high = "darkgreen", mid ="gold", midpoint = 0.65)+
  labs(title = "Model pass completion rates by zone")+
  annotate("segment", x = xmax/2 - 1000, xend = xmax/2+1000, y = 250, yend = 250, size = 2, arrow = arrow(length = unit(0.2, "cm"),type="closed"))+
  annotate("text", x = xmax/2, y = 550, label="Attacking Direction", size=5)+
  annotate("text", x = validation$maxX - 50, y = validation$maxY - 50, label = paste0(formatC(round(100*abs(validation$Exp), 2), digits = 1, format = "f"), "%"), hjust = 1, vjust = 1)+
  theme(plot.title = element_text(size=20))
p
# Validation plot
p <- createPitch(line_colour = "black", goal_colour = "black")+
  geom_rect(data = validation, aes(xmin = xmin, xmax = maxX, ymin = ymin, ymax = maxY, fill = A.E), alpha=0.9) +
  scale_fill_gradient2("Actual-Expected", low = "red", high = "red", mid ="darkgreen", midpoint = 0)+
  labs(title = "Actual vs. expected passing by zone")+
  annotate("segment", x = xmax/2 - 1000, xend = xmax/2+1000, y = 250, yend = 250, size = 2, arrow = arrow(length = unit(0.2, "cm"),type="closed"))+
  annotate("text", x = xmax/2, y = 550, label="Attacking Direction", size=5)+
  annotate("text", x = validation$maxX - 50, y = validation$maxY - 50, label = paste0(ifelse(validation$A.E < 0, "-", "+"), formatC(round(100*abs(validation$A.E), 2), digits = 1, format = "f"), "%"), hjust = 1, vjust = 1)+
  theme(plot.title = element_text(size=20))
p

ggsave(paste0('./AE_passing_map.png'), plot=p, height=(ymax+500)/1000, width=(xmax+1500)/1000, dpi=300, units='in' )