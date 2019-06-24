# Function to plot win probability charts by game for app

# # Sample inputs ####
# library(mgcv)
# library(ggplot2)
# library(reshape2)
# library(scales)
# library(plotly)
# load("ASAShootingApp_master/AppData/WinExpectancyModels.Rdata")
# app <- "master"
# action.data <- readRDS(paste0("ASAShootingApp_", app, "/AppData/WinProbabilityActions_bygame.rds"))
# action.input <- action.data %>%
#   filter(gameID == 914862) %>%
#   filter(Action == "lastminute")
# winmodel <- winmodel.purged
# drawmodel <- drawmodel.purged

winprobchart.func <- function(action.input,
                              winmodel,
                              drawmodel){
  
  annotations <- data.frame(
    xpos = c(0),
    ypos =  c(0.95),
    annotateText = c("@AnalysisEvolved"))
  
  
  last.minute <- action.input$minute[action.input$Action == "lastminute"]
  hteam.fixed <- tail(action.input$hteam, 1)
  hteam.fixed <- ifelse(hteam.fixed == "", " ", hteam.fixed)
  ateam.fixed <- tail(action.input$ateam, 1)
  final.fixed <- tail(action.input$final, 1)
  date.fixed <- tail(action.input$date, 1)
  
  action.input <- action.input %>%
    filter(Action != "lastminute")
  
  plot.data <- data.frame(minute = 0,
                          half = 1,
                          playerdiff = 0,
                          gamestate = 0,
                          final = final.fixed) %>%
    bind_rows(data.frame(minute = c(1:45, 46:last.minute),
                         half = c(rep(1, 45), rep(2, last.minute - 45))) %>%
                left_join(action.input %>% select(-c(gameID, date)),
                          by = c("half", "minute"))) %>%
    arrange(half, minute) %>%
    mutate(playerdiff = na.locf(playerdiff),
           gamestate = na.locf(gamestate))
  
  plot.data[["Win"]] <- as.numeric(predict(winmodel.purged, plot.data, type = "response"))
  plot.data[["Draw"]] <- as.numeric((1 - plot.data$Win)*predict(drawmodel.purged, plot.data, type = "response"))
  plot.data[["Loss"]] <- as.numeric(1 - plot.data$Win - plot.data$Draw)
  
  plot.data <- plot.data %>%
    melt(id.vars = c("minute", "team"), measure.vars = c("Win", "Draw", "Loss")) %>% 
    arrange(variable, minute)
  
  plot.data <- plot.data %>% 
    mutate(variable = factor(ifelse(variable == "Win", 
                                    paste0(hteam.fixed, " win"),
                                    ifelse(variable == "Loss",
                                           paste0(ateam.fixed, " win"),
                                           "Draw")), 
                             levels = unique(c(paste0(hteam.fixed, " win"),
                                               "Draw",
                                               paste0(ateam.fixed, " win")))))
  
  p <- ggplot(plot.data, aes(x = minute, y = value, color =  variable, fill = variable))+
    geom_area(alpha = 0.5)+
    labs(y = "Probability", 
         x = "Minute", 
         title = paste0(hteam.fixed, " vs. ", ateam.fixed, "    ", date.fixed)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(15, 90, 15)) +
    scale_fill_manual("Outcome",
                      labels = c(paste0(hteam.fixed, " win"),
                                 "Draw",
                                 paste0(ateam.fixed, " win")),
                      values = c("#27AAE1",
                                 "#6b6c6d",
                                 "#BE1E2D")) +
    scale_color_manual("Outcome",
                       labels = c(paste0(hteam.fixed, " win"),
                                  "Draw",
                                  paste0(ateam.fixed, " win")),
                       values = c("#27AAE1",
                                  "#6b6c6d",
                                  "#BE1E2D")) +
    geom_vline(xintercept = 45, 
               color = "black", 
               linetype = "dotted")
  
  if(nrow(action.input) > 0){
    p +
      geom_vline(data = action.input, 
                 aes(xintercept = minute), 
                 color = "black", 
                 linetype = "dashed",
                 alpha = 0.25) +
      geom_text(data = action.input, 
                inherit.aes = FALSE, 
                aes(x=minute, 
                    y=0.02, 
                    label=paste(team, Action)), 
                hjust = 0, 
                vjust = 1, 
                angle = 90, 
                nudge_x = 1.2,
                color = "white")+
      theme_minimal() +
      theme(text = element_text(face = "plain", size = 14),
            axis.title.x = element_text(face = "plain", size = 14),
            axis.title.y = element_text(face = "plain", size = 14, margin = margin(0, 5, 0, 0)),
            plot.title = element_text(face = "plain", size = 18),
            axis.text.x = element_text(face = "plain", size = 12),
            axis.text.y = element_text(face = "plain", size = 12),
            axis.ticks.length = unit(0.15, "lines"),
            legend.title = element_text(face = "bold", size = 14),
            legend.position = "right",
            legend.text = element_text(face = "plain", size = 12),
            legend.key = element_rect(fill = "white"),
            legend.key.size = unit(1, "cm")) +
      geom_text(data = annotations, 
                inherit.aes = FALSE,
                aes(x=xpos,
                    y=ypos,
                    hjust = rep(0, nrow(annotations)),
                    label=annotateText),
                alpha = 0.5)
  } else{
    p + theme_minimal() +
      theme(text = element_text(face = "plain", size = 14),
            axis.title.x = element_text(face = "plain", size = 14),
            axis.title.y = element_text(face = "plain", size = 14, margin = margin(0, 5, 0, 0)),
            plot.title = element_text(face = "plain", size = 18),
            axis.text.x = element_text(face = "plain", size = 12),
            axis.text.y = element_text(face = "plain", size = 12),
            axis.ticks.length = unit(0.15, "lines"),
            legend.title = element_text(face = "bold", size = 14),
            legend.position = "right",
            legend.text = element_text(face = "plain", size = 12),
            legend.key = element_rect(fill = "white"),
            legend.key.size = unit(1, "cm")) +
      geom_text(data = annotations, 
                inherit.aes = FALSE,
                aes(x=xpos,
                    y=ypos,
                    hjust = rep(0, nrow(annotations)),
                    label=annotateText),
                color = "black",
                alpha = 0.5)
  }
}

# # Function testing ####
# library(mgcv)
# library(ggplot2)
# library(reshape2)
# library(scales)
# library(plotly)
# load("ASAShootingApp_master/AppData/WinExpectancyModels.Rdata")
# app <- "master"
# action.data <- readRDS(paste0("ASAShootingApp_", app, "/AppData/WinProbabilityActions_bygame.rds"))
# action.input <- action.data %>%
#   filter(gameID == 914862) %>%
#   filter(Action == "lastminute")
# 
# winprobchart.func(action.input,
#                   winmodel.purged,
#                   drawmodel.purged)
