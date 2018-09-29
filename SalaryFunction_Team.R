# Summarize team salaries

# library(dplyr)
# salary.data <- read.csv("SalaryByPlayer_MLS.csv") %>% mutate(Season = 2018)
# grouping <- c("Team")
# season <- 2018

salary.func <- function(salary.data = salary.data,
                        grouping,
                        season){
  
  temp <- salary.data %>%
    filter(Season %in% season)
  
  temp %>%
    group_by_(grouping) %>%
    summarize(AvgGuar = mean(Guaranteed),
              MedGuar = median(Guaranteed),
              StdDevGuar = sd(Guaranteed)) %>%
    ungroup() %>% 
    arrange(desc(AvgGuar))
}