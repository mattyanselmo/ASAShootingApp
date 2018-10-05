# Summarize team salaries

# library(dplyr)
# salary.data <- readRDS("AppData/SalaryData.rds")
# grouping <- c("Team")
# season <- 2018

team.salary.func <- function(salary.data = salary.data,
                        grouping,
                        seasonfilter){
  
  temp <- salary.data %>%
    mutate(Season = as.numeric(format(Date, "%Y"))) %>%
    filter(Season %in% seasonfilter)
  
  temp %>%
    group_by_(grouping) %>%
    summarize(N = sum(Date == max(Date)),
              TotalGuar = sum(Guaranteed[Date == max(Date)]),
              AvgGuar = mean(Guaranteed[Date == max(Date)]),
              MedGuar = median(Guaranteed[Date == max(Date)]),
              StdDevGuar = sd(Guaranteed[Date == max(Date)])) %>%
    ungroup() %>% 
    arrange(desc(TotalGuar))
}

# library(dplyr)
# team.salary.func(salary.data = readRDS("AppData/SalaryData.rds"),
#                  grouping = "Team",
#                  season = 2015) %>% data.frame()
