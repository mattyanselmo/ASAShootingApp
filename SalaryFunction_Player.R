# Summarize player salaries

# library(dplyr)
# salary.data <- readRDS("AppData/SalaryData.rds")
# team <- "POR"
# position <- "F"
# extract.date1 <- "2017-04-01"
# extract.date2 <- "2018-05-01"

player.salary.func <- function(salary.data = salary.data,
                        teamfilter,
                        posfilter,
                        extract.date1,
                        extract.date2){
  
 temp <- salary.data %>%
   filter(Team %in% teamfilter,
          Pos %in% posfilter | is.na(Pos),
          Date >= extract.date1 & Date <= extract.date2)
 
 temp %>%
   arrange(desc(Guaranteed))
}

# library(dplyr)
# salary.data <- readRDS("AppData/SalaryData.rds")
# player.salary.func(salary.data = salary.data,
#                    teamfilter = "POR",
#                    posfilter = "F",
#                    extract.date1 = "2000-01-01",
#                    extract.date2 = "2020-01-01")