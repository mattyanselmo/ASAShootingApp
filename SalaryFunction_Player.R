# Summarize player salaries

# library(dplyr)
# salary.data <- read.csv("SalaryByPlayer_MLS.csv") %>% mutate(Date = "2018-05-01")
# team <- "POR"
# position <- "F"
# extract.date1 <- "2018-04-30"
# extract.date2 <- "2018-05-01"

player.salary.func <- function(salary.data = salary.data,
                        team,
                        position,
                        extract.date1,
                        extract.date2){
  
 temp <- salary.data %>%
   filter(Team %in% team,
          Pos %in% position | is.na(Pos),
          Date >= extract.date1 & Date <= extract.date2)
 
 temp
}

# library(dplyr)
# salary.data <- read.csv("SalaryByPlayer_MLS.csv") %>% mutate(Date = "2018-05-01")
# player.salary.func(salary.data = salary.data,
#                    team = "POR",
#                    position = "F",
#                    extract.date1 = "2000-01-01",
#                    extract.date2 = "2020-01-01")