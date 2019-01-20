# Summarize player salaries

# library(dplyr)
# salary.data <- readRDS("AppData/SalaryData.rds")
# teamfilter <- "POR"
# posfilter <- "F"
# extract.date1 <- "2011-01-01"
# extract.date2 <- "2018-10-01"

player.salary.func <- function(salary.data = salary.data,
                               teamfilter,
                               posfilter,
                               extract.date1,
                               extract.date2,
                               aggregate = F){
  
  temp <- salary.data %>%
    filter(Team %in% teamfilter,
           Pos %in% posfilter | is.na(Pos),
           Date >= extract.date1 & Date <= extract.date2)
  
  if(aggregate){
    temp %>%
      mutate(Season = as.numeric(format(Date, "%Y"))) %>%
      group_by(Player = paste0(First, " ", Last), Season) %>% 
      filter(row_number() == n()) %>% 
      group_by(Player) %>% 
      summarize(Team = paste0(unique(Team), collapse = ","),
                Last = Last[1],
                First = First[1],
                Records = n(), 
                Seasons = paste0(min(Season), " - ", max(Season)), 
                `Guar Total` = sum(Guaranteed, na.rm = T), 
                `Guar Avg` = mean(Guaranteed, na.rm = T)) %>%
      ungroup() %>%
      select(-Player) %>%
      arrange(desc(`Guar Avg`))
  } else{
    temp %>%
      arrange(desc(Guaranteed))
  }
}

# library(dplyr)
# salary.data <- readRDS("AppData/SalaryData.rds")
# player.salary.func(salary.data = salary.data,
#                    teamfilter = "POR",
#                    posfilter = "F",
#                    extract.date1 = "2000-01-01",
#                    extract.date2 = "2020-01-01",
#                    aggregate = T)