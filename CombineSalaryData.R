# Combine salary files
library(dplyr)
library(xlsx)

# Bind files ####
files <- list.files("IgnoreList/SalaryData")

temp <- lapply(files,
               function(x) read.xlsx(paste0("IgnoreList/SalaryData/", x),
                                     sheetName = "Sheet1",
                                     stringsAsFactors = F) %>%
                 mutate(Date = as.Date(paste0(substr(x, 1, 4), "-", substr(x, 5, 6), "-", substr(x, 7, 8)))))

salary.data <- bind_rows(temp)

# Clean data ####
salary.data <- salary.data %>%
  mutate(Club = ifelse(Club %in% c("No Team", "None", "Unassigned", "Pool", "POOL") | is.na(Club), "NONE", Club))

teamnamelinks <- read.csv("TeamNameLinks_Salaries.csv", stringsAsFactors = F)

salary.data <- salary.data %>%
  left_join(teamnamelinks, by = c("Club" = "Team")) %>%
  mutate(Club = Abbr) %>%
  select(-Abbr)

salary.data <- salary.data %>%
  rename(Team = Club) %>%
  mutate(Pos = ifelse(Pos %in% c("D-F", "D-M", "D/F", "D/M", "M-D", "M/D"), "B",
                      ifelse(Pos %in% c("F-M", "F/M", "M-F", "M/F"), "A",
                             ifelse(Pos %in% c("MF"), "M", Pos))))

saveRDS(salary.data, "AppData/SalaryData.rds")
