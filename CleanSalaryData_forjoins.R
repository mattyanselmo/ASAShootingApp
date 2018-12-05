# Clean salary data to join to other stats
library(dplyr)
library(stringr)

saldat <- readRDS("AppData/SalaryData.rds")
saldat <- saldat %>%
  mutate(Player = trimws(paste0(First, " ", Last)),
         Season = as.numeric(format(Date, "%Y"))) %>%
  group_by(Player, Season) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  mutate(Player = str_replace_all(Player,
                                  c("AJ Cochran" = "A.J. Cochran",
                                    "AJ DeLaGarza" = "A.J. DeLaGarza",
                                    "Kenny Mansally" = "Abdoulie Mansally")))


# Testing
playerpassing <- readRDS("IgnoreList/xPassingByPlayer.rds")
playerpassing <- playerpassing %>%
  left_join(saldat, 
            by = c("passer" = "Player", "Season"))

sort(unique(playerpassing$passer[is.na(playerpassing$Base)]))
