# Condense player passing data for web app

# Read dataset ####
library(dplyr)
library(stringr)
merged.passes <- readRDS("IgnoreList/AllPassingData.rds")
teamnames <- read.csv('TeamNameLinks.csv', stringsAsFactors = F)

if(file.exists('C:/Users/Matthias')){
  temp <- read.csv(paste0("C:/Users/Matthias/Dropbox/ASA Blog Data/", year, " Stats/touches.csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_development/IgnoreList/touches ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias/Documents/GitHub/ASAShootingApp_master/IgnoreList/touches ", year, ".csv"))
  rm(temp)
  gc()
} else if(file.exists('C:/Users/Matthias.Kullowatz')){
  temp <- read.csv(paste0("C:/Users/Matthias.Kullowatz/Dropbox/ASA Blog Data/", year, " Stats/touches.csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_development/IgnoreList/touches ", year, ".csv"))
  write.csv(temp, paste0("C:/Users/Matthias.Kullowatz/Documents/GitHub/ASAShootingApp_master/IgnoreList/touches ", year, ".csv"))
  rm(temp)
  gc()
}

touches <- bind_rows(lapply(grep('touches', list.files("IgnoreList/"), value = T),
                            function(x) read.csv(paste0("IgnoreList/", x), stringsAsFactors = F))) %>%
  mutate(player = str_replace_all(player, 
                                  c("Alexandre De Lima" = "Alex",
                                    "Aly Ghazal" = "Ali Ghazal",
                                    "Ambroise Oyongo Bitolo" = "Ambroise Oyongo",
                                    "Andreas Ivanschitz" = "Andreas Ivan",
                                    "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea",
                                    "Carlo Franco Chueco del Rio" = "Carlo Chueca",
                                    "Christian Hernandez" = "Cristhian Hernandez",
                                    "Clinton Irwin" = "Clint Irwin",
                                    "Cristian Nazarith" = "Cristian Nazarit",
                                    "Eric Ayuk Mbu" = "Eric Ayuk",
                                    "Felipe Martins Campanholi" = "Felipe Martins",
                                    "Franck Pangop" = "Frantz Pangop",
                                    "Harrison Shipp" = "Harry Shipp",
                                    "Helbert (Fred) da Silva" = "Fred",
                                    "Ibrahim Diop" = "Birahim Diop",
                                    "Ismael Tajouri-Shradi" = "Ismael Tajouri",
                                    'Jorge Villafaña' = 'Jorge Villafana',
                                    "Jose Leonardo Ribeiro da Silva" = "Leonardo",
                                    'Kazaishvili' = 'Qazaishvili', 
                                    "Kim Kee-hee" = "Kim Kee-Hee",
                                    "Martin Perez Garcia" = "Matias Perez Garcia",
                                    "Matt VanOekel" = "Matt Van Oekel",
                                    "Maxine Chanot" = "Maxime Chanot",
                                    "Michel Garbini Pereira" = "Michel",
                                    "Miguel Lopez" = "Mikey Lopez",
                                    "Nicholas DePuy" = "Nick DePuy",
                                    "Nick Depuy" = "Nick DePuy",
                                    "Oriol Rosell Argerich" = "Oriol Rosell",
                                    "Samba" = "Sambinha",
                                    "Sebastian Ibeagha" = "Sebastien Ibeagha",
                                    "Vitor Gomes Pereira Junior" = "Juninho",
                                    "Yefferson Quintana" = "Yeferson Quintana"
                                  )),
         player = ifelse(row_number() %in% grep("Boniek", player), "Oscar Boniek Garcia", player)) %>%
  left_join(teamnames, by = c('team' = 'FullName')) %>%
  mutate(team = Abbr) %>%
  select(-Abbr)

#minutesPlayed_season <- readRDS("IgnoreList/MinutesBySeason.rds")
minutesPlayed_gameID <- readRDS("IgnoreList/MinutesByGameID.rds") %>%
  left_join(unique(touches %>%
                     select(gameID, date)) %>%
                     mutate(date = as.Date(date, "%m/%d/%Y"),
                            Season = as.numeric(format(date, "%Y"))),
            by = "gameID")

touches <- minutesPlayed_gameID %>%
  left_join(touches %>%
              select(player, gameID, touches), by = c("gameID", "player")) %>%
  mutate(touches = ifelse(is.na(touches), 0, touches)) %>%
  group_by(gameID, team) %>%
  mutate(TeamTouches = sum(touches)) %>%
  group_by(player, gameID) %>%
  mutate(touchpct = sum(touches)/sum((minutes*TeamTouches)/96))

## balance predictions to actual by zone
pass.summ <- merged.passes %>%
  mutate(third = ifelse(x < 115/3, "Def",
                        ifelse(x < 115*2/3, "Mid", "Att"))) %>%
  group_by(passer, date, team, third) %>%
  summarize(N = n(),
            successes = sum(success),
            exp = sum(success.pred),
            Position = season.pos[1],
            Distance = sum(distance[success == 1]),
            Vert.Dist = sum((endX - x)[success == 1])) %>%
  ungroup()

player.stats <- pass.summ %>% 
  left_join(touches %>% select(-team), by = c("passer" = "player", "date")) %>%
  mutate(Season = as.numeric(format(date, "%Y"))) %>%
  filter(!is.na(touches))

# Passing
pass_salary_mapping <- read.csv("SalaryNameLinkingTable_passing.csv")
playerpassing <- player.stats %>%
  left_join(saldat %>% 
              select(-c(First, Last, Team, Pos, Date)) %>%
              left_join(pass_salary_mapping, 
                        by = c("Player" = "salary_name", "Season")) %>%
              select(-Player), 
            by = c("passer" = "passing_name", "Season"))

saveRDS(playerpassing, "IgnoreList/xPassingByPlayer.rds")
write.csv(playerpassing, "IgnoreList/xPassingByPlayer.csv", row.names = F)

rm(player.stats, playerpassing)
gc()