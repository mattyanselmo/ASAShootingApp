# File to create shot tables for Shiny app

library(dplyr)
library(gbm)
library(stringr)
teamnames <- read.csv('TeamNameLinks.csv', stringsAsFactors = F) %>%
  select(-one_of("X"))

path <- ifelse(file.exists("C:/Users/Matthias"), "C:/Users/Matthias", "C:/Users/Matthias.Kullowatz")

temp <- read.csv(paste0(path, "/Dropbox/ASA Blog Data/", year, " Stats/raw passes.csv"))
write.csv(temp, paste0(path, "/Documents/GitHub/ASAShootingApp_development/IgnoreList/raw passes ", year, ".csv"))
write.csv(temp, paste0(path, "/Documents/GitHub/ASAShootingApp_master/IgnoreList/raw passes ", year, ".csv"))
rm(temp)
gc()


#load in the requisite data
passes <- bind_rows(lapply(paste0("IgnoreList/", grep('raw passes', list.files("IgnoreList/"), value = T)),
                           function(x) read.csv(x, stringsAsFactors = F))) %>%
  select(-one_of("X")) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         year = as.numeric(format(date, "%Y"))) %>%
  mutate(passer = str_replace_all(passer, 
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
         passer = ifelse(row_number() %in% grep("Boniek", passer), "Oscar Boniek Garcia", 
                         ifelse(passer == "Eddie Johnson" & year == 2011, "Eddie Johnson (no, not that one)", 
                                ifelse(passer == "Jose Hernandez" & team == "ATL" & year == 2018, "Jose Rafael Hernandez", 
                                       passer)))) %>%
  select(-one_of("X"))

vertical.lineups <- read.csv('IgnoreList/vertical starting lineups.csv', stringsAsFactors = FALSE) %>%
  select(-one_of("X"))
jy.starting.lineups <- read.csv('IgnoreList/Starting Lineups editedJY.csv', stringsAsFactors = FALSE) %>%
  select(-one_of("X"))


#########################################################

# Now get passing ratio numbers for each team
#passes <- data.frame(read.csv('raw passes.csv'))
#passPercFor <- group_by(passes, team) %>%
#   summarize(finalThirdPer = sum((x > 66.7 | endX > 66.7) & success == 1)/sum(success == 1))

#passPercAgainst <- group_by(passes, team.1) %>%
#    summarize(finalThirdAPer = sum((x > 66.7 | endX > 66.7) & success == 1)/sum(success == 1))


#write.csv(passPerc, 'passing_percentages.csv')x


# Create key1
vertical.lineups$Key1 <- paste(vertical.lineups$formation, vertical.lineups$position, sep = "")

# Create key2
vertical.lineups$Key2 <- paste(vertical.lineups$gameID, vertical.lineups$players, sep = "")

# Create key2 for passes df
passes$Key2 <- paste(passes$gameID, passes$passer, sep = "")

# Merge lineups with their proper positions
merged.lineups <- vertical.lineups %>%
  left_join(jy.starting.lineups, by = c("Key1" = "Key")) %>%
  filter(!duplicated(Key2))

# Merge lineups with positions
merged.passes <- left_join(passes, 
                           merged.lineups %>%
                             select(-c(team, gameID)), 
                           by = "Key2")

merged.passes <- merged.passes %>%
  left_join(teamnames, by = c('team' = 'FullName')) %>%
  left_join(teamnames, by = c('team.1' = 'FullName'), suffix = c("_0", "_1")) %>%
  left_join(teamnames, by = c("hteam" = "FullName")) %>%
  left_join(teamnames, by = c("ateam" = "FullName"), suffix = c("_h", "_a")) %>%
  mutate(team = Abbr_0,
         team.1 = Abbr_1,
         hteam = Abbr_h,
         ateam = Abbr_a) %>%
  select(-c(Abbr_0, Abbr_1, Abbr_h, Abbr_a))

# Engineer features ####

# Fill NA positions
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

merged.passes <- merged.passes %>%
  group_by(passer) %>%
  mutate(Position = ifelse(is.na(Position) | Position == "S", na.omit(c(Mode(Position[Position != "S"]), "S"))[1], Position)) %>% # Edit this?
  ungroup() %>%
  mutate(Position.model = factor(ifelse(Position == "G", "GK", "Field")))

merged.passes <- merged.passes %>%
  mutate(endX = endX*115/100,
         x = x*115/100,
         endY = endY*80/100,
         y = y*80/100,
         distance = sqrt((endX - x)^2 + (endY - y)^2),
         angle = atan((endY - y)/(endX - x)) + pi*ifelse(endX < x & endY < y, -1, ifelse(endX < x & endY > y, 1, 0)),
         angle = ifelse(is.na(angle), 0, angle),
         year = as.numeric(as.character(year)),
         playerdiff = ifelse(team == hteam, hplayers - aplayers, aplayers - hplayers),
         minute.temp = unlist(sapply(strsplit(time, ":"), function(x) as.numeric(x[1]))),
         second.temp = unlist(sapply(strsplit(time, ":"), function(x) as.numeric(x[2]))),
         minute = minute.temp + second.temp/60) %>%
  mutate_at(.vars = names(merged.passes)[sapply(merged.passes, class) == "character"],
            .funs = factor) %>%
  group_by(gameID, half) %>%
  arrange(minute) %>%
  mutate(first.pass = ifelse(row_number() == 1, 1, 
                             ifelse(hscore != lag(hscore) | ascore != lag(ascore), 1, 0)),
         second.pass = ifelse(first.pass == 1, 0, 
                              ifelse(row_number() == 2, 1, 
                                     ifelse(team == lag(team) & (hscore != lag(hscore, 2) | ascore != lag(ascore, 2)), 1, 0)))) %>%
  select(-c(minute.temp, second.temp)) %>%
  ungroup()


success.gbm <- readRDS("IgnoreList/xPassModel.rds")
merged.passes[["success.pred"]] <- predict(success.gbm, merged.passes, type = "response", n.trees = 1000)

merged.passes <- merged.passes %>%
  select(-c(hteam, ateam, final, hplayers, aplayers, 
            teamEventId, Key2, position, Formation, Player, players,
            Key1)) %>%
  #   mutate(type = ifelse(ifelse(throughball == 1, "through",
  #                               ifelse(freekick == 1, "freekick",
  #                                      ifelse(corner == 1, "corner", )))))
  group_by(passer) %>%
  mutate(typical.pos = Mode(Position)) %>%
  ungroup()

playerpos <- merged.passes %>% 
  group_by(passer, year, team) %>% 
  summarize(season.pos = as.character(Mode(Position)),
            typical.pos = as.character(typical.pos[1])) %>%
  mutate(season.pos = ifelse(season.pos == "S", typical.pos, season.pos)) %>%
  select(-typical.pos) %>%
  ungroup()

saveRDS(playerpos, "IgnoreList/playerpositions_byseason.rds")

merged.passes <- merged.passes %>%
  left_join(playerpos, by = c("passer", "year", "team"))

saveRDS(merged.passes, "IgnoreList/AllPassingData.rds")
write.csv(merged.passes, "IgnoreList/AllPassingData.csv", row.names = F)

for(tm in unique(merged.passes$team)){
  write.csv(merged.passes %>%
              filter(team == tm) %>%
              select(eventID, team, team.1, passer, recipient, date, x, endX, y, endY, success, success.pred),
            paste0(path, 
                   "/Google Drive/Soccer Statistics and Research/ASA Blog/Analysis/Data/Shared data/xPassData_",
                   tm, "_allyears.csv"), 
            row.names = F)
}

for(yr in unique(merged.passes$year)){
  write.csv(merged.passes %>%
              filter(year == yr) %>%
              select(eventID, team, team.1, passer, recipient, date, x, endX, y, endY, success, success.pred),
            paste0(path, 
                   "/Google Drive/Soccer Statistics and Research/ASA Blog/Analysis/Data/Shared data/xPassData_",
                   yr, "_allteams.csv"), 
            row.names = F)
}