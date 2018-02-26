# File to create shot tables for Shiny app

library(dplyr)
library(gbm)
library(stringr)
teamnames <- read.csv('TeamNameLinks.csv', stringsAsFactors = F)

#load in the requisite data
passes <- bind_rows(lapply(paste0("IgnoreList/", grep('raw passes', list.files("IgnoreList/"), value = T)),
                           function(x) read.csv(x, stringsAsFactors = F))) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         year = as.numeric(format(date, "%Y")))

vertical.lineups <- read.csv('IgnoreList/vertical starting lineups.csv', stringsAsFactors = FALSE)
jy.starting.lineups <- read.csv('IgnoreList/Starting Lineups editedJY.csv', stringsAsFactors = FALSE)


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

# Progress: If we go back to 2015, it looks like 5 formations are missing from Jared's position analysis
# 3511, 3421, 3142, 343d, and 3331

# Merge lineups with their proper positions
# merged.lineups <- merge(jy.starting.lineups, vertical.lineups, by.x = 'Key', by.y = 'Key1')
merged.lineups <- vertical.lineups %>%
  left_join(jy.starting.lineups, by = c("Key1" = "Key")) %>%
  filter(!duplicated(Key2))

# Merge lineups with positions
# merged.passes <- merge(passes, merged.lineups, by.x = 'Key2', by.y = 'Key2')
merged.passes <- left_join(passes, 
                           merged.lineups %>%
                             select(-c(team, gameID)), 
                           by = "Key2")
# This merge above isn't quite exact. Somehow I wind up with like 50 more passes - because of the double counting of key2 - so I don't know where that's coming in

merged.passes <- merged.passes %>%
  mutate(passer = str_replace_all(passer, 
                                  c('Kazaishvili' = 'Qazaishvili', 
                                    'Jorge Villafaña' = 'Jorge Villafana',
                                    "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea"))) %>%
  left_join(teamnames, by = c('team' = 'FullName')) %>%
  left_join(teamnames, by = c('team.1' = 'FullName')) %>%
  mutate(team = Abbr.x,
         team.1 = Abbr.y) %>%
  select(-c(Abbr.x, Abbr.y))

# Engineer features ####

# Fill NA positions
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

merged.passes <- merged.passes %>%
  group_by(passer) %>%
  mutate(Position = ifelse(is.na(Position) | Position == "S", na.omit(c(Mode(Position[Position != "S"]), "S"))[1], Position)) %>%
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
  mutate(first.pass = ifelse(row_number() == 1, 1, 0),
         second.pass = ifelse(row_number() == 2, 1, 0)) %>%
  select(-c(minute.temp, second.temp)) %>%
  ungroup()


success.gbm <- readRDS("IgnoreList/xPassModel.rds")
merged.passes[["success.pred"]] <- predict(success.gbm, merged.passes, type = "response", n.trees = 1000)

merged.passes <- merged.passes %>%
  select(-c(eventID, hteam, ateam, final, hplayers, aplayers, 
            teamEventId, Key2, position, Formation, Player, players,
            Key1, second.pass)) %>%
#   mutate(type = ifelse(ifelse(throughball == 1, "through",
#                               ifelse(freekick == 1, "freekick",
#                                      ifelse(corner == 1, "corner", )))))
  group_by(passer) %>%
  mutate(typical.pos = Mode(Position)) %>%
  ungroup()



saveRDS(merged.passes, "IgnoreList/AllPassingData.rds")
write.csv(merged.passes, "IgnoreList/AllPassingData.csv", row.names = F)