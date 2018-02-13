# File to create shot tables for Shiny app

library(dplyr)

#load in the requisite data
passes <- bind_rows(lapply(paste0("IgnoreList/", grep('raw passes', list.files("IgnoreList/"), value = T)),
                           function(x) read.csv(x, stringsAsFactors = F))) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         year = format(date, "%Y"))

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
  left_join(jy.starting.lineups, by = c("Key1" = "Key"))

# Merge lineups with positions
# merged.passes <- merge(passes, merged.lineups, by.x = 'Key2', by.y = 'Key2')
merged.passes <- left_join(passes, 
                           merged.lineups %>%
                             select(-c(team, gameID)), 
                           by = "Key2")
# This merge above isn't quite exact. Somehow I wind up with like 50 more passes - because of the double counting of key2 - so I don't know where that's coming in

# Engineer features
merged.passes <- merged.passes %>% # include first pass of half indicator?
  mutate(endX = endX*115/100,
         x = x*115/100,
         endY = endY*80/100,
         y = y*80/100,
         distance = sqrt((endX - x)^2 + (endY - y)^2),
         angle = atan((endY - y)/(endX - x)) + pi*ifelse(endX < x & endY < y, -1, ifelse(endX < x & endY > y, 1, 0)),
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

# Build terminal x,y prediction model ####

# With censored obs
# library(gbm)
# surv.obj <- Surv(time = merged.passes$distance, 
#                  event = merged.passes$success)
# set.seed(23)
# surv.gbm <- gbm(surv.obj ~ home + x + y + angle + Position + 
#                   freekick + headpass + longball + throwin + throughball + 
#                   hplayers + aplayers + year,
#                 data = merged.passes,
#                 distribution = "coxph",
#                 n.trees = 1000,
#                 interaction.depth = 4,
#                 shrinkage = 0.01,
#                 n.minobsinnode = 20, 
#                 train.fraction = 0.7)
# 
# plot(surv.gbm$valid.error)
# min(surv.gbm$valid.error)
# which.min(surv.gbm$valid.error)
# summary(surv.gbm, plotit = F)

# With only successful passes
# library(gbm)
# set.seed(23)
# distance.gbm <- gbm(distance ~ x + y + angle + Position + 
#                   freekick + headpass + longball + throwin + throughball,
#                 data = merged.passes %>%
#                   filter(success == 1),
#                 distribution = "gaussian",
#                 n.trees = 200,
#                 interaction.depth = 5,
#                 shrinkage = 0.05,
#                 n.minobsinnode = 10, 
#                 train.fraction = 1)
# 
# merged.passes[["distance.pred"]] <- predict(distance.gbm, merged.passes, n.trees = 200)
# merged.passes <- merged.passes %>%
#   mutate(max.dist = 200, # placeholder for maximum distance the ball could travel in bounds; need some tricky geometry
#     distance.adj = ifelse(success == 1, distance,
#                            pmin(max.dist, distance.pred)))

# Build pass success model
# Include first pass of the half indicator!
library(gbm)
set.seed(17)
success.gbm.distance <- gbm(success ~ home + playerdiff + x + y + angle + Position + 
                              freekick + headpass + longball + throwin + throughball + 
                              cross + corner + playerdiff + distance + first.pass,
                            data = merged.passes,
                            distribution = "bernoulli",
                            n.trees = 1000,
                            interaction.depth = 5,
                            shrinkage = 0.1,
                            n.minobsinnode = 20, 
                            train.fraction = 1,
                            keep.data = F)

set.seed(21)
success.gbm <- gbm(success ~ home + playerdiff + x + y + angle + Position + 
                     freekick + headpass + longball + throwin + throughball + 
                     cross + corner + playerdiff + first.pass,
                   data = merged.passes,
                   distribution = "bernoulli",
                   n.trees = 1000,
                   interaction.depth = 5,
                   shrinkage = 0.1,
                   n.minobsinnode = 20, 
                   train.fraction = 1,
                   keep.data = F)

saveRDS(success.gbm, "IgnoreList/xPassModel.rds")
saveRDS(success.gbm.distance, "IgnoreList/xPassModel_withDistance.rds")

#merged.passes[["success.pred.distance"]] <- predict(success.gbm.distance, merged.passes, type = "response", n.trees = 1000)
merged.passes[["success.pred"]] <- predict(success.gbm, merged.passes, type = "response", n.trees = 1000)

merged.passes <- merged.passes %>%
  select(-c(eventID, hteam, ateam, final, hplayers, aplayers, 
            teamEventId, Key2, position, Formation, Player, players,
            Key1, second.pass))

library(stringr)
merged.passes <- merged.passes %>%
  mutate(passer = str_replace_all(passer, 
                                  c('Kazaishvili' = 'Qazaishvili', 
                                    'Jorge Villafaña' = 'Jorge Villafana',
                                    "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea")))

saveRDS(merged.passes, "IgnoreList/AllPassingData.rds")



