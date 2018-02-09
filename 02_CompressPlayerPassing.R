# Condense player passing data for web app

# Read dataset ####


## balance predictions to actual by zone
merged.passes.dt <- data.table(merged.passes)
merged.passes.dt[x < 33.33, section := 'Def']
merged.passes.dt[x >= 33.33 & x < 66.67, section := 'Mid']
merged.passes.dt[x >= 66.67, section := 'Att']
balance.factors <- merged.passes.dt[, .(bf = mean(success)/mean(score, na.rm = TRUE)), by = 'section']
merged.merged.passes <- merge(merged.passes.dt, balance.factors, by = 'section')
merged.merged.passes[, final.score := score * bf]

write.csv(merged.merged.passes, 'Scored Passes.csv')

player.stats <- merged.merged.passes[, .(actual = mean(success), expected = mean(final.score, na.rm = TRUE), .N), by = c('passer', 'section', 'team.x')]
player.stats[, diff := actual-expected]
att.stats <- player.stats[section == 'Att', ]
def.stats <- player.stats[section == 'Def', ]
mid.stats <- player.stats[section == 'Mid', ]

att.mid <- merge(att.stats, mid.stats, by = c('passer', 'team.x'), all.x = TRUE, all.y = TRUE)
att.def.mid <- merge(att.mid, def.stats, by = c('passer', 'team.x'), all.x = TRUE, all.y = TRUE)


colnames(att.def.mid) <- c('Player', 'Team', 'section.x', 'Att Third %', 'Att Third Exp. %', 'Att Third Attempts', 'Att Third % Diff', 'section.y', 'Mid Third %', 'Mid Third Exp. %', 'Mid Third Attempts', 'Mid Third % Diff', 'section', 'Def. Third %', 'Def Third Exp. %', 'Def Third Attempts', 'Def Third % Diff')

att.def.mid$section.x <- NULL
att.def.mid$section.y <- NULL
att.def.mid$section <- NULL
att.def.mid[is.na(att.def.mid)] <- 0
team.lookup <- data.table('full.name' = c("Houston",
                                          "FC Dallas",
                                          "Seattle",
                                          "New York",
                                          "Salt Lake",
                                          "Minnesota United",
                                          "Columbus",
                                          "Philadelphia",
                                          "Montreal",
                                          "Colorado",
                                          "Atlanta United",
                                          "Toronto",
                                          "New York City FC",
                                          "DC United",
                                          "Vancouver",
                                          "Portland",
                                          "San Jose",
                                          "New England",
                                          "Orlando City",
                                          "L.A. Galaxy",
                                          "Chicago",
                                          "Kansas City"),
                          "short.name" = c("HOU",
                                           "DAL",
                                           "SEA",
                                           "NYRB",
                                           "RSL",
                                           "MN",
                                           "CLB",
                                           "PHI",
                                           "MTL",
                                           "COL",
                                           "ATL",
                                           "TOR",
                                           "NYC",
                                           "DC",
                                           "VAN",
                                           "POR",
                                           "SJ",
                                           "NE",
                                           "ORL",
                                           "LA",
                                           "CHI",
                                           "SKC"))
final <- merge(att.def.mid, team.lookup, by.x = 'Team', by.y = 'full.name')
final$Team <- NULL
colnames(final)[14] <- "Team"
fwrite(final, "../For Website/player_passing_scores.csv")