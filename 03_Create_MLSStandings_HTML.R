library(XML)
library(rvest)
library(dplyr)

# year <- 2018
# merged.passes <- readRDS("IgnoreList/AllPassingData.rds")

shooting <- readRDS("IgnoreList/AllShotsData2011-2017.rds")
team.map <- read.csv("TeamNameLinks_MLSStandings.csv")

standings <- readHTMLTable(htmlParse(read_html("https://www.mlssoccer.com/standings")))
standings <- lapply(standings, function(x){names(x) <- as.character(as.matrix(x[1,])); x <- x[-1,]; x})
standings.east <- standings[[1]][,1:12] %>% select(`#`, Club, GP, W:`T`, GF:GD, Pts = PTS, PPG) %>%
  left_join(team.map, c("Club" = "Original")) %>%
  mutate(Club = Convert) %>%
  select(-Convert)
standings.west <- standings[[2]][,1:12] %>% select(`#`, Club, GP, W:`T`, GF:GD, Pts = PTS, PPG) %>%
  left_join(team.map, c("Club" = "Original")) %>%
  mutate(Club = Convert) %>%
  select(-Convert)

possession <- merged.passes %>% 
  mutate(Season = year) %>%
  select(-year) %>%
  filter(Season == year) %>%
  group_by(team) %>%
  summarize(PassF = n()) %>%
  left_join(merged.passes  %>% 
              mutate(Season = year) %>%
              select(-year) %>%
              filter(Season == year)  %>%
              group_by(team.1) %>%
              summarize(PassA = n()),
            by = c("team" = "team.1")) %>%
  mutate(Poss = PassF/(PassF + PassA)) %>%
  ungroup() %>%
  left_join(team.map, c("team" = "Original")) %>%
  mutate(team = Convert) %>%
  select(-Convert)

shots <- shooting  %>% 
  mutate(Season = year) %>%
  select(-year) %>%
  filter(Season == year) %>%
  group_by(team) %>%
  summarize(GF = sum(result == "Goal"),
            ShtF = n(),
            OnTargetF = sum(result %in% c('Goal', 'Saved', 'Blocked off line'))) %>%
  left_join(shooting %>% 
              mutate(Season = year) %>%
              select(-year) %>%
              filter(Season == year) %>%
              group_by(team.1) %>%
              summarize(GA = sum(result == "Goal"),
                        ShtA = n(),
                        OnTargetA = sum(result %in% c('Goal', 'Saved', 'Blocked off line'))),
            by = c("team" = "team.1")) %>%
  mutate(TSR = ShtF/(ShtF + ShtA),
         PDO = 1000*(GF/OnTargetF + 1 - GA/OnTargetA)) %>%
  ungroup() %>%
  left_join(team.map, c("team" = "Original")) %>%
  mutate(team = Convert) %>%
  select(-Convert)

# Join info by team to match MLS Standings tab on website
standings.east <- standings.east %>%
  left_join(shots, by = c("Club" = "team"), suffix = c("", "_xG")) %>%
  left_join(possession, by = c("Club" = "team"), suffix = c("", "_xG")) %>%
  mutate(GP = as.numeric(as.character(GP)), 
         AvShtF = ShtF/GP, 
         AvShtA = ShtA/GP,
         PPG = as.numeric(as.character(PPG))) %>%
  select(`#`, Club, GP, W, L, `T`, GF, GA, GD, ShtF, AvShtF, ShtA, AvShtA, TSR, PDO, Poss, Pts, PPG) %>%
  arrange(desc(PPG)) %>%
  mutate(`#` = 1:n())

standings.west <- standings.west %>%
  left_join(shots, by = c("Club" = "team"), suffix = c("", "_xG")) %>%
  left_join(possession, by = c("Club" = "team"), suffix = c("", "_xG")) %>%
  mutate(GP = as.numeric(as.character(GP)), 
         AvShtF = ShtF/GP, 
         AvShtA = ShtA/GP,
         PPG = as.numeric(as.character(PPG))) %>%
  select(`#`, Club, GP, W, L, `T`, GF, GA, GD, ShtF, AvShtF, ShtA, AvShtA, TSR, PDO, Poss, Pts, PPG) %>%
  arrange(desc(PPG)) %>%
  mutate(`#` = 1:n())

# Write conference tables to HTML ####
library(xtable)
path <- ifelse(file.exists("C:/Users/Matthias"), "C:/Users/Matthias", "C:/Users/Matthias.Kullowatz")

# Easter conference
output.east <- xtable(standings.east, 
                 digits = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 3, 0, 3, 0, 2),
                 align = rep("center", ncol(standings.east) + 1))
write.table(gsub("\n", "",
                 gsub(" <", "<", 
                      gsub("> ", ">", 
                           gsub("<table border=1>", '<script>
                                $(document).ready(function() {
                                $("#EasternConferenceTable").tablesorter();
                                });
                                </script> <b>Eastern Conference</b> <TABLE border=1 id="EasternConferenceTable" class="tablesorter" style="white-space:nowrap;"><thead>',
                                gsub(" PPG </th>  </tr>", " PPG </th>  </tr> </thead> <tbody>",
                                     print.xtable(output.east, 
                                                  type = "html",
                                                  include.rownames = F,
                                                  print.results = F)))))),
            file = paste0(path, "/Dropbox/ASA Blog Data/HTMLOutputs/MLSStandings_East_", year, ".txt"),
            row.names = F,
            quote = F)

# Western conference
output.west <- xtable(standings.west, 
                      digits = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 3, 0, 3, 0, 2),
                      align = rep("center", ncol(standings.west) + 1))
write.table(gsub("\n", "",
                 gsub(" <", "<", 
                      gsub("> ", ">", 
                           gsub("<table border=1>", '<script>
                                $(document).ready(function() {
                                $("#WesternConferenceTable").tablesorter();
                                });
                                </script> <b>Western Conference</b> <TABLE border=1 id="WesternConferenceTable" class="tablesorter" style="white-space:nowrap;"><thead>',
                                gsub(" PPG </th>  </tr>", " PPG </th>  </tr> </thead> <tbody>",
                                     print.xtable(output.west, 
                                                  type = "html",
                                                  include.rownames = F,
                                                  print.results = F)))))),
            file = paste0(path, "/Dropbox/ASA Blog Data/HTMLOutputs/MLSStandings_West_", year, ".txt"),
            row.names = F,
            quote = F)