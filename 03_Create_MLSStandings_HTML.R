library(XML)
library(rvest)
library(dplyr)

# year <- 2018
# merged.passes <- readRDS("IgnoreList/AllPassingData.rds")
# shooting <- readRDS("IgnoreList/AllShotsData2011-2017.rds)

standings <- readHTMLTable(htmlParse(read_html("https://www.mlssoccer.com/standings")))

possession <- merged.passes %>% 
  filter(year == year) %>%
  group_by(team) %>%
  summarize(PassF = n()) %>%
  left_join(merged.passes %>%
              filter(year == year) %>%
              group_by(team.1) %>%
              summarize(PassA = n()),
            by = c("team" = "team.1")) %>%
  mutate(Poss = PassF/(PassF + PassA)) %>%
  ungroup()

shots <- shooting %>%
  filter(as.numeric(year) == year) %>%
  group_by(team) %>%
  summarize(GF = sum(result == "Goal"),
            ShtF = n(),
            OnTargetF = sum(result %in% c('Goal', 'Saved', 'Blocked off line'))) %>%
  left_join(shooting %>% 
              filter(as.numeric(year) == year) %>%
              group_by(team.1) %>%
              summarize(GA = sum(result == "Goal"),
                        ShtA = n(),
                        OnTargetA = sum(result %in% c('Goal', 'Saved', 'Blocked off line'))),
            by = c("team" = "team.1")) %>%
  mutate(TSR = ShtF/(ShtF + ShtA),
         PDO = 1000*(GF/OnTargetF + 1 - GA/OnTargetA)) %>%
  ungroup()

# Read in team name links (MLSStandings)
# Clean standings tab
# Join info by team to match MLS Standings tab on website
# Write E/W conference HTML files for Drew
              