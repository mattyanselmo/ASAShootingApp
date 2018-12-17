# Clean salary data to join to other stats
library(dplyr)
library(stringr)

saldat <- readRDS("AppData/SalaryData.rds")
saldat <- saldat %>%
  mutate(Player = ifelse(is.na(First), Last, trimws(paste0(First, " ", Last))),
         Player = ifelse(Player == "Eddie Johnson" & Team == "POR", "Eddie Johnson (no, not that one)", Player),
         Season = as.numeric(format(Date, "%Y"))) %>%
  group_by(Player, Season) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  mutate(Player = str_replace_all(Player,
                                  c("Aiden Daniels" = "Aidan Daniels",
                                    "Alhassan Lalas Abubakar" = "Lalas Abubakar",
                                    "Aly Ghazal" = "Ali Ghazal",
                                    "Ambroise Bitolo" = "Ambroise Oyongo",
                                    "Ambroise Bitolo Oyongo" = "Ambroise Oyongo",
                                    "Andrew Hainault" = "Andre Hainault",
                                    "Andreas Ivanschitz" = "Andreas Ivan",
                                    "Charles Sapong" = "CJ Sapong",
                                    "Daniel Gargan" = "Dan Gargan",
                                    "Daniel Califf" = "Danny Califf",
                                    "Dilaver Duka" = "Dilly Duka",
                                    "Dmitry Imbongo" = "Dimitry Imbongo",
                                    "Fafa Picault" = "Fabrice Picault",
                                    "Harrison Shipp" = "Harry Shipp",
                                    "Ibson da Silva" = "Ibson",
                                    "Jake Nerwinski" = "Jakob Nerwinski",
                                    "Jay Demerit" = "Jay DeMerit",
                                    "Jerry Bengston" = "Jerry Bengtson",
                                    "John Kennedy Hurtado" = "Jhon Kennedy Hurtado",
                                    "Jose Erick Correa" = "Jose Correa",
                                    "Jose Erik Correa" = "Jose Correa",
                                    "Jose E Hernandez" = "Jose Hernandez",
                                    "Joseph Villarreal" = "Jose Villarreal",
                                    "Juan David Cabezas" = "Juan Cabezas",
                                    "Julio Cesar" = "Julio Santos Correa",
                                    "Juninho Gomes" = "Juninho",
                                    "Leandro Pirez" = "Leandro Gonzalez Pirez",
                                    "Leonardo Da Silva" = "Leonardo",
                                    "Leonardo Da Silva Ribeiro" = "Leonardo",
                                    "Leonardo Ribeiro Da Silva" = "Leonardo",
                                    "Leonardo Ribeiro" = "Leonardo",
                                    "Matias Perez" = "Matias Perez Garcia",
                                    "Maxi Moralez" = "Maximiliano Moralez",
                                    "Luis Fernando" = "Maximiniano",
                                    "Maicon Dos Santos" = "Maicon dos Santos",
                                    "Maicon Santos" = "Maicon dos Santos",
                                    "Mamdou Danso" = "Mamadou Danso",
                                    "Matias Perez" = "Matias Perez Garcia",
                                    "Michael De Leeuw" = "Michael de Leeuw",
                                    "Michael Azira" = "Micheal Azira",
                                    "Mike Da Fonte" = "Mike da Fonte",
                                    "Mohammed El-Mounir" = "Mohamed El-Munir",
                                    "Mkhokheli Dube" = "Mkokheli Dube",
                                    "Kheli Dube" = "Mkokheli Dube",
                                    "Nana Attakora-Gyan" = "Nana Attakora",
                                    "Nemanja Vukovich" = "Nemanja Vukovic",
                                    "Nick Depuy" = "Nick DePuy",
                                    "Nikolaj Hansen" = "Niko Hansen",
                                    "Norbeto Paparatto" = "Norberto Paparatto",
                                    "Oscar Garcia" = "Oscar Boniek Garcia",
                                    "Oscar Boniek" = "Oscar Boniek Garcia",
                                    "Pele Van Anholt" = "Pele van Anholt",
                                    "Rais Mbolhi" = "Rais M'bolhi",
                                    "Richmond Laryea" = "Richie Laryea",
                                    "Richard Marquez" = "Richie Marquez",
                                    "Mamadu Cande" = "Sambinha",
                                    "Sebastian Le Toux" = "Sebastien Le Toux",
                                    "Simon Elliot" = "Simon Elliott",
                                    "Stephen Neumann" = "Steve Neumann",
                                    "Steven Clark" = "Steve Clark",
                                    "Steven Purdy" = "Steve Purdy",
                                    "Stiven Mendoza" = "Steven Mendoza",
                                    "Anthony Beltran" = "Tony Beltran",
                                    'Valeri "Vako" Qazaishvili' = "Valeri Qazaishvili",
                                    "Valeri Vako Qazaishvili" = "Valeri Qazaishvili",
                                    "VillafaÃ±a" = "Villafana",
                                    "Vincente Sanchez" = "Vicente Sanchez"
                                  )))

# Passing
pass_salary_mapping <- read.csv("SalaryNameLinkingTable_passing.csv")
playerpassing <- readRDS("IgnoreList/xPassingByPlayer.rds")
playerpassing <- playerpassing %>%
  left_join(saldat %>% 
              select(-c(First, Last, Team, Pos, Date)) %>%
              left_join(pass_salary_mapping, 
                        by = c("Player" = "salary_name", "Season")), 
            by = c("passer" = "passing_name", "Season")) %>%
  select(-Base, -Player)


# Creating first draft shooting mapping table
shot_salary_mapping <- read.csv("SalaryNameLinkingTable_shooting.csv")
playerxgoals <- readRDS('IgnoreList/xGoalsByPlayer.rds') 

playerxgoals <- playerxgoals %>%
  left_join(saldat %>% 
              select(-c(First, Last, Team, Pos, Date)) %>%
              left_join(shot_salary_mapping, 
                        by = c("Player" = "salary_name", "Season")), 
            by = c("shooter" = "shooting_name", "Season"))

write.csv(playerxgoals %>% select(shooter, Season, Base, Player) %>% unique(), file = "SalaryNameLinkingTable_shooting.csv", row.names = F)
