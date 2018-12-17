# Combine salary files
library(dplyr)
library(xlsx)
library(stringr)

# Bind files ####
files <- list.files("IgnoreList/SalaryData")

temp <- lapply(files,
               function(x) read.xlsx(paste0("IgnoreList/SalaryData/", x),
                                     sheetName = "Sheet1",
                                     stringsAsFactors = F) %>%
                 mutate(Date = as.Date(paste0(substr(x, 1, 4), "-", substr(x, 5, 6), "-", substr(x, 7, 8)))))

salary.data <- bind_rows(temp)

# Clean data ####
salary.data.clean <- salary.data %>%
  mutate(Club = ifelse(Club %in% c("No Team", "None", "Unassigned", "Pool", "POOL") | is.na(Club), "NONE", Club))

teamnamelinks <- read.csv("TeamNameLinks_Salaries.csv", stringsAsFactors = F)

salary.data.clean <- salary.data.clean %>%
  left_join(teamnamelinks, by = c("Club" = "Team")) %>%
  mutate(Club = Abbr) %>%
  select(-Abbr)

salary.data.clean <- salary.data.clean %>%
  rename(Team = Club) %>%
  mutate(Pos = ifelse(Pos %in% c("D-F", "D-M", "D/F", "D/M", "M-D", "M/D"), "B",
                      ifelse(Pos %in% c("F-M", "F/M", "M-F", "M/F"), "A",
                             ifelse(Pos %in% c("MF"), "M", Pos))))

# Player names ####
salary.data.clean <- salary.data.clean %>%
  mutate(Last = str_replace_all(Last, 
                                c("De Lima Junior" = "de Lima",
                                "Carreiro da Silva" = "Carreiro",
                                "Brilliant" = "Brillant",
                                "Dos Santos" = "dos Santos")),
         First = str_replace_all(First,
                                 c("JeVaughn" = "Je-Vaughn",
                                 "Jevaughn" = "Je-Vaughn")),
         Last = ifelse(is.na(First), 
                       Last,
                       ifelse(First == "Fabinho" | Last == "Fabinho", 
                              "Fabinho", 
                              ifelse(Last %in% c("Junior", "Dos Santos") & First == "Gilberto", 
                                     "Souza", 
                                     ifelse((First == "Hassan" & Last == "N'dam") | (First == "Hassan Ndam" & Last == "Fouapon"), 
                                            "Ndam",
                                            ifelse(First == "Juan" & Last == "David Cabezas", 
                                                   "Cabezas",
                                                   ifelse(First == "Juan" & Last == "Edgardo Ramirez",
                                                          "Ramirez",
                                                          Last)))))),
         First = ifelse(First == "Fabinho" | Last == "Fabinho", 
                        NA,
                        ifelse(First == "Harrison" & Last == "Shipp", 
                               "Harry", 
                               ifelse((First == "Hassan" & Last == "N'dam") | (First == "Hassan Ndam" & Last == "Fouapon"), 
                                      "Hassan",
                                      ifelse(First == "Joshua" & Last == "Williams", "Josh",
                                      First)))))

# Clean salary data to join to other stats
saldat <- salary.data.clean %>%
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

saveRDS(saldat, "AppData/SalaryData.rds")
