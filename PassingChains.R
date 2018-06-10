# library(tidyverse)
# 
# # create function to add "chainID" column to passes
# # input is data frame of passes from /just one game/
# # Creates unqiue ID with 1, then counting up
# # Final chainID column contains gameID, making it unique across database
# 
# 
# ### EXAMPLE
# 
# # Add chain IDs to all 2018 passes
# # code splits file into list of games, applies pass chain funcion with lapply, rejoins results
# # could take 1-3 min to run
# 
# # library(tidyverse)
# # setwd("your Dropbox path/ASA/2018 Stats")
# # passes18 <- read_csv("raw passes.csv")
# # passes18 <- bind_rows(lapply(split(passes17, f = passes18$gameID), add_pass_chains))
# 
# 
# add_pass_chains <- function(passes){
#   
#   # arrange passes by time
#   passes <- arrange(passes, time)
#   
#   # set ID column
#   passes$temp <- 1
#   
#   # set ID
#   id <- 1
#   
#   # must start with 2nd row to compare with 1st which is already in first chain
#   for(a in 2:nrow(passes)){
#     
#     # conditional if statement to change chain ID
#     if(passes$team[a] != passes$team[a-1] |
#        passes$freekick[a] == 1 |
#        passes$corner[a] == 1 |
#        passes$throwin[a] == 1){
#       id <- id + 1
#     }
#     
#     # add current ID to column for row a
#     passes$temp[a] <- id
#   }
#   
#   passes$chainID <- paste(as.character(passes$gameID), as.character(passes$temp), sep = "-")
#   passes$temp <- NULL
#   
#   passes
# }
# 
