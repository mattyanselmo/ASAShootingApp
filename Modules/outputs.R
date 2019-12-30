# Date: 2019-10-01
# Updated: 2019-12-29
# These functions configure the final displayed tables

# This preps the team xg totals dataframe for display
xGTeamPerGameTable <- function(dataset, input, conference){
  dt <- dataset
  if('Conf' %in% names(dt)){
    dt <- dt %>%
      filter(Conf == conference) %>%
      select(-Conf)
  } else{
    dt <- dt
  }
  
  if(input$team_advanced == "Basic stats"){
    columns.perc1 <- c('SoT%F', 'SoT%A', 'Finish%F', 'Finish%A')
    columns.dec1 <- c("ShtF", "ShtA", "SoTF", "SoTA")
    columns.dec2 <- c("GF", "GA", "GD", "Pts", "xPts", "Gini18")
  } else{
    columns.perc1 <- c() #c("Solo%F", "Solo%A")
    columns.dec1 <- c("ShtF", "ShtA")
    columns.dec2 <- c("xGF", "xGA", "xGD", "GF", "GA", "GD", "GD-xGD", "TSR", "Pts", "xPts", "Gini18")
  }
  
  DT::datatable(dt,
                rownames = F,
                options = list(autoWidth = T,
                               pageLength = 25,
                               dom = 'ft')) %>%
    formatPercentage(columns = columns.perc1, digits = 1) %>%
    formatRound(columns = columns.dec1, digits = 1) %>%
    formatRound(columns = columns.dec2, digits = 2) %>%
    formatCurrency(columns = c("Comp ($MM)"),
                   currency = "$",
                   interval = 3,
                   mark = ",",
                   digits = 1)
}

# This preps the team xg totals dataframe for display
xGTeamTotalTable <- function(dataset, input, conference){
  dt <- dataset
  if('Conf' %in% names(dt)){
    dt <- dt %>%
      filter(Conf == conference) %>%
      select(-Conf)
  }
  
  if(input$team_advanced == "Basic stats"){
    columns.perc1 <- c('SoT%F', 'SoT%A', 'Finish%F', 'Finish%A')      
    columns.dec1 <- c("Pts", "xPts")[c(input$team_homeadjusted == "Home-adjusted", T)]
    columns.dec1.2 <- c("ShtF", "ShtA", "SoTF", "SoTA", "GF", "GA", "GD")[rep(input$team_homeadjusted == "Home-adjusted", 7)]
    columns.dec2 <- c("Gini18")
  } else{
    columns.perc1 <- c() #c("Solo%F", "Solo%A")
    columns.dec1 <- c("xGF", "xGA", "xGD", "GD-xGD", "xPts", "Pts")[c(T, T, T, T, T, input$team_homeadjusted == "Home-adjusted")]
    columns.dec1.2 <- c("ShtF", "ShtA", "GF", "GA", "GD")[rep(input$team_homeadjusted == "Home-adjusted", 5)]
    columns.dec2 <- c("TSR", "Gini18")
  }
  
  if(!input$team_conferenceview | length(input$team_seasonfilter) > 1){
    DT::datatable(dt,
                  rownames = F,
                  options = list(autoWidth = T,
                                 pageLength = 25)) %>%
      formatPercentage(columns = columns.perc1, digits = 1) %>%
      formatRound(columns = columns.dec1, digits = 1) %>%
      formatRound(columns = columns.dec1.2, digits = 1) %>%
      formatRound(columns = columns.dec2, digits = 2) %>%
      formatCurrency(columns = c("Comp ($MM)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 1) 
  } else{
    DT::datatable(dt,
                  rownames = F,
                  options = list(autoWidth = T,
                                 pageLength = 25,
                                 dom = 't')) %>%
      formatPercentage(columns = columns.perc1, digits = 1) %>%
      formatRound(columns = columns.dec1, digits = 1) %>%
      formatRound(columns = columns.dec1.2, digits = 1) %>%
      formatRound(columns = columns.dec2, digits = 2) %>%
      formatCurrency(columns = c("Comp ($MM)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 1)
    
  }
}
# This preps the shooter totals dataframe for display
xGPlayersTotalTable <- function(dataset){
  # Shooter datatable prep ####
  datatable(dataset %>% select(-c(Dist.key, `xG/shot`:`A-xA/pass`)),
              rownames = F,
              options = list(autoWidth = T,
                             pageLength = 25,
                             lengthMenu = seq(25, 100, 25))) %>%
      formatRound(columns = c('Dist', 'xG', 'G-xG', 'xPlace', 'xA', 'A-xA', 'xG+xA'), 
                  digits = 1) %>%
      formatRound(columns = c("PA", "xPA"), 
                  digits = 2) %>%
      formatPercentage(columns = c('Solo'), digits = 1) %>%
      formatCurrency(columns = c("Comp ($K)"),
                     currency = "$",
                     interval = 3,
                     mark = ",",
                     digits = 0)
  
}

# This preps the shooter per96 table for display
xGPlayersPer96Table <- function(dataset){
  datatable(dataset %>% select(-one_of("extreme", "plotnames")),
            rownames = F,
            options = list(autoWidth = T,
                           pageLength = 25,
                           lengthMenu = seq(25, 100, 25))) %>%
    formatRound(columns = c("Shots", "SoT", "G", "xG", "xPlace", "G-xG", 
                            "KeyP", "A", "xA", "A-xA", "xG+xA", "PA", "xPA"), 
                digits = 2)  %>%
    formatCurrency(columns = c("Comp ($K)"),
                   currency = "$",
                   interval = 3,
                   mark = ",",
                   digits = 0)
}

