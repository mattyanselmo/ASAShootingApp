

xGPlayersTable <- function(dataset){
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