
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('Modules/xGoalsPlayers.R')
source('Modules/xGPlayersTable.R')
source('Modules/wrangle.R')
source('Modules/outputs.R')

shinyServer(function(input, output, session) {
  
  # Create tab redirect ####
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query[["headnavbar"]])){
      updateNavbarPage(session, "headnavbar", selected = query[["headnavbar"]])
    }
  })
  
  shooter_inputs <- callModule(xGoalsPlayers, "shooting")
  # output$testing <- renderText({shooter_inputs$teamfilter})
  # values do get updated by filter
  
  # Shooter tables ####
  dt_total <- reactive({get_dt_total(shooter_inputs)})
  # output$testing <- renderText({class(dt_total())})
  # output$test_table <- DT::renderDataTable({head(dt_total())})
  # dt_total is getting updated by the shooter_input filters
  
  output$shooter_table_total <- DT::renderDataTable({xGPlayersTable(dt_total())})

})