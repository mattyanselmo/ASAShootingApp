
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('Modules/xGPlayersSidebar.R')
source('Modules/xGPlayersScatter.R')
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
  
  ## Shooter inputs -----------------------------------------------------------
  shooter_inputs <- callModule(xGPlayersSidebar, "shooting")
  # output$testing <- renderText({shooter_inputs$teamfilter})
  # values do get updated by filter
  
  ## Shooter tables -----------------------------------------------------------
  dt_total <- reactive({get_dt_total(shooter_inputs)})
  # output$testing <- renderText({class(dt_total())})
  # output$test_table <- DT::renderDataTable({head(dt_total())})
  # dt_total is getting updated by the shooter_input filters
  output$shooter_table_total <- DT::renderDataTable({xGPlayersTotalTable(dt_total())})
  
  dt_per96 <- reactive({get_dt_per96(shooter_inputs)})
  # output$testing <- renderText({class(dt_per96())})
  # output$test_table <- DT::renderDataTable({head(dt_per96())})
  output$shooter_table_per96 <- DT::renderDataTable({xGPlayersPer96Table(dt_per96())})

  ## Shooter scatter plot -----------------------------------------------------
  # output$testing <- renderText({shooter_inputs$seasonfilter})
  shooter_scatter <- callModule(xGPlayersScatter,
                                'shooter_scatter',
                                dt_total = dt_total(),
                                dt_per96 = dt_per96(),
                                shooter_inputs = shooter_inputs)
  
  ##  Shooter downloads -------------------------------------------------------
  # Should put this into a function to use more easily across pages
  output$player_download <- downloadHandler(
    filename = paste0("ASAshootertable.csv"),
    
    content = function(file){
      namesFL <- as.data.frame(do.call("rbind", strsplit(sub(" ", ";", dt_playershootingplot()$Player), ";")))
      names(namesFL) <- c("First", "Last")
      write.csv(data.frame(namesFL, dt_playershootingplot(), check.names = FALSE), file, row.names = FALSE)
    }
  )
  
  ## Team shot inputs ---------------------------------------------------------
  
  
  ## Team shot tables ---------------------------------------------------------
  
  ## Team shot scatter plots --------------------------------------------------
  
  
  ## Team shots downloads -----------------------------------------------------
})