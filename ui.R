
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('Modules/xGoalsPlayers.R')
source('Modules/xGPlayersTable.R')

shinyUI(
  # Shooting navbar ####
  navbarPage(title = HTML('<b>ASA Database</b>'),
             theme = 'bootstrap_edited.css',
             id = "headnavbar",
             navbarMenu(strong('xGoals'),
                        # Shooting xGoals tab panel ####
                        tabPanel('Players',
                                 value = "playerxgoals",
                                 sidebarLayout(
                                   sidebarPanel(
                                     xGoalsPlayersUI('shooting')
                                   ),
                                   mainPanel(
                                     # div(style="display: inline-block;vertical-align:bottom; width: 250px;", h1('Player xGoals')),
                                     # div(style="display: inline-block;vertical-align:bottom; width: 250px;", downloadButton('player_download', 'Download CSV')),
                                     h1('Player xGoals'),
                                     p(paste0('Updated through games on ', max(as.Date(playerxgoals$date)))),
                                     downloadButton('player_download', 'Download CSV'),
                                     br(),
                                     tabsetPanel(id = 'player_subtab',
                                                 tabPanel('Tables: totals',
                                                          value = "tablestotals",
                                                          p(HTML("<i>Per-minutes data and position information only goes back to 2015. Related filters will disappear when seasons before 2015 are checked.</i>")),
                                                          #tags$head(tags$script(src = "www/tablesorter.js")),
                                                          # textOutput('testing')
                                                          # DT::dataTableOutput('test_table')
                                                          DT::dataTableOutput('shooter_table_total')
                                                         ),
                                                 tabPanel('Tables: per 96',
                                                          value = "tablesper96",
                                                          p(HTML("<i>Per-minutes data and position information only goes back to 2015.</i>")),
                                                          DT::dataTableOutput('shooter_table_per96')
                                                 ),
                                                 tabPanel('Scatter plots',
                                                          value = "plots",
                                                          p(HTML("<i>Per-minutes data and position information only goes back to 2015. Please allow a few seconds for the plot to load.</i>")),
                                                          fluidPage(fluidRow(
                                                            column(3,
                                                                   selectInput('shooterplot_xvar',
                                                                               label = 'X-axis variable',
                                                                               choices = "xG",
                                                                               selected = "xG")),
                                                            column(3,
                                                                   selectInput('shooterplot_yvar',
                                                                               label = 'Y-axis variable',
                                                                               choices = "G",
                                                                               selected = "G")))),
                                                          htmlOutput("shooterplot_text"),
                                                          plotlyOutput('shooterplot')
                                                          )
                                                 )                                                          
                                     )
                                   )
                                 )
                          )
             )
             
  )

