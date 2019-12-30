
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('Modules/xGPlayersSidebar.R')
source('Modules/xGPlayersScatter.R')
source('Modules/xGTeamsSidebar.R')

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
                                     xGPlayersSidebarUI('shooting')
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
                                                          # textOutput('testing')
                                                          # DT::dataTableOutput('test_table')
                                                          DT::dataTableOutput('shooter_table_per96')
                                                 ),
                                                 tabPanel('Scatter plots',
                                                          value = "plots",
                                                          p(HTML("<i>Per-minutes data and position information only goes back to 2015. Please allow a few seconds for the plot to load.</i>")),
                                                          # textOutput('testing'),
                                                          xGPlayersScatterUI('shooter_scatter')
                                                          )
                                                 )                                                          
                                     )
                                   )
                                 ),
                        tabPanel('Teams',
                                 value = "teamxgoals",
                                 sidebarLayout(
                                   sidebarPanel(
                                     xGTeamsSidebarUI('team_shooting')
                                   ),
                                   mainPanel(
                                     h1('Team shots data'),
                                     p(paste0('Updated through games on ', max(as.Date(teamxgoals$date)))),
                                     tabsetPanel(id = 'team_subtab',
                                                 tabPanel('Totals',
                                                          downloadButton('team_download', 'Download CSV'),
                                                          br(),
                                                          br(),
                                                          # textOutput('testing')
                                                          # DT::dataTableOutput('test_table')
                                                          div(id = 'west', conditionalPanel(condition = "team_shot_inputs.team_conferenceview == 1 && ((team_shot_inputs.team_seasonordate == 'Season' && team_shot_inputs.team_seasonfilter.length == 1) ||
                                                                                            (team_shot_inputs.team_seasonordate == 'Date' && team_shot_inputs.team_date1.substring(0,4) == team_shot_inputs.team_date2.substring(0,4)))",
                                                                                            h2('Western conference')),
                                                              DT::dataTableOutput('teamtotalxgoalswest')),
                                                          br(),
                                                          div(id = 'east', conditionalPanel(condition = "team_shot_inputs.team_conferenceview == 1 && ((team_shot_inputs.team_seasonordate == 'Season' && team_shot_inputs.team_seasonfilter.length == 1) ||
                                                                                            (team_shot_inputs.team_seasonordate == 'Date' && iteam_shot_inputs.team_date1.substring(0,4) == team_shot_inputs.team_date2.substring(0,4)))",
                                                                                            h2('Eastern conference'),
                                                                                            DT::dataTableOutput('teamtotalxgoalseast')))
                                                  ),
                                                 tabPanel('Per game',
                                                          downloadButton('team_download_pergame', 'Download CSV'),
                                                          br(),
                                                          br(),
                                                          conditionalPanel(condition = "team_shot_inputs.team_conferenceview == 1 && ((team_shot_inputs.team_seasonordate == 'Season' && team_shot_inputs.team_seasonfilter.length == 1) ||
                                                                                              (team_shot_inputs.team_seasonordate == 'Date' && team_shot_inputs.team_date1.substring(0,4) == team_shot_inputs.team_date2.substring(0,4)))",
                                                                           h2('Western conference')),
                                                          div(DT::dataTableOutput('teampergamexgoalswest')),
                                                          br(),
                                                          conditionalPanel(condition = "team_shot_inputs.team_conferenceview == 1 && ((team_shot_inputs.team_seasonordate == 'Season' && team_shot_inputs.team_seasonfilter.length == 1) ||
                                                                                              (team_shot_inputs.team_seasonordate == 'Date' && team_shot_inputs.team_date1.substring(0,4) == team_shot_inputs.team_date2.substring(0,4)))",
                                                                           h2('Eastern conference'),
                                                                           div(DT::dataTableOutput('teampergamexgoalseast')))
                                                 )
                                     )                                                          
                                   )
                                 )
                          )
                    )
             )
             
  )

