
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
  # Shooting navbar ####
  navbarPage(title = HTML('<b>ASA Database</b>'),
             theme = 'bootstrap_edited.css',
             id = "headnavbar",
             navbarMenu(strong('xGoals'),
                        # Players tab panel ####
                        tabPanel('Players',
                                 value = "playerxgoals",
                                 sidebarLayout(
                                   sidebarPanel(
                                     tagList(
                                       tags$head(
                                         tags$style(
                                           HTML(
                                             ".checkbox-inline { 
                    margin-left: 20px;
                    margin-right: 0px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 20px;
                    margin-right: 0px;
          }
        "
                                           )
                                         ) 
                                       )),
                                     width = 2,
                                     actionButton('shooting_action',
                                                  label = "Refresh filters"),
                                     conditionalPanel(condition = 
                                                        "(input.shooting_seasonordate == 'Season' && 
                                                      Math.min(parseInt(input.shooting_seasonfilter)) >= 2015) ||
                                                      (input.shooting_seasonordate == 'Date' && 
                                                      parseInt(input.shooting_date1.substring(0,4)) >= 2015)",
                                                      numericInput("shooting_minfilter",
                                                                   label = "Minimum minutes:",
                                                                   value = 0,
                                                                   min = 0, max = 10000, step = 500)),
                                     numericInput('shooting_minshots',
                                                  "Minimum shots:",
                                                  value = 0,
                                                  min = 0, max = 100, step = 10),
                                     numericInput('shooting_minkeypasses',
                                                  'Minimum key passes:',
                                                  value = 0,
                                                  min = 0, max = 100, step = 10),
                                     conditionalPanel(condition = 
                                                        "(input.shooting_seasonordate == 'Season' && 
                                                      Math.min(parseInt(input.shooting_seasonfilter)) >= 2015) ||
                                                      (input.shooting_seasonordate == 'Date' && 
                                                      parseInt(input.shooting_date1.substring(0,4)) >= 2015)",
                                                      checkboxGroupInput("shooting_position",
                                                                         label = "Position:",
                                                                         inline = T,
                                                                         choices = c("Keeper (G)" = "G",
                                                                                     "Central Def (D)" = "D",
                                                                                     "Back (B)" = "B",
                                                                                     "Midfielder (M)" = "M",
                                                                                     "Attacking Mid (A)" = "A",
                                                                                     "Forward (F)" = "F",
                                                                                     "Sub (S)" = "S"),
                                                                         selected = c("G", "D", "B", "M", "A", "F", "S"))),
                                     radioButtons('shooting_seasonordate',
                                                  'Filter by:',
                                                  choices = c('Season', 'Date'),
                                                  inline = T),
                                     conditionalPanel(condition = "input.shooting_seasonordate == 'Season'",
                                                      checkboxGroupInput('shooting_seasonfilter',
                                                                         'Select seasons:',
                                                                         choices = min(playerxgoals$Season):max(playerxgoals$Season),
                                                                         selected = max(playerxgoals$Season),
                                                                         inline = T)),
                                     conditionalPanel(condition = "input.shooting_seasonordate == 'Date'",
                                                      dateInput('shooting_date1',
                                                                'From:',
                                                                value = min(playerxgoals$date[playerxgoals$Season == max(playerxgoals$Season)]),
                                                                min = min(playerxgoals$date),
                                                                max = max(playerxgoals$date),
                                                                format = 'mm/dd/yyyy'),
                                                      dateInput('shooting_date2',
                                                                'To:',
                                                                value = max(playerxgoals$date),
                                                                min = min(playerxgoals$date),
                                                                max = max(playerxgoals$date),
                                                                format = 'mm/dd/yyyy')
                                     ),                  
                                     h5(HTML('<b>Other filters:</b>')),
                                     checkboxInput('shooting_byteams',
                                                   label = 'Split players by teams',
                                                   value = F),
                                     checkboxInput('shooting_byseasons',
                                                   label = 'Split players by seasons',
                                                   value = T),
                                     checkboxInput('shooting_other',
                                                   label = 'Include non PK/FK',
                                                   value = T),
                                     checkboxInput('shooting_pk',
                                                   label = 'Include PKs',
                                                   value = T),
                                     checkboxInput('shooting_fk',
                                                   label = 'Include FKs',
                                                   value = T)),
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
                                                          p(HTML("<i>Per-minutes data and position information only goes back to 2015.</i>")),
                                                          #tags$head(tags$script(src = "www/tablesorter.js")),
                                                          DT::dataTableOutput('shootertable')
                                                 ),
                                                 tabPanel('Tables: per 96',
                                                          value = "tablesper96",
                                                          p(HTML("<i>Per-minutes data and position information only goes back to 2015.</i>")),
                                                          DT::dataTableOutput('shootertable_per96')
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
                                                                               choices = "Goals",
                                                                               selected = "Goals")))),
                                                          htmlOutput("shooterplot_text"),
                                                          plotlyOutput('shooterplot')                                                            )))
                                 )),
                        
                        # Team xG tab panel ####
                        tabPanel('Teams',
                                 value = "teamxgoals",
                                 sidebarLayout(
                                   sidebarPanel(tagList(
                                     tags$head(
                                       tags$style(
                                         HTML(
                                           ".checkbox-inline { 
                                           margin-left: 20px;
                                           margin-right: 0px;
                                           }
                                           .checkbox-inline+.checkbox-inline {
                                           margin-left: 20px;
                                           margin-right: 0px;
                                           }
                                           "
                                         )
                                       ) 
                                     )),
                                     width = 2,
                                     # actionButton('team_action', 
                                     #              label = "Refresh filters"),
                                     radioButtons('team_advanced',
                                                  'Stats option:',
                                                  choices = c('Basic' = "Basic stats", 'Advanced' = "Advanced stats"),
                                                  inline = T),
                                     
                                     conditionalPanel("(input.team_seasonordate == 'Season' && 
                                                      input.team_seasonfilter.length == 1) ||
                                                      (input.team_seasonordate == 'Date' && 
                                                      parseInt(input.team_date1.substring(0,4)) == parseInt(input.team_date2.substring(0,4)))",
                                                      
                                                      checkboxInput('team_conferenceview',
                                                                    'View by conference',
                                                                    value = T)),
                                     checkboxInput('team_byseasons',
                                                   label = 'Split teams by seasons',
                                                   value = T),
                                     radioButtons('team_seasonordate',
                                                  'Filter by:',
                                                  choices = c('Season', 'Date'),
                                                  inline = T),
                                     conditionalPanel(condition = "input.team_seasonordate == 'Season' && input.team_subtab != 'teamxgoalsplitsplots'",
                                                      checkboxGroupInput('team_seasonfilter',
                                                                         'Select seasons:',
                                                                         choices = min(teamxgoals$Season):max(teamxgoals$Season),
                                                                         selected = max(teamxgoals$Season),
                                                                         inline = T)),
                                     conditionalPanel(condition = "input.team_seasonordate == 'Season' && input.team_subtab == 'teamxgoalsplitsplots'",
                                                      checkboxGroupInput('team_seasonfilter2',
                                                                         'Select seasons:',
                                                                         choices = min(teamxgoals$Season):(max(teamxgoals$Season) - 1),
                                                                         selected = min(teamxgoals$Season):(max(teamxgoals$Season) - 1),
                                                                         inline = T)),
                                     conditionalPanel(condition = "input.team_seasonordate == 'Date'",
                                                      dateInput('team_date1',
                                                                'From:',
                                                                value = min(teamxgoals$date[teamxgoals$Season == max(teamxgoals$Season)]),
                                                                min = min(teamxgoals$date),
                                                                max = max(teamxgoals$date),
                                                                format = 'mm/dd/yyyy'),
                                                      dateInput('team_date2',
                                                                'To:',
                                                                value = max(teamxgoals$date),
                                                                min = min(teamxgoals$date),
                                                                max = max(teamxgoals$date),
                                                                format = 'mm/dd/yyyy')
                                     ),
                                     selectInput('team_pattern',
                                                 'Pattern of play:',
                                                 choices = c('All', sort(unique(teamxgoals$patternOfPlay.model))),
                                                 selected = 'All'),
                                     checkboxInput('team_evenstate',
                                                   label = 'Even gamesate only',
                                                   value = F),
                                     checkboxGroupInput('team_home',
                                                        label = 'Venue:',
                                                        choices = c('Home', 'Away'),
                                                        selected = c('Home', 'Away'),
                                                        inline = T)),
                                   mainPanel(
                                     h1('Team shots data'),
                                     p(paste0('Updated through games on ', max(as.Date(teamxgoals$date)))),
                                     tabsetPanel(id = 'team_subtab',
                                                 tabPanel('Totals',
                                                          downloadButton('team_download', 'Download CSV'),
                                                          br(),
                                                          br(),
                                                          div(id = 'west', conditionalPanel(condition = "input.team_conferenceview == 1 && ((input.team_seasonordate == 'Season' && input.team_seasonfilter.length == 1) ||
                                                                                              (input.team_seasonordate == 'Date' && input.team_date1.substring(0,4) == input.team_date2.substring(0,4)))",
                                                                                            h2('Western conference')),
                                                              DT::dataTableOutput('teamtotalxgoalswest')),
                                                          br(),
                                                          div(id = 'east', conditionalPanel(condition = "input.team_conferenceview == 1 && ((input.team_seasonordate == 'Season' && input.team_seasonfilter.length == 1) ||
                                                                                              (input.team_seasonordate == 'Date' && input.team_date1.substring(0,4) == input.team_date2.substring(0,4)))",
                                                                                            h2('Eastern conference'),
                                                                                            DT::dataTableOutput('teamtotalxgoalseast')))
                                                 ),
                                                 tabPanel('Per game',
                                                          downloadButton('team_download_pergame', 'Download CSV'),
                                                          br(),
                                                          br(),
                                                          conditionalPanel(condition = "input.team_conferenceview == 1 && ((input.team_seasonordate == 'Season' && input.team_seasonfilter.length == 1) ||
                                                                                              (input.team_seasonordate == 'Date' && input.team_date1.substring(0,4) == input.team_date2.substring(0,4)))",
                                                                           h2('Western conference')),
                                                          div(DT::dataTableOutput('teampergamexgoalswest')),
                                                          br(),
                                                          conditionalPanel(condition = "input.team_conferenceview == 1 && ((input.team_seasonordate == 'Season' && input.team_seasonfilter.length == 1) ||
                                                                                              (input.team_seasonordate == 'Date' && input.team_date1.substring(0,4) == input.team_date2.substring(0,4)))",
                                                                           h2('Eastern conference'),
                                                                           div(DT::dataTableOutput('teampergamexgoalseast')))
                                                 ),
                                                 tabPanel('Scatter plots',
                                                          value = "teamxgoalplots",
                                                          p(HTML("These plots show the linear correlation between pairs of metrics calculated concurrently.
                                                                 <br> <i>Please allow a few seconds for the plot to load.</i>")),
                                                          fluidPage(fluidRow(
                                                            column(3,
                                                                   selectInput('teamplot_xvar',
                                                                               label = 'X-axis variable',
                                                                               choices = "xGF",
                                                                               selected = 'xGF')),
                                                            column(3,
                                                                   selectInput('teamplot_yvar',
                                                                               label = 'Y-axis variable',
                                                                               choices = "GF",
                                                                               selected = 'GF')))),
                                                          htmlOutput("teamshootingplot_text"),
                                                          plotlyOutput("teamplot")),
                                                 
                                                 tabPanel('Scatter plots (split seasons)',
                                                          value = "teamxgoalsplitsplots",
                                                          downloadButton('team_download_splits', 'Download CSV'),
                                                          br(),
                                                          p(HTML("These plots show the linear correlation between pairs of metrics, one calculated before a season split and one after.
                                                                 <br> <i>Please allow a few seconds for the plot to load.</i>")),
                                                          fluidPage(fluidRow(
                                                            column(3,
                                                                   sliderInput("teamsplitsplot_split",
                                                                               label = "Split after game #",
                                                                               value = 17,
                                                                               min = 1, max = 33, step = 1)),
                                                            column(3,
                                                                   selectInput('teamsplitsplot_xvar',
                                                                               label = 'X-axis variable',
                                                                               choices = "xGD (before split)",
                                                                               selected = 'xGD (before split)')),
                                                            column(3,
                                                                   selectInput('teamsplitsplot_yvar',
                                                                               label = 'Y-axis variable',
                                                                               choices = "GD (after split)",
                                                                               selected = "GD (after split")))),
                                                          htmlOutput("teamshootingsplitsplot_text"),
                                                          plotlyOutput("teamsplitsplot"))
                                     )
                                   )
                                 )
                        ),
                        tabPanel('Game-by-game xG',
                                 value = "gamebygamexg",
                                 sidebarLayout(
                                   sidebarPanel(width = 2,
                                                radioButtons('teambygame_seasonordate',
                                                             'Filter by:',
                                                             choices = c('Season', 'Date')),
                                                conditionalPanel(condition = "input.teambygame_seasonordate == 'Season'",
                                                                 checkboxGroupInput('teambygame_seasonfilter',
                                                                                    'Select seasons:',
                                                                                    choices = min(xgbygame$Season):max(xgbygame$Season),
                                                                                    selected = max(xgbygame$Season))),
                                                conditionalPanel(condition = "input.teambygame_seasonordate == 'Date'",
                                                                 dateInput('teambygame_date1',
                                                                           'From:',
                                                                           value = min(xgbygame$Date[xgbygame$Season == max(xgbygame$Season)]),
                                                                           min = min(xgbygame$Date),
                                                                           max = max(xgbygame$Date),
                                                                           format = 'mm/dd/yyyy'),
                                                                 dateInput('teambygame_date2',
                                                                           'To:',
                                                                           value = max(xgbygame$Date),
                                                                           min = min(xgbygame$Date),
                                                                           max = max(xgbygame$Date),
                                                                           format = 'mm/dd/yyyy'))
                                   ),
                                   mainPanel(
                                     h1('Team xGoals by game'),
                                     p(paste0('Updated through games on ', max(as.Date(xgbygame$Date)))),
                                     downloadButton('teambygame_download', 'Download CSV'),
                                     br(),
                                     br(),
                                     DT::dataTableOutput('teamxgoalsbygame')
                                   )
                                 )
                        ),
                        # Keepers tab panel ####
                        tabPanel('Keepers',
                                 value = "keeperxgoals",
                                 sidebarLayout(
                                   sidebarPanel(tagList(
                                     tags$head(
                                       tags$style(
                                         HTML(
                                           ".checkbox-inline { 
                     margin-left: 20px;
                     margin-right: 0px;
                     }
                     .checkbox-inline+.checkbox-inline {
                     margin-left: 20px;
                     margin-right: 0px;
                     }
                     "
                                         )
                                       ) 
                                     )),
                                     width = 2,
                                     actionButton('keeper_action',
                                                  label = "Refresh filters"),
                                     conditionalPanel(
                                       condition = "(input.keeper_seasonordate == 'Season' && 
                                                      Math.min(parseInt(input.keeper_seasonfilter)) >= 2015) ||
                                        (input.keeper_seasonordate == 'Date' && 
                                        parseInt(input.keeper_date1.substring(0,4)) >= 2015)",  
                                       numericInput("keeper_minfilter",
                                                    label = "Minimum minutes:",
                                                    value = 0,
                                                    min = 0, max = 10000, step = 500)),
                                     numericInput('keeper_minshots',
                                                  "Minimum shots faced:",
                                                  value = 0,
                                                  min = 0, max = 100, step = 10),
                                     radioButtons('keeper_seasonordate',
                                                  'Filter by:',
                                                  choices = c('Season', 'Date'),
                                                  inline = T),
                                     conditionalPanel(condition = "input.keeper_seasonordate == 'Season'",
                                                      checkboxGroupInput('keeper_seasonfilter',
                                                                         'Select seasons:',
                                                                         choices = min(playerxgoals$Season):max(playerxgoals$Season),
                                                                         selected = max(playerxgoals$Season),
                                                                         inline = T)),
                                     conditionalPanel(condition = "input.keeper_seasonordate == 'Date'",
                                                      dateInput('keeper_date1',
                                                                'From:',
                                                                value = min(playerxgoals$date[playerxgoals$Season == max(playerxgoals$Season)]),
                                                                min = min(playerxgoals$date),
                                                                max = max(playerxgoals$date),
                                                                format = 'mm/dd/yyyy'),
                                                      dateInput('keeper_date2',
                                                                'To:',
                                                                value = max(playerxgoals$date),
                                                                min = min(playerxgoals$date),
                                                                max = max(playerxgoals$date),
                                                                format = 'mm/dd/yyyy')
                                     ),                  
                                     h5(HTML('<b>Other filters:</b>')),
                                     checkboxInput('keeper_byteams',
                                                   label = 'Split by teams',
                                                   value = F),
                                     checkboxInput('keeper_byseasons',
                                                   label = 'Split by seasons',
                                                   value = T),
                                     checkboxInput('keeper_othershots',
                                                   label = 'Include non PK/FK',
                                                   value = T),
                                     checkboxInput('keeper_pk',
                                                   label = 'Include PKs',
                                                   value = T),
                                     checkboxInput('keeper_fk',
                                                   label = 'Include FKs',
                                                   value = T)),
                                   mainPanel(
                                     h1('Keeper xGoals'),
                                     p(paste0('Updated through games on ', max(as.Date(keeperxgoals$date)))),
                                     downloadButton('keeper_download', 'Download CSV'),
                                     br(),
                                     br(),
                                     tabsetPanel(id = 'keeper_subtab',
                                                 tabPanel('Tables: totals',
                                                          value = "tablestotals",
                                                          p(HTML("<i>Per-minutes data only goes back to 2015.</i>")),
                                                          DT::dataTableOutput('keepertable')),
                                                 tabPanel("Tables: per 96",
                                                          value = "tablesper96",
                                                          p(HTML("<i>Per-minutes data only goes back to 2015.</i>")),
                                                          DT::dataTableOutput("keepertable_per96")
                                                 ),
                                                 tabPanel('Scatter plots',
                                                          value = "plotstotals",
                                                          fluidPage(fluidRow(
                                                            p(HTML("<i>Per-minutes data only goes back to 2015. Please allow a few seconds for the plot to load.</i>")),
                                                            column(4,
                                                                   selectInput('keeperplot_xvar',
                                                                               label = 'X-axis variable',
                                                                               choices = c('Shots faced' = 'Shots', 'Goals allowed' = 'GA',
                                                                                           'GA/shot' = 'GAperShot', 'xG/Shot' = 'xGperShot',
                                                                                           'G-xG/shot' = 'GmxGperShot',
                                                                                           '%Shots headed' = 'Header%', 'Avg. distance' = 'Dist',
                                                                                           'xG faced' = 'xG', 'GA above average' = 'G-xG'),
                                                                               selected = 'xG')),
                                                            column(4,
                                                                   selectInput('keeperplot_yvar',
                                                                               label = 'Y-axis variable',
                                                                               choices = c('Shots faced' = 'Shots', 'Goals allowed' = 'GA',
                                                                                           'GA/shot' = 'GAperShot', 'xG/Shot' = 'xGperShot',
                                                                                           'G-xG/shot' = 'GmxGperShot',
                                                                                           '%Shots headed' = 'Header%', 'Avg. distance' = 'Dist',
                                                                                           'xG faced' = 'xG', 'GA above average' = 'G-xG'),
                                                                               selected = 'G-xG')))),
                                                          htmlOutput("keeperplot_text"),
                                                          plotlyOutput('keeperplot')
                                                 )
                                     ))
                                 )
                        )),
             # Passing navbar ####
             navbarMenu(strong('xPasses'),
                        # Passing: Players tab panel ####
                        tabPanel('Players',
                                 value = "playerpassing",
                                 sidebarLayout(
                                   sidebarPanel(tagList(
                                     tags$head(
                                       tags$style(
                                         HTML(
                                           ".checkbox-inline { 
                    margin-left: 20px;
                    margin-right: 0px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 20px;
                    margin-right: 0px;
          }
        "
                                         )
                                       ) 
                                     )),
                                     width = 2,
                                     actionButton('passing_action',
                                                  label = "Refresh filters"),
                                     numericInput("passing_minfilter",
                                                  label = "Minimum minutes:",
                                                  value = 0,
                                                  min = 0, max = 10000, 500),
                                     numericInput('passing_minpasses',
                                                  "Minimum passes:",
                                                  value = 0,
                                                  min = 0, max = 1000, step = 50),
                                     checkboxGroupInput("passing_position",
                                                        label = "Position:",
                                                        inline = T,
                                                        choices = c("Keeper (G)" = "G",
                                                                    "Central Def (D)" = "D",
                                                                    "Back (B)" = "B",
                                                                    "Midfielder (M)" = "M",
                                                                    "Attacking Mid (A)" = "A",
                                                                    "Forward (F)" = "F",
                                                                    "Sub (S)" = "S"),
                                                        selected = c("G", "D", "B", "M", "A", "F", "S")),
                                     radioButtons('passing_third',
                                                  label = "Third:",
                                                  inline = F,
                                                  choices = c("All", "Attacking" = "Att", "Middle" = "Mid", "Defensive" = "Def"),
                                                  selected = "All"),
                                     radioButtons('passing_seasonordate',
                                                  'Filter by:',
                                                  choices = c('Season', 'Date'),
                                                  inline = T),
                                     conditionalPanel(condition = "input.passing_seasonordate == 'Season'",
                                                      checkboxGroupInput('passing_seasonfilter',
                                                                         'Select seasons:',
                                                                         choices = min(playerxgoals$Season):max(playerxgoals$Season),
                                                                         selected = max(playerxgoals$Season),
                                                                         inline = T)),
                                     conditionalPanel(condition = "input.passing_seasonordate == 'Date'",
                                                      dateInput('passing_date1',
                                                                'From:',
                                                                value = min(playerpassing$date[playerpassing$Season == max(playerpassing$Season)]),
                                                                min = min(playerpassing$date),
                                                                max = max(playerpassing$date),
                                                                format = 'mm/dd/yyyy'),
                                                      dateInput('passing_date2',
                                                                'To:',
                                                                value = max(playerpassing$date),
                                                                min = min(playerpassing$date),
                                                                max = max(playerpassing$date),
                                                                format = 'mm/dd/yyyy')
                                     ),  
                                     h5(HTML('<b>Other filters:</b>')),
                                     checkboxInput('passing_byteams',
                                                   label = 'Split players by teams',
                                                   value = F),
                                     checkboxInput('passing_byseasons',
                                                   label = 'Split players by seasons',
                                                   value = T)),
                                   mainPanel(h1('Player xPasses'),
                                             p(paste0('Updated through games on ', max(as.Date(playerxgoals$date)))),
                                             downloadButton('passing_download', 'Download CSV'),
                                             br(),
                                             tabsetPanel(id = 'passing_subtab',
                                                         tabPanel('Tables: totals',
                                                                  value = "passingtablestotals",
                                                                  DT::dataTableOutput('passingtable_player')),
                                                         tabPanel("Tables: per 96",
                                                                  value = "passingtablesper96",
                                                                  DT::dataTableOutput("passingtable_player_per96")),
                                                         tabPanel("Scatter plots",
                                                                  value = "passingplots",
                                                                  p(HTML("<i>Please allow a few seconds for the plot to load.</i>")),
                                                                  fluidPage(fluidRow(
                                                                    column(3,
                                                                           selectInput('passerplot_xvar',
                                                                                       label = 'X-axis variable',
                                                                                       choices = "xPassPct",
                                                                                       selected = "xPassPct")),
                                                                    column(3,
                                                                           selectInput('passerplot_yvar',
                                                                                       label = 'Y-axis variable',
                                                                                       choices = "PassPct",
                                                                                       selected = "PassPct")))),
                                                                  htmlOutput("passerplot_text"),
                                                                  plotlyOutput('passerplot'))
                                             )))),
                        # Passing: Teams ####
                        tabPanel('Teams',
                                 value = "teampassing",
                                 sidebarLayout(
                                   sidebarPanel(tagList(
                                     tags$head(
                                       tags$style(
                                         HTML(
                                           ".checkbox-inline { 
                                           margin-left: 20px;
                                           margin-right: 0px;
                                           }
                                           .checkbox-inline+.checkbox-inline {
                                           margin-left: 20px;
                                           margin-right: 0px;
                                           }
                                           "
                                         )
                                       ) 
                                     )),
                                     width = 2,
                                     # conditionalPanel("input.team_seasonordate == 'Season' && input.team_seasonfilter.length == 1",
                                     #                  checkboxInput('team_conferenceview',
                                     #                                'By conference',
                                     #                                value = T)),
                                     radioButtons('teampassing_seasonordate',
                                                  'Filter by:',
                                                  choices = c('Season', 'Date'),
                                                  inline = T),
                                     conditionalPanel(condition = "input.teampassing_seasonordate == 'Season'",
                                                      checkboxGroupInput('teampassing_seasonfilter',
                                                                         label = 'Select seasons:',
                                                                         choices = min(teampassing.offense$year):max(teampassing.offense$year),
                                                                         selected = max(teampassing.offense$year),
                                                                         inline = T)),
                                     conditionalPanel(condition = "input.teampassing_seasonordate == 'Date'",
                                                      dateInput('teampassing_date1',
                                                                'From:',
                                                                value = min(teampassing.offense$date[teampassing.offense$year == max(teampassing.offense$year)]),
                                                                min = min(teampassing.offense$date),
                                                                max = max(teampassing.offense$date),
                                                                format = 'mm/dd/yyyy'),
                                                      dateInput('teampassing_date2',
                                                                'To:',
                                                                value = max(teampassing.offense$date),
                                                                min = min(teampassing.offense$date),
                                                                max = max(teampassing.offense$date),
                                                                format = 'mm/dd/yyyy')
                                     ),
                                     checkboxGroupInput("teampassing_thirdfilter",
                                                        label = "Third:",
                                                        choices = c("Defensive" = "Def", "Middle" = "Mid", "Attacking" = "Att"),
                                                        selected = c("Def", "Mid", "Att")),
                                     # conditionalPanel(condition = "input.team_seasonordate == 'Season'",
                                     #                  checkboxGroupInput('team_seasonfilter',
                                     #                                     'Select seasons:',
                                     #                                     choices = min(teamxgoals$Season):max(teamxgoals$Season),
                                     #                                     selected = max(teamxgoals$Season))),
                                     # conditionalPanel(condition = "input.team_seasonordate == 'Date'",
                                     #                  dateInput('team_date1',
                                     #                            'From:',
                                     #                            value = min(teamxgoals$date[teamxgoals$Season == max(teamxgoals$Season)]),
                                     #                            min = min(teamxgoals$date),
                                     #                            max = max(teamxgoals$date),
                                     #                            format = 'mm/dd/yyyy'),
                                     #                  dateInput('team_date2',
                                     #                            'To:',
                                     #                            value = max(teamxgoals$date),
                                     #                            min = min(teamxgoals$date),
                                     #                            max = max(teamxgoals$date),
                                     #                            format = 'mm/dd/yyyy')
                                     # ),
                                     checkboxInput('teampassing_byseasons',
                                                   label = 'Split teams by seasons',
                                                   value = T)
                                     #,
                                     # selectInput('teampassing_type',
                                     #             'Pattern of play:',
                                     #             choices = c('All', sort(unique(teamxgoals$patternOfPlay.model))),
                                     #             selected = 'All'),
                                     # checkboxInput('team_evenplayer',
                                     #               label = 'Even gamesate only',
                                     #               value = F),
                                     # checkboxGroupInput('team_home',
                                     #                    label = 'Venue:',
                                     #                    choices = c('Home', 'Away'),
                                     #                    selected = c('Home', 'Away'))),
                                   ),
                                   mainPanel(
                                     h1('Team passing data'),
                                     p(paste0('Updated through games on ', max(as.Date(playerxgoals$date)))),
                                     # p(paste0('Updated through games on ', max(as.Date(???)))),
                                     p(HTML("<i>Against stats (A) refer to the third of the field from the perspective of the teams' opponents.</i>")),
                                     tabsetPanel(id = 'teampassing_subtab',
                                                 tabPanel('Totals',
                                                          downloadButton('teampassing_download', 'Download CSV'),
                                                          br(),
                                                          br(),
                                                          DT::dataTableOutput("teampassing_total")
                                                 ),
                                                 tabPanel('Per game',
                                                          downloadButton('teampassing_download_pergame', 'Download CSV'),
                                                          br(),
                                                          br(),
                                                          DT::dataTableOutput("teampassing_pergame")),
                                                 tabPanel('Scatter plots',
                                                          value = "teampassingplots",
                                                          p(HTML("<i>Please allow a few seconds for the plot to load.</i>")),
                                                          fluidPage(fluidRow(
                                                            column(3,
                                                                   selectInput('teampassingplot_xvar',
                                                                               label = 'X-axis variable',
                                                                               choices = "xPctF",
                                                                               selected = 'xPctF')),
                                                            column(3,
                                                                   selectInput('teampassingplot_yvar',
                                                                               label = 'Y-axis variable',
                                                                               choices = c("PctF"),
                                                                               selected = "PctF")))),
                                                          htmlOutput("teampassingplot_text"),
                                                          plotlyOutput('teampassingplot')
                                                 )
                                     )
                                   )
                                 )
                        )
             ),
             # Glossary ####
             tabPanel(strong('Glossary'),
                      value = "glossary",
                      h1('Glossary'),
                      dataTableOutput('glossary')                        
             ),
             # App info ####
             tabPanel(strong('App info'),
                      value = "appinfo",
                      h1('App info'),
                      p("We built this interactive web application to give ASA's loyal readers more control over 
                        sorting and filtering our data. This is a work in progress, and we plan to continue to
                        add new features on a regular basis.
                        If you have an idea for a feature that will make the app totes better,
                        then please don't hesitate to email Matthias (mkullowatz at gmail) with your idea."),
                      br(),
                      p('We have been asked why we measure individual stats on a per-96-minute basis, rather than
                        per 90 minutes. No, we are not trying to be hip for the sake of being hip. The average MLS game
                        is 96 minutes long, including average stoppage times for each half, and thus our team per-game
                        statistics are actually per-96-minute statistics. To maintain internal consistency of our data,
                        we show player statistics on a per-96-minute basis, too, so that the average number of goals per game
                        at the team level will be equal to the average number of goals per 96 minutes at the player level.'),
                      br(),
                      p('Please note that the statistics displayed in this app for previous seasons are very similiar to, but not exactly
                        the same as, those in our static tables. This is because this app utilizes
                        updated xGoal models, fit through 2017, with a better method of capturing penalty kicks. For most
                        players the differences are negligible. We will correct this very soon.'))
             
  )
)
