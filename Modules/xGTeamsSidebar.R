# This module is used to create the sidebar layout for the team xG
# tables.
xGTeamsSidebarUI <- function(id){
  ns <- NS(id)
  
  tagList(
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
    # width = 2,
    actionButton(ns('team_action'),
                 label = "Refresh filters"),
    radioButtons(ns('team_advanced'),
                 'Stats option:',
                 choices = c('Basic' = "Basic stats", 'Advanced' = "Advanced stats"),
                 selected = "Advanced stats",
                 inline = T),
    radioButtons(ns("team_homeadjusted"),
                 "Home adjustment:",
                 choices = c("Standard", "Home-adjusted"),
                 inline = T),
    conditionalPanel("(input.ns(team_seasonordate) == 'Season' && 
                     input.ns(team_seasonfilter).length == 1) ||
                     (input.ns(team_seasonordate) == 'Date' && 
                     parseInt(input.ns(team_date1).substring(0,4)) == parseInt(input.ns(team_date2).substring(0,4)))",
                     
                     checkboxInput(ns('team_conferenceview'),
                                   'View by conference',
                                   value = T)),
    checkboxInput(ns('team_byseasons'),
                  label = 'Split teams by seasons',
                  value = T),
    radioButtons(ns('team_seasonordate'),
                 'Filter by:',
                 choices = c('Season', 'Date'),
                 inline = T),
    conditionalPanel(condition = "input.ns(team_seasonordate) == 'Season' && input.ns(team_subtab) != 'teamxgoalsplitsplots'",
                     dropdownButton(inputId = ns("team_seasonfilter_dropdown"),
                                    label = "Seasons:",
                                    circle = FALSE,
                                    width = 3,
                                    checkboxInput(ns("team_seasonfilter_selectall"),
                                                  label = "All/None",
                                                  value = F),
                                    checkboxGroupInput(ns('team_seasonfilter'),
                                                       "",
                                                       choices = min(teamxgoals$Season):max(teamxgoals$Season),
                                                       selected = max(teamxgoals$Season),
                                                       inline = T))),
    conditionalPanel(condition = "input.ns(team_seasonordate) == 'Season' && input.ns(team_subtab) == 'teamxgoalsplitsplots'",
                     dropdownButton(inputId = ns("team_seasonfilter2_dropdown"),
                                    label = "Seasons:",
                                    circle = FALSE,
                                    width = 3,
                                    checkboxInput(ns("team_seasonfilter2_selectall"),
                                                  label = "All/None",
                                                  value = F),
                                    checkboxGroupInput(ns('team_seasonfilter2'),
                                                       "",
                                                       choices = min(teamxgoals$Season):(max(teamxgoals$Season) - 1),
                                                       selected = min(teamxgoals$Season):(max(teamxgoals$Season) - 1),
                                                       inline = T))),
    conditionalPanel(condition = "input.ns(team_seasonordate) == 'Date'",
                     dateInput(ns('team_date1'),
                               'From:',
                               value = min(teamxgoals$date[teamxgoals$Season == max(teamxgoals$Season)]),
                               min = min(teamxgoals$date),
                               max = max(teamxgoals$date),
                               format = 'mm/dd/yyyy'),
                     dateInput(ns('team_date2'),
                               'To:',
                               value = max(teamxgoals$date),
                               min = min(teamxgoals$date),
                               max = max(teamxgoals$date),
                               format = 'mm/dd/yyyy')
    ),
    br(),
    dropdownButton(inputId = ns("team_pattern_dropdown"),
                   label = "Pattern:",
                   circle = FALSE,
                   width = 3,
                   checkboxInput(ns("team_pattern_selectall"),
                                 label = "All/None",
                                 value = F),
                   checkboxGroupInput(ns('team_pattern'),
                                      "",
                                      choices = sort(unique(teamxgoals$patternOfPlay.model)),
                                      selected = sort(unique(teamxgoals$patternOfPlay.model)),
                                      inline = T)),
    checkboxInput(ns('team_evenstate'),
                  label = 'Even gamesate only',
                  value = F),
    checkboxGroupInput(ns('team_home'),
                       label = 'Venue:',
                       choices = c('Home', 'Away'),
                       selected = c('Home', 'Away'),
                       inline = T)
  )
  
  }

###############################################################################
###############################################################################
###############################################################################

xGTeamsSidebar <- function(input, output, session, data=teamxgoals){
  # Shooter reactive values ####
  # Initial values for filters
  team_shot_inputs <- reactiveValues(team_seasonordate = 'Season',
                                team_date1 = as.Date('2000-01-01'),
                                team_date2 = as.Date('9999-12-31'),
                                team_seasonfilter = max(teamxgoals$Season),
                                team_byseasons = T,
                                team_conferenceview = T,
                                team_advanced = 'Basic stats',
                                team_home = c('Home', 'Away'),
                                team_pattern =  'All',
                                team_evenstate = F,
                                teamplot_xvar = 'xGF',
                                teamplot_yvar = 'GF',
                                team_homeadjusted = 'Standard')

  observeEvent(input$team_action,
               {
                 team_shot_inputs$team_seasonordate <- input$team_seasonordate
                 team_shot_inputs$team_date1 <- input$team_date1
                 team_shot_inputs$team_date2 <- input$team_date2
                 team_shot_inputs$team_seasonfilter <- input$team_seasonfilter
                 team_shot_inputs$team_conferenceview <- input$team_conferenceview
                 team_shot_inputs$team_byseasons <- input$team_byseasons
                 team_shot_inputs$team_advanced <- input$team_advanced
                 team_shot_inputs$team_home <- input$team_home
                 team_shot_inputs$team_pattern <- input$team_pattern
                 team_shot_inputs$team_evenstate <- input$team_evenstate
                 team_shot_inputs$teamplot_xvar <- input$teamplot_xvar
                 team_shot_inputs$teamplot_yvar <- input$teamplot_yvar
                 team_shot_inputs$team_homeadjusted <- input$team_homeadjusted
               })
  
  # Select all controls
  observeEvent(input$team_seasonfilter_selectall,
               { print(c(min(teamxgoals$Season):max(teamxgoals$Season))[-c(length(unique(teamxgoals$Season)))[input$team_subtab == "teamxgoalsplitsplots"]])
                 updateCheckboxGroupInput(
                   session, 
                   "team_seasonfilter", 
                   choices = min(teamxgoals$Season):ifelse(input$team_subtab == "teamxgoalsplitsplots", max(teamxgoals$Season) - 1, max(teamxgoals$Season)),
                   selected = if (input$team_seasonfilter_selectall) min(teamxgoals$Season):ifelse(input$team_subtab == "teamxgoalsplitsplots", max(teamxgoals$Season) - 1, max(teamxgoals$Season))
                 )
               },
               ignoreInit = T)
  
  observeEvent(input$team_seasonfilter2_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "team_seasonfilter2", 
                   choices = min(teamxgoals$Season):ifelse(input$team_subtab == "teamxgoalsplitsplots", max(teamxgoals$Season) - 1, max(teamxgoals$Season)),
                   selected = if (input$team_seasonfilter2_selectall) min(teamxgoals$Season):ifelse(input$team_subtab == "teamxgoalsplitsplots", max(teamxgoals$Season) - 1, max(teamxgoals$Season))
                 )
               },
               ignoreInit = T)
  
  observeEvent(input$team_pattern_selectall,
               {
                 updateCheckboxGroupInput(
                   session, 
                   "team_pattern", 
                   choices = sort(unique(teamxgoals$patternOfPlay.model)),
                   selected = if (input$team_pattern_selectall) sort(unique(teamxgoals$patternOfPlay.model))
                 )
               },
               ignoreInit = T)
  
  return(team_shot_inputs)
}
