# tabPanel('predictions',
#          sidebarLayout(
#            sidebarPanel(
#              selectInput('season_predict',
#                          label = 'Season:',
#                          choices = sort(unique(pred.data$Season), decreasing = T),
#                          selected = max(pred.data$Season)),
#              HOW DO YOU REACT TO AN INPUT WITHIN THE UI
# 
#              selectInput('hteam_predict',
#                          label = 'Home:',
#                          choices = sort(unique(pred.data$team[pred.data$Season == input$season_predict]))),
#              selectInput('ateam_predict',
#                          label = 'Home:',
#                          choices = sort(unique(pred.data$team[pred.data$Season == input$season_predict &
#                                                                 pred.data$team != input$hteam_predict]))),
#              mainPanel(
#                h1('Game predictions'),
#                p('These predictions are optimized for games on or after week 25'),
#                br(),
#                br(),
#                tableOutput('predictoutcomes'),
#                tableOutput('predictmatrix')
#              )
#            )
#          ))