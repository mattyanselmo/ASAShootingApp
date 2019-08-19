## Functions to plot fields with data 
## functions to plot field

# removing extraneous ggplot shit
pitch_theme <- ggplot2::theme(
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  axis.title       = ggplot2::element_blank(),
  axis.ticks       = ggplot2::element_blank(),
  axis.text        = ggplot2::element_blank(),
  axis.line        = ggplot2::element_blank(),
  panel.background = ggplot2::element_blank(),
  panel.border     = ggplot2::element_blank()
)

# green and white pitch
colored_pitch <- c(pitch_lines = 'white', pitch_color = '#006d2c')

# Function to draw pitch
draw_pitch <- function(pitch_lines='black', 
                       pitch_color='white', 
                       data_source=c('Opta', 'StatsBomb'), 
                       size=c('full', 'half')){
  
  # Setting pitch size
  if(data_source=='StatsBomb'){
    pitch_dims <- list(
      length = 120,
      width = 80,
      penalty_box_length = 18,
      penalty_box_width = 44,
      six_yard_box_length = 6,
      six_yard_box_width = 20,
      penalty_spot_distance = 12,
      goal_width = 8,
      origin_x = 0,
      origin_y = 0)
  }
  # This is raw opta field size, need to change this if change the 
  # x, y to opta full field (115x80)
  else if (data_source=='Opta') {
    pitch_dims <-list(
      length = 100,
      width = 100,
      penalty_box_length = 18,
      penalty_box_width = 44,
      six_yard_box_length = 6,
      six_yard_box_width = 20,
      penalty_spot_distance = 12,
      goal_width = 8,
      origin_x = 0,
      origin_y = 0)
  } 
  # Need to work on this error statement to break out of the function
  else {print('Select either Opta or StatsBomb')}
    
  # Drawing lines
  if(size=='half'){
    list(
      # rectangular border outside of the field
      ggplot2::annotate(
        geom = 'rect',
        xmin = (pitch_dims$length / 2) - 4,
        ymin = pitch_dims$origin_y - 4,
        xmax = pitch_dims$length + 4,
        ymax = pitch_dims$width + 4,
        fill = pitch_color
      ),
      # Drawing goals
      ggplot2::annotate(
        geom = 'rect',
        xmin = pitch_dims$length,
        ymin = (pitch_dims$width - pitch_dims$goal_width) / 2,
        xmax = pitch_dims$length + 2,
        ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$goal_width) / 2),
        color = pitch_lines,
        fill = pitch_color
      ),
      # drawing outline of field
      ggplot2::annotate(
        geom = 'rect',
        xmin = pitch_dims$length / 2,
        ymin = pitch_dims$origin_y,
        xmax = pitch_dims$length,
        ymax = pitch_dims$width,
        color = pitch_lines, 
        fill = pitch_color
      ),
      # Drawing center circle
      ggplot2::annotation_custom(
        grob = grid::circleGrob(gp = grid::gpar(col = pitch_lines,
                                                fill = pitch_color,
                                                lwd  = 2)),
        xmin = (pitch_dims$length / 2) - 12,
        xmax = (pitch_dims$length / 2) + 12,
        ymin = (pitch_dims$width / 2) - 12,
        ymax = (pitch_dims$width / 2) + 12
      ),
      # Drawing D
      ggplot2::annotation_custom(
        grob = grid::circleGrob(gp = grid::gpar(col = pitch_lines,
                                                fill = pitch_color,
                                                lwd  = 2)),
        xmin = pitch_dims$length - (2*pitch_dims$penalty_spot_distance),
        xmax = pitch_dims$length,
        ymin = (pitch_dims$width / 2) - 12,
        ymax = (pitch_dims$width / 2) + 12
      ),
      # drawing midfield line
      ggplot2::annotate(
        geom = 'segment',
        x = pitch_dims$length / 2,
        y = pitch_dims$origin_y,
        xend = pitch_dims$length / 2,
        yend = pitch_dims$width,
        color = pitch_lines
      ),
      # drawing center point
      ggplot2::annotate(
        geom = 'point',
        x = pitch_dims$length / 2,
        y = pitch_dims$width / 2,
        color = pitch_lines
      ),
      # Drawing 18 yard box
      ggplot2::annotate(
        geom = 'rect',
        xmin = pitch_dims$length - pitch_dims$penalty_box_length,
        ymin = (pitch_dims$width - pitch_dims$penalty_box_width) / 2,
        xmax = pitch_dims$length,
        ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$penalty_box_width) / 2),
        color = pitch_lines, 
        fill = pitch_color
      ),
      # Drawing 6 yard box
      ggplot2::annotate(
        geom = 'rect',
        xmin = pitch_dims$length - pitch_dims$six_yard_box_length,
        ymin = (pitch_dims$width - pitch_dims$six_yard_box_width) / 2,
        xmax = pitch_dims$length,
        ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$six_yard_box_width) / 2),
        color = pitch_lines, 
        fill = pitch_color
      ),
      # Drawing penalty spot
      ggplot2::annotate(
        geom = 'point',
        x = pitch_dims$length - pitch_dims$penalty_spot_distance,
        y = pitch_dims$width / 2,
        color = pitch_lines
      )
    )
  }
  else {
  list(
    # rectangular border outside of the field
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$origin_x - 4,
      ymin = pitch_dims$origin_y - 4,
      xmax = pitch_dims$length + 4,
      ymax = pitch_dims$width + 4,
      fill = pitch_color
    ),
    # Drawing goals
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$origin_x - 2,
      ymin = (pitch_dims$width - pitch_dims$goal_width) / 2,
      xmax = pitch_dims$origin_x,
      ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$goal_width) / 2),
      color = pitch_lines,
      fill = pitch_color
    ),
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$length,
      ymin = (pitch_dims$width - pitch_dims$goal_width) / 2,
      xmax = pitch_dims$length + 2,
      ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$goal_width) / 2),
      color = pitch_lines,
      fill = pitch_color
    ),
    # drawing outline of field
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$origin_x,
      ymin = pitch_dims$origin_y,
      xmax = pitch_dims$length,
      ymax = pitch_dims$width,
      color = pitch_lines, 
      fill = pitch_color
    ),
    # Drawing center circle
    ggplot2::annotation_custom(
      grob = grid::circleGrob(gp = grid::gpar(col = pitch_lines,
                                              fill = pitch_color,
                                              lwd  = 2)),
      xmin = (pitch_dims$length / 2) - 12,
      xmax = (pitch_dims$length / 2) + 12,
      ymin = (pitch_dims$width / 2) - 12,
      ymax = (pitch_dims$width / 2) + 12
    ),
    # Drawing D
    ggplot2::annotation_custom(
      grob = grid::circleGrob(gp = grid::gpar(col = pitch_lines,
                                              fill = pitch_color,
                                              lwd  = 2)),
      xmin = pitch_dims$origin_x,
      xmax = pitch_dims$penalty_spot_distance + pitch_dims$penalty_spot_distance,
      ymin = (pitch_dims$width / 2) - pitch_dims$penalty_spot_distance,
      ymax = (pitch_dims$width / 2) + pitch_dims$penalty_spot_distance
    ),
    ggplot2::annotation_custom(
      grob = grid::circleGrob(gp = grid::gpar(col = pitch_lines,
                                              fill = pitch_color,
                                              lwd  = 2)),
      xmin = pitch_dims$length - (2*pitch_dims$penalty_spot_distance),
      xmax = pitch_dims$length,
      ymin = (pitch_dims$width / 2) - 12,
      ymax = (pitch_dims$width / 2) + 12
    ),
    # drawing midfield line
    ggplot2::annotate(
      geom = 'segment',
      x = pitch_dims$length / 2,
      y = pitch_dims$origin_y,
      xend = pitch_dims$length / 2,
      yend = pitch_dims$width,
      color = pitch_lines
    ),
    # drawing center point
    ggplot2::annotate(
      geom = 'point',
      x = pitch_dims$length / 2,
      y = pitch_dims$width / 2,
      color = pitch_lines
    ),
    # Drawing 18 yard box
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$origin_x,
      ymin = (pitch_dims$width - pitch_dims$penalty_box_width) / 2,
      xmax = pitch_dims$penalty_box_length,
      ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$penalty_box_width) / 2),
      color = pitch_lines, 
      fill = pitch_color
    ),
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$length - pitch_dims$penalty_box_length,
      ymin = (pitch_dims$width - pitch_dims$penalty_box_width) / 2,
      xmax = pitch_dims$length,
      ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$penalty_box_width) / 2),
      color = pitch_lines, 
      fill = pitch_color
    ),
    # Drawing 6 yard box
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$origin_x,
      ymin = (pitch_dims$width - pitch_dims$six_yard_box_width) / 2,
      xmax = pitch_dims$six_yard_box_length,
      ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$six_yard_box_width) / 2),
      color = pitch_lines, 
      fill = pitch_color
    ),
    ggplot2::annotate(
      geom = 'rect',
      xmin = pitch_dims$length - pitch_dims$six_yard_box_length,
      ymin = (pitch_dims$width - pitch_dims$six_yard_box_width) / 2,
      xmax = pitch_dims$length,
      ymax = pitch_dims$width - ((pitch_dims$width - pitch_dims$six_yard_box_width) / 2),
      color = pitch_lines, 
      fill = pitch_color
    ),
    # Drawing penalty spot
    ggplot2::annotate(
      geom = 'point',
      x = pitch_dims$penalty_spot_distance,
      y = pitch_dims$width / 2,
      color = pitch_lines
    ),
    ggplot2::annotate(
      geom = 'point',
      x = pitch_dims$length - pitch_dims$penalty_spot_distance,
      y = pitch_dims$width / 2,
      color = pitch_lines
    )
  )
  }
}

# Test drawing pitch with shots
# The dataframe manipulation will be integrated into 01_ASAShotCharts.R to 
# save a condensed RDS of just shots to be used in app
# sample_shot_data <- readRDS('Google Drive/SoccerApps/ASAShootingApp_master/AppData/ShotsWithxG.rds') %>%
#   filter(year==2018 & team=='SJE') 
# 
# ggplot(data=sample_shot_data, aes(x=x, y=y, color=as.factor(xG_percentile))) +
#   draw_pitch(pitch_lines = 'black', pitch_color = 'white', 
#              data_source = 'Opta', size = 'half')+
#   geom_point(alpha=.8)+
#   coord_flip()+
#   theme(legend.position = 'bottom')+
#   pitch_theme +
#   scale_color_viridis_d(name = 'xG')
#   NULL

# Draw legend for xG, xA, etc? This may be done in the server script?

# Draw ggridges for 'radar' substitute? This may be done in the server script?