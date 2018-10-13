library(StatsBombR)
library(dplyr)
library(SBpitch)
library(ggplot2)
comp <- FreeCompetitions()
matches <- FreeMatches(comp$competition_id)
wc <- matches %>%
  subset(competition.competition_id == 43)

events <- get.matchFree(wc[4,])

create_StatsBomb_ShotMap <- function(grass_colour, line_colour, background_colour, goal_colour){
  
  theme_blankPitch = function(size=12) { 
    theme(
      #axis.line=element_blank(), 
      axis.text.x=element_blank(), 
      axis.text.y=element_blank(), 
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"), 
      #axis.ticks.margin=unit(0, "lines"), 
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(), 
      legend.background=element_rect(fill=background_colour, colour=NA), 
      legend.key=element_rect(colour=background_colour,fill=background_colour), 
      legend.key.size=unit(1.2, "lines"), 
      legend.text=element_text(size=size), 
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour), 
      #       panel.border=element_blank(), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      panel.spacing=element_blank(), 
      plot.background=element_blank(), 
      plot.margin=unit(c(0, 0, 0, 0), "lines"), 
      plot.title=element_text(size=size*1.2), 
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  
  ymin <- 0 # minimum width
  ymax <- 80 # maximum width
  xmin <- 60 # minimum length
  xmax <- 120 # maximum length
  
  # Defining features along the length
  boxEdgeOff <- 102
  sixYardOff <- 114
  penSpotOff <- 108
  halfwayline <- 60
  
  # Defining features along the width
  boxEdgeLeft <- 18
  boxEdgeRight <- 62
  sixYardLeft <- 30 
  sixYardRight <- 50
  goalPostLeft <- 36
  goalPostRight <- 44
  CentreSpot <- 40   
  
  # other dimensions
  centreCirle_d <- 20   
  
  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  #### create leftD arc ####
  dArc <- circleFun(c((40),(penSpotOff)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  dArc <- dArc[which(dArc$y <= (boxEdgeOff)),]
  
  ## initiate the plot, set some boundries to the plot
  p <- ggplot() + xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
    # add the theme 
    theme_blankPitch() +
    # add the base rectangle of the pitch 
    geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill = grass_colour, colour = line_colour) +
    # add the 18 yard box offensive
    geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
    # add the six yard box offensive
    geom_rect(aes(xmin=sixYardLeft, xmax=sixYardRight, ymin=sixYardOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
    # add the arc circle 
    geom_path(data=dArc, aes(x=x,y=y), colour = line_colour) +
    # add penalty spot 
    geom_point(aes(x = CentreSpot , y = penSpotOff), colour = line_colour) +
    # add the goal offensive
    geom_segment(aes(x = goalPostLeft, y = xmax, xend = goalPostRight, yend = xmax),colour = goal_colour, size = 1)
  
  return(p)
  
}

p <- create_StatsBomb_ShotMap("#ffffff", "#A9A9A9", "#ffffff", "#000000")
p

shots <- events %>%
  subset(type.name == "Shot" & team.name == "France")

shots$location.x <- unlist(lapply(shots$location, `[[`,1))
shots$location.y <- unlist(lapply(shots$location, `[[`,2))

p +
  geom_point(data = shots, aes(x = location.y, y = location.x, size = shot.statsbomb_xg, color = shot.outcome.name)) +
  guides(size = FALSE) +
  labs(color = "Shot Outcome") +
  theme(legend.position = "right")

shots$location.x <- unlist(lapply(shots$location, `[[`,1))
shots$location.y <- unlist(lapply(shots$location, `[[`,2))

pogba <- events %>%
  subset(player.name == "Paul Pogba")

pogba$location.x <- unlist(lapply(pogba$location, `[[`,1))
pogba$location.y <- unlist(lapply(pogba$location, `[[`,2))

create_Pitch() +
  geom_point(data = subset(pogba, type.name == "Ball Receipt*"), 
             aes(x = location.x, y = location.y, color = under_pressure)) +
  guides(color = FALSE)

passes <- events %>%
  subset(type.name == "Pass")

passes$location.x <- unlist(lapply(passes$location, `[[`,1))
passes$location.y <- unlist(lapply(passes$location, `[[`,2))
passes$end.location.x <- unlist(lapply(passes$pass.end_location, `[[`,1))
passes$end.location.y <- unlist(lapply(passes$pass.end_location, `[[`,2))
#passes$pass.outcome.name[is.na(passes$pass.outcome.name) == TRUE] <- "Complete"
passes$pass.outcome.name[passes$pass.outcome.name == "Out"] <- "Incomplete"
passes$pass.outcome.name[passes$pass.outcome.name == "Pass Offside"] <- "Incomplete"
#passes <- passes[!(passes$pass.outcome.name == "Unknown"),] 
table(passes$pass.outcome.name)

library(ggthemes)
p <- create_Pitch() +
  geom_segment(data = subset(passes, player.name == "Paul Pogba"), aes(x = location.x, y = location.y, 
                                  xend = end.location.x, yend = end.location.y, color = pass.outcome.name), 
               arrow = arrow(length = unit(.3, "cm"))) +
  geom_point(data = subset(passes, player.name == "Paul Pogba"), 
             aes(x = location.x, y = location.y, color = under_pressure)) +
  guides(color = FALSE)
p

pogba_passes <- events %>%
  subset(type.name == "Pass" & player.name == "Paul Pogba")
