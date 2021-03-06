

### RINK PLOTS ###

#################################################################################

# Creates a data frame of xy-coordinates for a custom circle

circleFun = function(center = c(0,0), diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}


#################################################################################

# Creates a full rink plot in a ggplot object

fullrink = function(plt, a = 1) {
  
  require(ggplot2) # This function is to be used inside a ggplot object
  require(DataCombine) # Used for the InsertRow() function
  
  # Create data frames for faceoff circles
  lower_right_circle = circleFun(c(169, 20.5), 30, npoints = 100)
  upper_right_circle = circleFun(c(169, 64.5), 30, npoints = 100)
  lower_left_circle = circleFun(c(31, 20.5), 30, npoints = 100)
  upper_left_circle = circleFun(c(31, 64.5), 30, npoints = 100)
  centre_ice = circleFun(c(100, 42.5), 30, npoints = 100)
  
  # Create data frames for corners
  lower_right_corner = circleFun(c(187.5, 12.5), 25, npoints = 100)[76:100,]
  upper_right_corner = circleFun(c(187.5, 72.5), 25, npoints = 100)[1:25,]
  lower_left_corner = circleFun(c(12.5, 12.5), 25, npoints = 100)[51:75,]
  upper_left_corner = circleFun(c(12.5, 72.5), 25, npoints = 100)[26:50,]
  
  # Create data frames for creases
  left_crease = circleFun(c(189, 42.5), 12, npoints = 100)[38:62,] %>%
    InsertRow(NewRow = c(189.5, 38.5)) %>%
    InsertRow(NewRow = c(189.5, 46.5), 1)
  
  right_crease = rbind(circleFun(c(11, 42.5), 12, npoints = 100)[88:100,], circleFun(c(11, 42.5), 12, npoints = 100)[1:12,]) %>%
    InsertRow(NewRow = c(10.5, 38.5), 1) %>%
    InsertRow(NewRow = c(10.5, 46.5))
  
  return(
    plt +
      # Fix the xy-coordinates
      coord_fixed() +
      
      # Net and crease
      geom_segment(aes(x = 189.5, xend = 192.5, y = 39.5, yend = 39.5), colour = "grey", size = 0.5) +
      geom_segment(aes(x = 189.5, xend = 192.5, y = 45.5, yend = 45.5), colour = "grey", size = 0.5) +
      geom_segment(aes(x = 192.5, xend = 192.5, y = 39.5, yend = 45.5), colour = "grey", size = 0.5) +
      geom_polygon(data = left_crease, aes(x,y), fill = "skyblue", alpha = a) +
      geom_path(data = left_crease, aes(x = x, y = y), colour = "red", alpha = a) +
      geom_segment(aes(x = 10.5, xend = 7.5, y = 39.5, yend = 39.5), colour = "grey", size = 0.5) +
      geom_segment(aes(x = 10.5, xend = 7.5, y = 45.5, yend = 45.5), colour = "grey", size = 0.5) +
      geom_segment(aes(x = 7.5, xend = 7.5, y = 39.5, yend = 45.5), colour = "grey", size = 0.5) +
      geom_polygon(data = right_crease, aes(x,y), fill = "skyblue", alpha = a) +
      geom_path(data = right_crease, aes(x = x, y = y), colour = "red", alpha = a) +
      
      # Lines
      geom_segment(aes(x = 75, xend = 75, y = 0, yend = 85), colour = "blue", size = 1.5) +
      geom_segment(aes(x = 125, xend = 125, y = 0, yend = 85), colour = "blue", size = 1.5) +
      geom_segment(aes(x = 100, xend = 100, y = 0, yend = 85), colour = "red", size = 1.5) +
      geom_segment(aes(x = 190, xend = 190, y = 0, yend = 85), colour = "red") +
      geom_segment(aes(x = 10, xend = 10, y = 0, yend = 85), colour = "red") +
      
      # Faceoff Circles
      geom_path(data = upper_left_circle, aes(x, y), colour = "red", alpha = a) +
      geom_path(data = upper_right_circle, aes(x, y), colour = "red", alpha = a) +
      geom_path(data = lower_left_circle, aes(x, y), colour = "red", alpha = a) +
      geom_path(data = lower_right_circle, aes(x, y), colour = "red", alpha = a) +
      geom_path(data = centre_ice, aes(x, y), colour = "blue", alpha = a) +
      annotate("point", x = 169, y = 20.5, colour = "red", size = 1.8, alpha = a) +
      annotate("point", x = 169, y = 64.5, colour = "red", size = 1.8, alpha = a) +
      annotate("point", x = 120, y = 20.5, colour = "red", size = 1.8, alpha = a) +
      annotate("point", x = 120, y = 64.5, colour = "red", size = 1.8, alpha = a) +
      annotate("point", x = 31, y = 20.5, colour = "red", size = 1.8, alpha = a) +
      annotate("point", x = 31, y = 64.5, colour = "red", size = 1.8, alpha = a) +
      annotate("point", x = 80, y = 20.5, colour = "red", size = 1.8, alpha = a) +
      annotate("point", x = 80, y = 64.5, colour = "red", size = 1.8, alpha = a) +
      annotate("point", x = 100, y = 42.5, colour = "blue", size = 1.8, alpha = a) +
      
      # Boards
      geom_path(data = upper_left_corner, aes(x, y), colour = "black", size = 1.5) +
      geom_path(data = upper_right_corner, aes(x, y), colour = "black", size = 1.5) +
      geom_path(data = lower_left_corner, aes(x, y), colour = "black", size = 1.5) +
      geom_path(data = lower_right_corner, aes(x, y), colour = "black", size = 1.5) +
      geom_segment(aes(x = 0, xend = 0, y = 11.5, yend = 73.5), size = 1.5) +
      geom_segment(aes(x = 200, xend = 200, y = 11.5, yend = 73.5), size = 1.5) +
      geom_segment(aes(x = 11.5, xend = 188.5, y = 0, yend = 0), size = 1.5) +
      geom_segment(aes(x = 11.5, xend = 188.5, y = 85, yend = 85), size = 1.5)
  )
  
}


#################################################################################

# Creates a full rink plot in a ggplot object with a clear background

fullrink_white = function(plt, a = 1) {

  return(
    # Add in the rink
    fullrink(plt, a) +
      # Remove background
      theme_bw() +
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
            panel.grid = element_blank(), panel.border = element_blank())
  )
  
}


#################################################################################
#################################################################################
#################################################################################

### INLA HEATMAP RINK PLOTS ###


#################################################################################

# Creates a spatial map for the OZ based off of our INLA model results (based off ggregplot's ggField)

gg_OZ = function (Model, 
                  Mesh, 
                  Mat_Name = "w", 
                  Groups = 1, 
                  GroupLabels = NULL, 
                  family = "none",
                  Res = 300) {
  
  # Load necessary packages
  require(INLA)
  require(tidyverse)
  require(magrittr) 
  require(grid)
  require(reshape2)
  require(png)
  
  ## EXTRACT AND FORMAT DATA ##
  
  # Get mesh projections
  Projection <- inla.mesh.projector(Mesh, dims = c(Res, Res))
  
  # Create a full projection matrix
  Full.Projection <- expand.grid(x = Projection$x, y = Projection$y)
  
  # Get the total number of values in this matrix
  Dim1 <- nrow(Full.Projection)
  
  # Get the mean values for each xy-projection for one group
  if (Groups == 1) {
    Full.Projection$value <- c(inla.mesh.project(Projection, 
                                                 Model$summary.random[[Mat_Name]]$mean))
  }
  
  # Get the mean values for each xy-projection when there is more than 1 group
  else {
    mean_estimates = Model$summary.random[[Mat_Name]]$mean
    
    Full.Projection[, paste0("Group", 1:Groups)] <- apply(matrix(mean_estimates, ncol = Groups), 
                                                          2, function(x) c(inla.mesh.project(Projection, x)))
    
    Full.Projection <- melt(Full.Projection, id.vars = c(names(Full.Projection)[-which(names(Full.Projection) %in% paste0("Group", 1:Groups))]))
  }
  
  # Add in groups
  Full.Projection$Group <- rep(1:Groups, each = Dim1)
  
  # Create a function to convert link to response for logistic regression
  logit = function(x) {return(exp(x + Model$summary.fixed$mean[1])/ (1 + exp(x + Model$summary.fixed$mean[1])))}
  
  # Convert to logistic regression response
  if (family == "binomial") {
    Full.Projection = Full.Projection %>% 
      mutate(value = value %>% as.character() %>% as.numeric() %>% logit())
  }
  
  # Convert to poisson regression response
  if (family == "poisson") {
    Full.Projection = Full.Projection %>% 
      mutate(value = value %>% as.character() %>% as.numeric() %>% exp())
  }
  
  # Calculate fill value
  Full.Projection$Fill <- cut(Full.Projection$value, breaks = quantile(Full.Projection$value, 
                                                                        0:9/9, na.rm = T), labels = c(round(quantile(Full.Projection$value, 
                                                                                                                    0:9/9, na.rm = T), 2)[1:9]), include.lowest = T)
  # Remove NAs
  Full.Projection <- na.omit(Full.Projection)
  
  
  ## CREATE PLOT ##
  
  # Create the heatmap
  FieldPlot <- ggplot(Full.Projection, aes(x, y)) +
    geom_tile(colour = NA, aes(fill = Fill)) + 
    coord_fixed() + 
    labs(fill = "Mean", x = NULL, y = NULL) + 
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_continuous(expand = c(0, 0), limits = c(125, 200))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 85)) +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.text = element_blank())
  
  # Set custom specs for poisson regression outputs
  if (family == "poisson") {
    FieldPlot <- FieldPlot +
      scale_fill_viridis_d() +
      labs(fill = "Mean Count")
  }
  
  # Set custom specs for logistic regression outputs (shot to goal)
  if (family == "binomial") {
    FieldPlot <- FieldPlot +
      scale_fill_viridis_d(option = "magma") +
      labs(fill = "Mean xG")
  }
    
  if (Groups > 1) {
    
    # Label groups
    if (!is.null(GroupLabels)) {
      FacetLabels <- GroupLabels
    }
    else FacetLabels <- as.character(1:Groups)
    names(FacetLabels) <- as.character(1:Groups)
    
    # Separate plot by groups
    FieldPlot <- FieldPlot + 
      facet_wrap(~Group, labeller = as_labeller(FacetLabels)) + 
      theme(strip.background = element_blank())
  }

  
  # Load in rink plot png's
  image <- readPNG(here::here("Figures/Rink Outline/Rink2.png"))
  m <- image
  w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 1), nrow=dim(m)[1])
  
  image2 <- readPNG(here::here("Figures/Rink Outline/Rink.png"))
  m2 <- image2
  w2 <- matrix(rgb(m2[,,1],m2[,,2],m2[,,3], m2[,,4] * 0.3), nrow=dim(m2)[1])
  
  # Add rink to plot
  FieldPlot = FieldPlot +
    annotation_custom(rasterGrob(w, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf)+
    annotation_custom(rasterGrob(w2, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf)
  
  return(FieldPlot)
}


#################################################################################

# Creates a spatial map for the NZ based off of our INLA results (based off ggregplot's ggField)

gg_NZ = function (Model, 
                  Mesh, 
                  Mat_Name = "w", 
                  Groups = 1, 
                  GroupLabels = NULL, 
                  family = "none",
                  Res = 300) {
  
  # Load necessary packages
  require(INLA)
  require(tidyverse)
  require(magrittr) 
  require(grid)
  require(reshape2)
  require(png)
  
  ## EXTRACT AND FORMAT DATA ##
  
  # Get mesh projections
  Projection <- inla.mesh.projector(Mesh, dims = c(Res, Res))
  
  # Create a full projection matrix
  Full.Projection <- expand.grid(x = Projection$x, y = Projection$y)
  
  # Get the total number of values in this matrix
  Dim1 <- nrow(Full.Projection)
  
  # Get the mean values for each xy-projection for one group
  if (Groups == 1) {
    Full.Projection$Mat_Name <- c(inla.mesh.project(Projection, 
                                                    Model$summary.random[[Mat_Name]]$mean))
  }
  
  # Get the mean values for each xy-projection when there is more than 1 group
  else {
    mean_estimates = Model$summary.random[[Mat_Name]]$mean
    
    Full.Projection[, paste0("Group", 1:Groups)] <- apply(matrix(mean_estimates, ncol = Groups), 
                                                          2, function(x) c(inla.mesh.project(Projection, x)))
    
    Full.Projection <- melt(Full.Projection, id.vars = c(names(Full.Projection)[-which(names(Full.Projection) %in% paste0("Group", 1:Groups))]))
  }
  
  # Add in groups
  Full.Projection$Group <- rep(1:Groups, each = Dim1)
  
  # Create a function to convert link to response for logistic regression
  logit = function(x) {return(exp(x) / (1 + exp(x)))}
  
  # Convert to logistic regression response
  if (family == "binomial") {
    Full.Projection = Full.Projection %>% 
      mutate(value = value %>% as.character() %>% as.numeric() %>% logit())
  }
  
  # Convert to poisson regression response
  if (family == "poisson") {
    Full.Projection = Full.Projection %>% 
      mutate(value = value %>% as.character() %>% as.numeric() %>% exp())
  }
  
  # Calculate fill value
  Full.Projection$Fill <- cut(Full.Projection$value, breaks = quantile(Full.Projection$value, 
                                                                       0:9/9, na.rm = T), labels = c(round(quantile(Full.Projection$value, 
                                                                                                                    0:9/9, na.rm = T), 2)[1:9]), include.lowest = T)
  # Remove NAs
  Full.Projection <- na.omit(Full.Projection)
  
  
  ## CREATE PLOT ##
  
  # Create the heatmap
  FieldPlot <- ggplot(Full.Projection, aes(x, y)) +
    geom_tile(colour = NA, aes(fill = Fill)) + 
    coord_fixed() + 
    labs(fill = "Mean", x = NULL, y = NULL) + 
    guides(fill = guide_legend(reverse = TRUE)) +
    #scale_x_continuous(expand = c(0, 0), limits = c(125, 200))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 85)) +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.text = element_blank())
  
  # Set custom specs for poisson regression outputs
  if (family == "poisson") {
    FieldPlot <- FieldPlot +
      scale_fill_viridis_d() +
      labs(fill = "Mean Count")
  }
  
  # Set custom specs for logistic regression outputs (shot to goal)
  if (family == "binomial") {
    FieldPlot <- FieldPlot +
      scale_fill_viridis_d(option = "magma") +
      labs(fill = "Mean xG")
  }
  
  if (Groups > 1) {
    
    # Label groups
    if (!is.null(GroupLabels)) {
      FacetLabels <- GroupLabels
    }
    else FacetLabels <- as.character(1:Groups)
    names(FacetLabels) <- as.character(1:Groups)
    
    # Separate plot by groups
    FieldPlot <- FieldPlot + 
      facet_wrap(~Group, labeller = as_labeller(FacetLabels)) + 
      theme(strip.background = element_blank())
  }
  
  
  # Load in rink plot png's
  image <- readPNG(here::here("Figures/Rink Outline/Rink2_Entries.png"))
  m <- image
  w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 1), nrow=dim(m)[1])
  
  image2 <- readPNG(here::here("Figures/Rink Outline/Rink_Entries.png"))
  m2 <- image2
  w2 <- matrix(rgb(m2[,,1],m2[,,2],m2[,,3], m2[,,4] * 0.3), nrow=dim(m2)[1])
  
  # Add rink to plot
  FieldPlot = FieldPlot +
    annotation_custom(rasterGrob(w, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      75, 200, 0, 85)+
    annotation_custom(rasterGrob(w2, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      75, 200, 0, 85)
  
  return(FieldPlot)
}



