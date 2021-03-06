

samp_frame <- readRDS("all_samples_so_far.Rda")

pass_frame <- samp_frame[which(samp_frame$prev_event %in% c("Play", "Incomplete Play")),]

hist(pass_frame$Expected_Goals)

library(tidyverse) # For various data analysis functions

library(here) # To call or save code/data in certain folders

library(INLA) # To fit model with INLA

source(here::here("Code/2. Functions/Plotting-Functions.R")) # For plotting function



oz_boundary = data.frame(
  x = c(125, 200, 200, 125, 125),
  y = c(0, 0, 85, 85, 0)
)


model_domain = as.matrix(oz_boundary)
model_coords = as.matrix(cbind(pass_frame$same_direction_x, pass_frame$same_direction_y))

# Create the mesh
Mesh = inla.mesh.2d(model_coords, loc.domain = model_domain, max.edge = c(12, 12), offset = c(1,2), cutoff = 8)

# Plot the mesh along with the data points
plot(Mesh)


# Set the knots
knots = c(0, 1, 2, 5,10,20, max(pass_frame$time_since_entry.x))

# Create 1D mesh
time_Mesh <- inla.mesh.1d(loc = knots)

model_A_matrix = inla.spde.make.A(Mesh, loc = model_coords, n.group = length(knots), group = pass_frame$time_since_entry.x, group.mesh = time_Mesh)

# Set the PC Matern prior
model_spde = inla.spde2.pcmatern(mesh = Mesh, prior.range = c(22, 0.6), prior.sigma = c(0.5, 0.5))

# Create the weight index
model_w_index = inla.spde.make.index("w", n.spde = model_spde$n.spde, n.group = length(knots))

pass_frame$is.deficit <- ifelse(pass_frame$score_effect =="Deficit",1,0)
pass_frame$is.lead <- ifelse(pass_frame$score_effect =="Lead",1,0)

pass_frame$last_minute <- ifelse(pass_frame$seconds > 1140,1,0)

pass_frame$time_last_minute <- ifelse(pass_frame$last_minute == 1, abs(pass_frame$seconds - 1200),0)

pass_frame$xg_0 <- ifelse(pass_frame$Expected_Goals ==0,1,0)

X = pass_frame %>%
  # Select necessary columns to be used in model
  select(is.deficit, is.lead, pass_count, last_minute, time_last_minute)


model_stack = inla.stack(
  
  # Add our response variable
  data = list(y = pass_frame$xg_0), 
  
  # Add a list of the weights on the effects
  A = list(1, 1, model_A_matrix),
  
  # Add spatial effects
  effects = list(
    
    # Intercept, repeat 1 n times
    Intercept = rep(1, nrow(pass_frame)),
    
    # Add our model matrix
    X = X,
    
    # Add spatial effect
    model_w_index
    
  ))


model_formula = as.formula(paste0("y ~ -1 + Intercept ", 
                                  " + is.deficit + is.lead + last_minute + time_last_minute",
                                  " + f(w, model = model_spde, group = w.group, control.group = list(model = 'ar1'))"))


model = inla(model_formula, 
             
             family = "binomial",
             
             data = inla.stack.data(model_stack),
             
             control.compute = list(dic = TRUE, config = TRUE),
             
             control.predictor = list(A = inla.stack.A(model_stack)),
             
             num.threads = 8
)


model_stack2 = inla.stack(
  
  # Add our response variable
  data = list(y = pass_frame$Expected_Goals[pass_frame$xg_0 == 0]), 
  
  # Add a list of the weights on the effects
  A = list(1, 1, model_A_matrix),
  
  # Add spatial effects
  effects = list(
    
    # Intercept, repeat 1 n times
    Intercept = rep(1, nrow(pass_frame[pass_frame$xg_0 == 0])),
    
    # Add our model matrix
    X = X,
    
    # Add spatial effect
    model_w_index
    
  ))


summary(model)

gg_OZ(model, Mesh, Groups = length(knots),family = "binomial")


model2 = inla(model_formula, 
             
             family = "beta",
             
             data = inla.stack.data(model_stack),
             
             control.compute = list(dic = TRUE, config = TRUE),
             
             control.predictor = list(A = inla.stack.A(model_stack)),
             
             num.threads = 8
)


