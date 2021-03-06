


### GLUE MODEL SAMPLING ###

#################################################################################

# Predict X Location

gen_samp_x_location <- function(n_samples, x_loc, y_loc, prev_transition, transition, time_since_entry, 
                                pass_type = "none", model = x_prediction, mesh = x_prediction_mesh){
  
  prev_behind_net = ifelse(x_loc >= 189, 1, 0)
  
  transition = case_when(
    transition == "Play" & pass_type == "Direct" ~ "Direct",
    transition == "Play" & pass_type == "Indirect" ~ "Indirect",
    transition == "Play" ~ "Direct",
    TRUE ~ transition
  )
  
  direct_pass = 0.68
  
  prev_play = ifelse(prev_transition == "Play", 1, 0)
  prev_shot = ifelse(prev_transition == "Shot", 1, 0)
  prev_recovery = ifelse(prev_transition == "Recovery", 1, 0)
  prev_zone_entry = ifelse(prev_transition == "Zone Entry", 1, 0)
  
  time_int_shot = ifelse(transition == "Shot", time_since_entry, 0)
  
  X_location_sampling <- inla.posterior.sample(n=n_samples, result = model)
  
  xnames <- rownames(X_location_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', "direct_pass", "prev_behind_net", "prev_play", "prev_shot",
                  "prev_recovery", "prev_zone_entry", "time_since_entry", 
                  'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples <- sapply(X_location_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]], direct_pass = spl$latent[idx[[2]]], prev_behind_net = spl$latent[idx[[3]]], 
      prev_play = spl$latent[idx[[4]]], prev_shot = spl$latent[idx[[5]]],  prev_recovery = spl$latent[idx[[6]]], 
      prev_zone_entry = spl$latent[idx[[7]]], time_since_entry = spl$latent[idx[[8]]], 
      w = spl$latent[idx[[9]]]))
  
  NGroups <- 5
  
  group_mat <- data.frame(Transition = c("Turnover","Recovery", "Shot", "Direct", "Indirect"), Number = c(1:5))
  
  group_n <- group_mat[which(group_mat$Transition == transition),2]
  
  HostA_x_pred <- inla.spde.make.A(mesh = mesh, # Leave
                                   loc = cbind(x_loc,y_loc), # Leave
                                   group = group_n,# this must be a numeric value counting from 1. If the groups variable is a factor, this will happen by default.
                                   n.group = NGroups) 
  
  
  out <- as.matrix(cbind(Intercept=1, direct_pass = direct_pass, prev_behind_net = prev_behind_net, prev_play, prev_shot,
                         prev_recovery = prev_recovery, prev_zone_entry = prev_zone_entry,
                         time_since_entry = time_since_entry, w = HostA_x_pred)%*%mat.samples)
  
  out
  
}


#################################################################################

# Predict Y Location

gen_samp_y_location <- function(n_samples, x_prev, y_prev, x_loc, prev_transition, transition, time_since_entry,
                                pass_type = "none", model = y_prediction, mesh = y_prediction_mesh){
  
  prev_behind_net = ifelse(x_prev >= 189, 1, 0)
  
  transition = case_when(
    transition == "Play" & pass_type == "Direct" ~ "Direct",
    transition == "Play" & pass_type == "Indirect" ~ "Indirect",
    transition == "Play" ~ "Direct",
    TRUE ~ transition
  )
  
  direct_pass = 0.68
  
  prev_play = ifelse(prev_transition == "Play", 1, 0)
  prev_shot = ifelse(prev_transition == "Shot", 1, 0)
  prev_recovery = ifelse(prev_transition == "Recovery", 1, 0)
  prev_turnover = ifelse(prev_transition == "Turnover", 1, 0)
  prev_zone_entry = ifelse(prev_transition == "Zone Entry", 1, 0)
  
  time_int_shot = ifelse(transition == "Shot", time_since_entry, 0)
  
  prev_left_side = ifelse(y_prev > 42.5, 1, 0)
  
  x_normalized = (x_loc - 156.5)/27.4
  
  x_normalized_int_right = x_normalized * (1 - prev_left_side)
  
  Y_location_sampling <- inla.posterior.sample(n=n_samples, result = model)
  
  xnames <- rownames(Y_location_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', "prev_behind_net", "prev_play", "prev_recovery", "prev_shot", "prev_zone_entry", "time_zone_entry",
                  "x_normalized", "x_normalized_int_right", 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples <- sapply(Y_location_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]], prev_behind_net=spl$latent[idx[[2]]], prev_play=spl$latent[idx[[3]]], prev_recovery=spl$latent[idx[[4]]],
      prev_shot=spl$latent[idx[[5]]], prev_zone_entry=spl$latent[idx[[6]]], time_since_entry=spl$latent[idx[[7]]], x_normalized=spl$latent[idx[[8]]], 
      x_normalized_int_right=spl$latent[idx[[9]]], w=spl$latent[idx[[10]]]))
  
  NGroups <- 5
  
  group_mat <- data.frame(Transition = c("Turnover","Recovery","Shot", "Direct", "Indirect"), Number = c(1:5))
  
  group_n <- group_mat[which(group_mat$Transition == transition),2]
  
  HostA_y_pred <- inla.spde.make.A(mesh = mesh, # Leave
                                   loc = cbind(x_prev, y_prev), # Leave
                                   group = group_n,# this must be a numeric value counting from 1. If the groups variable is a factor, this will happen by default.
                                   n.group = NGroups) 
  
  
  out <- as.matrix(cbind(Intercept=1, prev_behind_net = prev_behind_net, prev_play = prev_play, prev_recovery = prev_recovery,
                         prev_shot = prev_shot, prev_zone_entry = prev_zone_entry, time_since_entry = time_since_entry,
                         x_normalized = x_normalized, x_normalized_int_right = x_normalized_int_right, w = HostA_y_pred)%*%mat.samples)
  
  out
  
}


#################################################################################

# Predict elapsed time

gen_samp_time <- function(n_samples, x_loc, y_loc, transition, prev_x_loc, prev_y_loc, prev_transition, model = time_to_event, mesh = time_to_event_mesh){
  
  Time_sampling <- inla.posterior.sample(n=n_samples, result = model)
  
  xnames <- rownames(Time_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept','distance_last_event','prev_direct_pass','prev_indirect_pass',
                  'prev_turnover','prev_recovery', 'prev_shot','int_direct_dist','int_indirect_dist','int_turnover_dist',
                  'int_recovery_dist','int_shot_dist',  'w'), function(nam) ## for each effect
                    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples <- sapply(Time_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]], distance_last_event = spl$latent[idx[[2]]], prev_direct_pass = spl$latent[idx[[3]]],
      prev_indirect_pass = spl$latent[idx[[4]]], prev_turnover = spl$latent[idx[[5]]], prev_recovery = spl$latent[idx[[6]]],
      prev_shot =spl$latent[idx[[7]]], int_direct_dist = spl$latent[idx[[8]]], int_indirect_dist = spl$latent[idx[[9]]],
      int_turnover_dist = spl$latent[idx[[10]]], int_recovery_dist = spl$latent[idx[[11]]], int_shot_dist = spl$latent[idx[[12]]],
      w=spl$latent[idx[[13]]]))
  
  NGroups <- 4
  
  Mesh_time <- mesh
  
  group_mat <- data.frame(Transition = c("Turnover","Recovery","Shot", "Play"), Number = c(1:4))
  
  group_n <- group_mat[which(group_mat$Transition == transition),2]
  
  Host_time <- inla.spde.make.A(Mesh_time, # Leave
                                loc = cbind(x_loc,y_loc), # Leave
                                group = group_n,# this must be a numeric value counting from 1. If the groups variable is a factor, this will happen by default.
                                n.group = NGroups) 
  
  
  distance_last_event <- sqrt((x_loc - prev_x_loc)^2 + (y_loc - prev_y_loc)^2)
  
  prev_direct_pass <- ifelse(prev_transition == "Play",1,0)
  prev_indirect_pass <- ifelse(prev_transition =="Incomplete Play",1,0)
  
  prev_turnover <- ifelse(prev_transition =="Turnover",1,0)
  prev_recovery <- ifelse(prev_transition =="Recovery",1,0)
  
  prev_shot <- ifelse(prev_transition =="Shot",1,0)
  
  int_direct_dist =  distance_last_event*prev_direct_pass
  
  int_indirect_dist = distance_last_event*prev_indirect_pass
  
  int_turnover_dist = distance_last_event*prev_turnover
  
  int_recovery_dist = distance_last_event*prev_recovery
  
  int_shot_dist = distance_last_event*prev_shot
  
  
  out <- as.matrix(cbind(Intercept = 1, distance_last_event = distance_last_event, prev_direct_pass = prev_direct_pass,
                         prev_indirect_pass = prev_indirect_pass, prev_turnover = prev_turnover,
                         prev_shot = prev_shot, int_direct_dist = int_direct_dist, int_indirect_dist = int_indirect_dist,
                         int_turnover_dist = int_turnover_dist, int_recovery_dist = int_recovery_dist,
                         int_shot_dist = int_shot_dist, w = Host_time)%*%mat.samples)
  
  exp(out)
  
}


#################################################################################

# Predict direct/indirect passes

gen_samp_direct_pass <- function(n_samples, x_loc, y_loc, prev_event, score_state, model = direct_pass, mesh = direct_pass_mesh){
  
  direct_sampling <- inla.posterior.sample(n=n_samples, result = direct_pass)
  
  prev_pass <-   ifelse(prev_event %in% c("Play", "Incomplete Play"),1,0)
  
  prev_recovery <- ifelse(prev_event == "Recovery",1,0)
  
  lead <- ifelse(score_state == "Lead",1,0)
  
  deficit <- ifelse(score_state =="Deficit",1,0)
  
  xnames <- rownames(direct_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'prev_pass','prev_recovery', 'lead','deficit', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples <- sapply(direct_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]], prev_pass = spl$latent[idx[[2]]], prev_recovery = spl$latent[idx[[3]]],
      lead = spl$latent[idx[[4]]], deficit = spl$latent[idx[[5]]], w=spl$latent[idx[[6]]]))
  
  HostA_pass <- inla.spde.make.A(direct_pass_mesh, # Leave
                                 loc = cbind(x_loc,y_loc)) 
  
  out <- as.matrix(cbind(Intercept=1,prev_pass = prev_pass, prev_recovery = prev_recovery, lead = lead, deficit = deficit, w = HostA_pass)%*%mat.samples)
  
  exp(out)/(1 + exp(out))
}


#################################################################################


# Predict pass target xy-location

gen_samp_pass_target <- function(n_samples, x_loc, y_loc, pass_type, x_model = pass_x_target, y_model = pass_y_target, mesh = pass_target_mesh){
  
  X_Pass_target_sampling <- inla.posterior.sample(n=n_samples, result = x_model)
  
  xnames <- rownames(X_Pass_target_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples <- sapply(X_Pass_target_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))
  
  NGroups <- 2
  
  group_mat <- data.frame(Pass_type = c("Direct", "Indirect"), Number = c(1:2))
  
  group_n <- group_mat[which(group_mat$Pass_type == pass_type),2]
  
  HostA_x_targ <- inla.spde.make.A(pass_target_mesh, # Leave
                                   loc = cbind(x_loc,y_loc), # Leave
                                   group = group_n,# this must be a numeric value counting from 1. If the groups variable is a factor, this will happen by default.
                                   n.group = NGroups) 
  
  
  out_x <- as.matrix(cbind(Intercept=1, w = HostA_x_targ)%*%mat.samples)
  
  Y_Pass_target_sampling <- inla.posterior.sample(n=n_samples, result = y_model)
  
  
  xnames <- rownames(Y_Pass_target_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples_y <- sapply(Y_Pass_target_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))
  
  
  out_y <- as.matrix(cbind(Intercept=1, w = HostA_x_targ)%*%mat.samples_y)
  
  list(out_x,out_y)
  
}


#################################################################################

# Predict entry type decision

gen_samp_entry_decision = function(n_samples, y_loc, offensive_score_diff, period, model = entry_decision) {
  
  # Take n samples from the posterior distribution
  entry_sampling = inla.posterior.sample(n = n_samples, result = model)
  
  # Find the rownames that correspond to our model parameters
  x_names = rownames(entry_sampling[[1]]$latent)
  
  idx = lapply(c("Intercept", "offensive_score_diff", "period", "period:offensive_score_diff", "i"), function(nam)
    which(substr(x_names, 1, nchar(nam)) == nam))
  
  # Extract the sample values for each parameter
  mat.samples = sapply(entry_sampling, function(spl)
    c(Intercept = spl$latent[idx[[1]]], offensive_score_diff = spl$latent[idx[[2]]], 
      period = spl$latent[idx[[3]][[1]]], period_score_effect = spl$latent[idx[[4]]], i = spl$latent[idx[[5]]]))
  
  # Create a 1D inla mesh with knots from 0-85
  Mesh = inla.mesh.1d(0:85)
  
  # Calculate the spatial weight matrix at our y-location
  shot_A = inla.mesh.1d.A(Mesh, loc = y_loc)
  
  # Output the value of the link function for the sample(s)
  out = as.matrix(cbind(Intercept = 1, offensive_score_diff = offensive_score_diff, period = period, 
                        `offensive_score_diff:period` = offensive_score_diff*period, i = shot_A) %*% mat.samples)
  
  # Convert from link to response
  out_controlled = exp(out) / (1 + exp(out))
  
  return(out_controlled)
  
}


#################################################################################

# Predict if a shot will be a one-timer

gen_samp_one_timer <- function(n_samples, shot_x, shot_y, pass_x, pass_y, detail_3, time_since_entry, time_since_last_pass, 
                               one_timer_model = one_timer, shot_mesh = one_timer_shot_mesh, pass_mesh = one_timer_pass_mesh, 
                               time_mesh = one_timer_time_mesh, time_mesh_random = one_timer_time_mesh_random) {
  
  # Take n samples from the posterior distribution
  one_timer_sampling = inla.posterior.sample(n = n_samples, result = one_timer_model)
  
  # Find the rownames that correspond to our model parameters
  x_names = rownames(one_timer_sampling[[1]]$latent)
  
  idx = lapply(c("Intercept", "detail_3", "g", "w"), function(nam)
    which(substr(x_names, 1, nchar(nam)) == nam))
  
  # Extract the sample values for each parameter
  mat.samples = sapply(one_timer_sampling, function(spl)
    c(Intercept = spl$latent[idx[[1]]], detail_3 = spl$latent[idx[[2]]], g = spl$latent[idx[[3]]], w = spl$latent[idx[[4]]]))
  
  # Calculate the spatial weight matrix for both our shot and pass xy-locations and time since entry
  shot_A = inla.spde.make.A(shot_mesh, loc = cbind(shot_x, shot_y), group = time_since_entry, group.mesh = time_mesh_random, n.group = 5)
  pass_A = inla.spde.make.A(pass_mesh, loc = cbind(pass_x, pass_y), group = time_since_last_pass, group.mesh = time_mesh, n.group = 5)
  
  # Output the value of the link function for the sample(s)
  out = as.matrix(cbind(Intercept = 1, detail_3 = detail_3, g = pass_A, w = shot_A) %*% mat.samples)
  
  # Convert from link to response
  out = exp(out) / (1+exp(out))
  
  return(out)
  
}


#################################################################################

# Predict if there is traffic on a shot that is preceded by a pass

gen_samp_traffic_with_pass <- function(n_samples, shot_x, shot_y, pass_x, pass_y, time_since_entry, traffic_model = traffic_with_pass, 
                                       shot_mesh = traffic_wp_shot_mesh, pass_mesh = traffic_wp_pass_mesh, time_mesh = traffic_wp_time_mesh) {
  
  # Take n samples from the posterior distribution
  traffic_sampling = inla.posterior.sample(n = n_samples, result = traffic_model)
  
  # Find the rownames that correspond to our model parameters
  x_names = rownames(traffic_sampling[[1]]$latent)
  
  idx = lapply(c("Intercept", "g", "w"), function(nam)
    which(substr(x_names, 1, nchar(nam)) == nam))
  
  # Extract the sample values for each parameter
  mat.samples = sapply(traffic_sampling, function(spl)
    c(Intercept = spl$latent[idx[[1]]], g = spl$latent[idx[[2]]], w = spl$latent[idx[[3]]]))
  
  # Calculate the spatial weight matrix for both our shot and pass xy-locations and time since entry
  shot_A = inla.spde.make.A(shot_mesh, loc = cbind(shot_x, shot_y), group = time_since_entry, group.mesh = time_mesh, n.group = 5)
  pass_A = inla.spde.make.A(pass_mesh, loc = cbind(pass_x, pass_y), group = time_since_entry, group.mesh = time_mesh, n.group = 5)
  
  # Output the value of the link function for the sample(s)
  out = as.matrix(cbind(Intercept = 1, g = pass_A, w = shot_A) %*% mat.samples)
  
  # Convert from link to response
  out = exp(out) / (1+exp(out))
  
  return(out)
  
}


#################################################################################

# Predict if there is traffic on a shot that is NOT preceded by a pass

gen_samp_traffic_no_pass <- function(n_samples, shot_x, shot_y, time_since_entry, traffic_model = traffic_no_pass, shot_mesh = traffic_np_shot_mesh, 
                                    time_mesh = traffic_np_time_mesh) {
  
  # Take n samples from the posterior distribution
  traffic_sampling = inla.posterior.sample(n = n_samples, result = traffic_model)
  
  # Find the rownames that correspond to our model parameters
  x_names = rownames(traffic_sampling[[1]]$latent)
  
  idx = lapply(c("Intercept", "w"), function(nam)
    which(substr(x_names, 1, nchar(nam)) == nam))
  
  # Extract the sample values for each parameter
  mat.samples = sapply(traffic_sampling, function(spl)
    c(Intercept = spl$latent[idx[[1]]], w = spl$latent[idx[[2]]]))
  
  # Calculate the spatial weight matrix for both our shot and pass xy-locations and time since entry
  shot_A = inla.spde.make.A(shot_mesh, loc = cbind(shot_x, shot_y), group = time_since_entry, group.mesh = time_mesh, n.group = 5)
  
  # Output the value of the link function for the sample(s)
  out = as.matrix(cbind(Intercept = 1, w = shot_A) %*% mat.samples)
  
  # Convert from link to response
  out = exp(out) / (1+exp(out))
  
  return(out)
  
}



#################################################################################
#################################################################################
#################################################################################



### TRANSITION PROBABILITIES ###

#################################################################################

# Dump-in entry transition probabilities

dump_transition_probabilites <- function(n_samples, x_loc, y_loc,prev_event, score_state, dump_2_recovery = dumped_2_recovery, 
                                         dump_2_turnover = dumped_2_turnover, dump_mesh = dumped_2_recovery_mesh){
  
  Dump_2_Recovery_sampling <- inla.posterior.sample(n=n_samples, result = dump_2_recovery)
  
  xnames <- rownames(Dump_2_Recovery_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples_rec <- sapply(Dump_2_Recovery_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))
  
  group_n <- ifelse(score_state =="Deficit",1,ifelse(score_state == "Close",2,3))
  
  A_dump_recovery = inla.spde.make.A(dump_mesh, loc = cbind(x_loc,y_loc),
                                     group = group_n,
                                     n.group = 3)
  
  out_recovery <- exp(as.matrix(cbind(Intercept=1, w = A_dump_recovery)%*%mat.samples_rec))
  
  
  #################################
  
  Dump_2_Turnover_sampling <- inla.posterior.sample(n=n_samples, result = dump_2_turnover)
  
  xnames <- rownames(Dump_2_Turnover_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples_turn <- sapply(Dump_2_Turnover_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))
  
  
  out_turnover <- exp(as.matrix(cbind(Intercept=1, w = A_dump_recovery)%*%mat.samples_turn))
  
  
  out_probabilities <- data.frame(probability_recover = out_recovery/(out_recovery + out_turnover), probability_turnover = out_turnover/(out_recovery + out_turnover))
  
  out_probabilities
  
  
}


#################################################################################

# Controlled entry transition probabilities

controlled_transition_probabilities = function(n_samples, y_loc, control_2_shot = controlled_2_shot, control_2_pass = controlled_2_play, 
                                               control_2_turnover = controlled_2_turnover) {
  
  # Determine if the entry was near the boards
  is_near_boards = ifelse(y_loc < 10 | y_loc > 75, 1, 0)
  
  # Calculate the mesh (same for all transitions)
  Mesh = inla.mesh.1d(0:85)
  
  # Calculate the spatial weight matrix at our xy-location (same for all transitions)
  control_A = inla.mesh.1d.A(Mesh, loc = y_loc)
  
  ##############################
  ## CONTROLLED ENTRY TO SHOT ##
  
  # Take n samples from the posterior distribution
  control_2_shot_sampling = inla.posterior.sample(n = n_samples, result = control_2_shot)
  
  # Find the rownames that correspond to our model parameters
  x_names = rownames(control_2_shot_sampling[[1]]$latent) 
  
  idx = lapply(c("Intercept", "is_near_boards", "i"), function(nam)
    which(substr(x_names, 1, nchar(nam))==nam))
  
  # Extract the sample values for each parameter
  mat.samples_shot = sapply(control_2_shot_sampling, function(spl)
    c(Intercept = spl$latent[idx[[1]]], is_near_boards = spl$latent[idx[[2]]], i = spl$latent[idx[[3]]][1:86]))
  
  # Output the value of the link function for the sample(s)
  out_shot = as.matrix(cbind(Intercept = 1, is_near_boards = is_near_boards, i = control_A) %*% mat.samples_shot)
  
  # Convert link to response
  out_shot = exp(out_shot)
  
  ##############################
  ## CONTROLLED ENTRY TO PASS ##
  
  # Take n samples from the posterior distribution
  control_2_pass_sampling = inla.posterior.sample(n = n_samples, result = control_2_pass)
  
  # Find the rownames that correspond to our model parameters
  x_names = rownames(control_2_pass_sampling[[1]]$latent) 
  
  idx = lapply(c("Intercept", "is_near_boards", "i"), function(nam)
    which(substr(x_names, 1, nchar(nam))==nam))
  
  # Extract the sample values for each parameter
  mat.samples_pass = sapply(control_2_pass_sampling, function(spl)
    c(Intercept = spl$latent[idx[[1]]], is_near_boards = spl$latent[idx[[2]]], i = spl$latent[idx[[3]][1:86]]))
  
  # Output the value of the link function for the sample(s)
  out_pass = as.matrix(cbind(Intercept = 1, is_near_boards = is_near_boards, i = control_A) %*% mat.samples_pass)
  
  # Convert link to response
  out_pass = exp(out_pass)
  
  ##################################
  ## CONTROLLED ENTRY TO TURNOVER ##
  
  # Take n samples from the posterior distribution
  control_2_turnover_sampling = inla.posterior.sample(n = n_samples, result = control_2_turnover)
  
  # Find the rownames that correspond to our model parameters
  x_names = rownames(control_2_turnover_sampling[[1]]$latent) 
  
  idx = lapply(c("Intercept", "is_near_boards", "i"), function(nam)
    which(substr(x_names, 1, nchar(nam))==nam))
  
  # Extract the sample values for each parameter
  mat.samples_turnover = sapply(control_2_turnover_sampling, function(spl)
    c(Intercept = spl$latent[idx[[1]]], is_near_boards = spl$latent[idx[[2]]], i = spl$latent[idx[[3]][1:86]]))
  
  # Output the value of the link function for the sample(s)
  out_turnover = as.matrix(cbind(Intercept = 1, is_near_boards = is_near_boards, i = control_A) %*% mat.samples_turnover)
  
  # Convert link to response
  out_turnover = exp(out_turnover)
  
  ##################
  ## ALL TOGETHER ##
  
  # Transpose the vectors of output frequencies
  out_shot = t(out_shot)
  out_pass = t(out_pass)
  out_turnover = t(out_turnover)
  
  # Create a data frame with probabilities for each transition
  out_probabilities = data.frame(prob_shot = out_shot/(out_shot + out_pass + out_turnover), 
                                 prob_pass = out_pass/(out_shot + out_pass + out_turnover), 
                                 prob_turnover = out_turnover/(out_shot + out_pass + out_turnover))
  
  return(out_probabilities)
  
  
  
}


#################################################################################

# Pass transition probabilities

pass_transition_probabilities <- function(n_samples,x_loc,y_loc, n_passes_prev, pass_2_pass = play_2_play, pass_2_shot = play_2_shot, 
                                          pass_2_turnover = play_2_turnover, mesh = play_2_shot_mesh){
  
  Pass_2_Pass_sampling <- inla.posterior.sample(n=n_samples, result = pass_2_pass)
  
  xnames <- rownames(Pass_2_Pass_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples_pass <- sapply(Pass_2_Pass_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))
  
  group_n <- ifelse(n_passes_prev + 1 >= 4, 4, n_passes_prev + 1)
  
  
  A_pass = inla.spde.make.A(mesh = mesh, 
                            loc = cbind(x_loc,y_loc),
                            group = group_n,
                            n.group = 4)
  
  
  out_pass <- exp(as.matrix(cbind(Intercept=1, w = A_pass)%*%mat.samples_pass))
  
  #########################################################################
  
  Pass_2_Shot_sampling <- inla.posterior.sample(n=n_samples, result = pass_2_shot)
  
  xnames <- rownames(Pass_2_Shot_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples_shot <- sapply(Pass_2_Shot_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))
  
  out_shot <- exp(as.matrix(cbind(Intercept=1, w = A_pass)%*%mat.samples_shot))
  
  ############################################################################
  ############################################################################
  
  Pass_2_Turnover_sampling <- inla.posterior.sample(n=n_samples, result = pass_2_turnover)
  
  xnames <- rownames(Pass_2_Turnover_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples_turnover <- sapply(Pass_2_Turnover_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))
  
  out_turnover <- exp(as.matrix(cbind(Intercept=1, w = A_pass)%*%mat.samples_turnover))
  
  ##################################################################################
  
  out_probabilities <- data.frame(probability_pass = out_pass/(out_pass + out_shot + out_turnover),
                                  probability_turnover = out_turnover/(out_pass+ out_shot +  out_turnover), 
                                  probability_shot = out_shot/(out_pass+ out_shot +  out_turnover))
  
  out_probabilities
  
}


#################################################################################

# Shot transition probabilities

shot_transition_probabilities <- function(n_samples, x_loc, y_loc, time_since_entry, shot_2_recovery = shot_2_recovery, shot_2_turnover = shot_2_turnover,
                                          shot_2_whistle = shot_2_whistle, mesh = shot_2_recovery_mesh){
  
  # Determine which time since entry bin the shot belongs in
  entry_time_group = case_when(
    # 0-2 seconds
    time_since_entry <= 2 ~ 1,
    # 3-5 seconds
    time_since_entry <= 5 ~ 2,
    # 6-10 seconds
    time_since_entry <= 10 ~ 3,
    # 10+ seconds
    TRUE ~ 4
  )
  
  # Calculate the spatial weight matrix at our xy-location and the time since entry
  shot_A = inla.spde.make.A(mesh, loc = cbind(x_loc, y_loc), group = entry_time_group, n.group = 4)

  
  ##########################
  #### SHOT TO RECOVERY ####
  
  # Take n samples from the posterior distribution
  shot_2_recovery_sampling <- inla.posterior.sample(n = n_samples, result = shot_2_recovery)
  
  # Find the rownames that correspond to our model parameters
  xnames <- rownames(shot_2_recovery_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  # Extract the sample values for each parameter
  mat.samples_recovery <- sapply(shot_2_recovery_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))
  
  # Output the value of the link function for the sample(s)
  out_recovery <- as.matrix(cbind(Intercept=1, w = shot_A) %*% mat.samples_recovery)
  
  # Convert link to response
  out_recovery <- exp(out_recovery)
  
  
  ##########################
  #### SHOT TO TURNOVER ####
  
  # Take n samples from the posterior distribution
  shot_2_turnover_sampling <- inla.posterior.sample(n = n_samples, result = shot_2_turnover)
  
  # Find the rownames that correspond to our model parameters
  xnames <- rownames(shot_2_turnover_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  # Extract the sample values for each parameter
  mat.samples_turnover <- sapply(shot_2_turnover_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))
  
  # Output the value of the link function for the sample(s)
  out_turnover <- as.matrix(cbind(Intercept = 1, w = shot_A)%*%mat.samples_turnover)
  
  # Convert link to response
  out_turnover <- exp(out_turnover)
  
  
  ##########################
  #### SHOT TO WHISTLE ####
  
  # Take n samples from the posterior distribution
  shot_2_whistle_sampling <- inla.posterior.sample(n = n_samples, result = shot_2_whistle)
  
  # Find the rownames that correspond to our model parameters
  xnames <- rownames(shot_2_whistle_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  # Extract the sample values for each parameter
  mat.samples_whistle <- sapply(shot_2_whistle_sampling, function(spl)
    c(Intercept = spl$latent[idx[[1]]], w = spl$latent[idx[[2]]]))
  
  # Output the value of the link function for the sample(s)
  out_whistle <- as.matrix(cbind(Intercept = 1, w = shot_A)%*%mat.samples_whistle)
  
  # Convert link to response
  out_whistle <- exp(out_whistle)
  
  ##################
  ## ALL TOGETHER ##
  
  # Transpose the vectors of output frequencies
  out_recovery = t(out_recovery)
  out_turnover = t(out_turnover)
  out_whistle = t(out_whistle)
  
  # Create a data frame with probabilities for each transition
  out_probabilities = data.frame(prob_recovery = out_recovery/(out_recovery + out_whistle + out_turnover), 
                                 prob_turnover = out_turnover/(out_recovery + out_whistle + out_turnover), 
                                 prob_whistle = out_whistle/(out_recovery + out_whistle + out_turnover))
  
  return(out_probabilities)
  
}


#################################################################################

# Recovery transition probabilities

recovery_transition_probabilities <- function(n_samples, x_loc, y_loc, time_since_entry, recovery_2_play = recovery_2_play, 
                                              recovery_2_shot = recovery_2_shot, recovery_2_turnover = recovery_2_turnover,
                                              mesh = recovery_2_shot_mesh){
  
  Recovery_2_Pass_sampling <- inla.posterior.sample(n=n_samples, result = recovery_2_play)
  
  xnames <- rownames(Recovery_2_Pass_sampling[[1]]$latent)
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples_pass <- sapply(Recovery_2_Pass_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))
  
  # Determine which time since entry bin the shot belongs in
  entry_time_group = case_when(
    # 0-2 seconds
    time_since_entry <= 2 ~ 1,
    # 3-5 seconds
    time_since_entry <= 5 ~ 2,
    # 6-10 seconds
    time_since_entry <= 10 ~ 3,
    # 10+ seconds
    TRUE ~ 4
  )
  
  A_recovery = inla.spde.make.A(mesh, loc = cbind(x_loc,y_loc),
                                group = entry_time_group,
                                n.group = 4)
  
  
  
  
  out_pass <- exp(as.matrix(cbind(Intercept=1, w = A_recovery)%*%mat.samples_pass))
  
  #########################################################################
  
  Recovery_2_Shot_sampling <- inla.posterior.sample(n=n_samples, result = recovery_2_shot)
  
  xnames <- rownames(Recovery_2_Shot_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples_shot <- sapply(Recovery_2_Shot_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))
  
  out_shot <- exp(as.matrix(cbind(Intercept=1, w = A_recovery)%*%mat.samples_shot))
  
  ############################################################################
  ############################################################################
  
  Recovery_2_Turnover_sampling <- inla.posterior.sample(n=n_samples, result = recovery_2_turnover)
  
  xnames <- rownames(Recovery_2_Turnover_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples_turnover <- sapply(Recovery_2_Turnover_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))
  
  out_turnover <- exp(as.matrix(cbind(Intercept=1, w = A_recovery)%*%mat.samples_turnover))
  
  ##################################################################################
  
  out_probabilities <- data.frame(probability_pass = out_pass/(out_pass + out_shot + out_turnover),
                                  probability_turnover = out_turnover/(out_pass+ out_shot +  out_turnover), 
                                  probability_shot = out_shot/(out_pass+ out_shot +  out_turnover))
  
  out_probabilities
  
}


#################################################################################

# Turnover transition probabilities

turnover_transition_probabilities <- function(n_samples, x_loc, y_loc, time_since_entry, turnover_2_exit = turnover_2_exit, 
                                              turnover_2_recovery = turnover_2_recovery, mesh = turnover_2_exit_mesh){
  
  Turnover_2_Exit_sampling <- inla.posterior.sample(n=n_samples, result = turnover_2_exit)
  
  xnames <- rownames(Turnover_2_Exit_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples_exit <- sapply(Turnover_2_Exit_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))
  
  group_n <- ifelse(time_since_entry <=2,1,ifelse(time_since_entry <= 5,2,ifelse(time_since_entry <=10,3,4)))
  
  A_turnover = inla.spde.make.A(mesh, loc = cbind(x_loc,y_loc),
                                group = group_n,
                                n.group = 4)
  
  
  out_exit <- exp(as.matrix(cbind(Intercept=1, w = A_turnover)%*%mat.samples_exit))
  
  ####################################
  
  Turnover_2_Recovery_sampling <- inla.posterior.sample(n=n_samples, result = turnover_2_recovery)
  
  xnames <- rownames(Turnover_2_Recovery_sampling[[1]]$latent) 
  
  idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
    which(substr(xnames, 1, nchar(nam))==nam)) ##
  
  mat.samples_recovery <- sapply(Turnover_2_Recovery_sampling, function(spl)
    c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))
  
  
  out_recovery <- exp(as.matrix(cbind(Intercept=1, w = A_turnover)%*%mat.samples_recovery))
  
  ########################
  
  out_probabilities <- data.frame(Probability_Exit = out_exit/(out_exit + out_recovery), 
                                  Probability_Recovery = out_recovery/(out_exit + out_recovery))
  
  out_probabilities
  
}



#################################################################################
#################################################################################
#################################################################################



### EXPECTED GOALS MODELS ###

#################################################################################

# Expected goals model for shots that are preceded by a pass

expected_goals_with_pass <- function(n_samples, shot_x, shot_y, pass_x, pass_y, detail_3, detail_4, time_since_last_pass, time_since_last_shot, 
                                     shot_count, time_since_entry, shot_model = shot_2_goal_with_pass, shot_mesh = shot_2_goal_wp_shot_mesh, 
                                     pass_mesh = shot_2_goal_wp_pass_mesh, time_mesh = shot_2_goal_wp_time_mesh) {
  
  # Generate entry time decay function
  entry_time_decay = case_when(
    # 0 seconds since entry --> 1
    time_since_entry == 0 ~ 1,
    # 1-9 seconds since entry --> 1 - log10(seconds)
    time_since_entry < 10 ~ 1 - log10(time_since_entry),
    # 10+ seconds since entry --> 0
    time_since_entry >= 10 ~ 0,
    TRUE ~ 0
  )
  
  # Generate pass time decay function
  shot_time_decay = case_when(
    # 0 seconds since last shot --> 1
    time_since_last_shot == 0 ~ 1,
    # 1-9 seconds since last shot --> 1 - log10(seconds)
    time_since_last_shot < 10 ~ 1 - log10(time_since_last_shot),
    # 10+ seconds since last shot --> 0
    time_since_last_shot >= 10 ~ 0,
    TRUE ~ 0
  )
  
  # Generate shot time decay function
  pass_time_decay = case_when(
    # 0 seconds since last pass --> 1
    time_since_last_pass == 0 ~ 1,
    # 1-9 seconds since last pass --> 1 - log10(seconds)
    time_since_last_pass < 10 ~ 1 - log10(time_since_last_pass),
    # 10+ seconds since last pass --> 0
    time_since_last_pass >= 10 ~ 0,
    TRUE ~ 0
  )
  
  
  # List required packages
  require(raster, exclude = "select")
  require(LearnGeom)
  
  # Calculate pass distance
  pass_distance = pointDistance(c(shot_x, shot_y), c(pass_x, pass_y), lonlat = FALSE)
  
  # Calculate angle change
  angle = Angle(A = c(shot_x, shot_y), B = c(189, 42.5), C = c(100, 42.5))
  angle = ifelse(shot_y < 42.5, -angle, angle)
  
  pass_angle = Angle(A = c(pass_x, pass_y), B = c(189, 42.5), C = c(100, 42.5))
  pass_angle = ifelse(shot_y < 42.5, -pass_angle, pass_angle)
  
  angle_change = abs(angle - pass_angle)
  
  # Determine if the pass was from behind the net
  from_behind_net = ifelse(pass_y > 189, 1, 0)
  
  # Take n samples from the posterior distribution
  shot_sampling = inla.posterior.sample(n = n_samples, result = shot_model)
  
  # Find the rownames that correspond to our model parameters
  x_names = rownames(shot_sampling[[1]]$latent)
  
  idx = lapply(c("Intercept", "detail_3", "detail_4", "pass_distance", "angle_change", "pass_time_decay",
                 "shot_time_decay", "entry_time_decay", "shot_count", "from_behind_net", "g", "w"), function(nam)
                   which(substr(x_names, 1, nchar(nam)) == nam))
  
  # Extract the sample values for each parameter
  mat.samples = sapply(shot_sampling, function(spl)
    c(Intercept = spl$latent[idx[[1]]], detail_3 = spl$latent[idx[[2]]], detail_4 = spl$latent[idx[[3]]], 
      pass_distance = spl$latent[idx[[4]]], angle_change = spl$latent[idx[[5]]], pass_time_decay = spl$latent[idx[[6]]], 
      shot_time_decay = spl$latent[idx[[7]]], entry_time_decay = spl$latent[idx[[8]]], shot_count = spl$latent[idx[[9]]], 
      from_behind_net = spl$latent[idx[[10]]], g = spl$latent[idx[[11]]], w = spl$latent[idx[[12]]]))
  
  # Calculate the spatial weight matrix for both our shot and pass xy-locations and time since entry
  shot_A = inla.spde.make.A(shot_mesh, loc = cbind(shot_x, shot_y), group = time_since_entry, group.mesh = time_mesh, n.group = 5)
  pass_A = inla.spde.make.A(pass_mesh, loc = cbind(pass_x, pass_y), group = time_since_entry, group.mesh = time_mesh, n.group = 5)
  
  # Output the value of the link function for the sample(s)
  out = as.matrix(cbind(Intercept = 1, detail_3 = detail_3, detail_4 = detail_4, pass_distance = pass_distance, angle_change = angle_change,
                        pass_time_decay = pass_time_decay, shot_time_decay = shot_time_decay, entry_time_decay = entry_time_decay,
                        shot_count = shot_count, from_behind_net = from_behind_net, g = pass_A, w = shot_A) %*% mat.samples)
  
  # Convert from link to response
  out = exp(out) / (1+exp(out))
  
  return(out)
  
}


#################################################################################

# Expected goals model for shots that are NOT preceded by a pass

expected_goals_no_pass <- function(n_samples, shot_x, shot_y, detail_3, time_since_last_shot, shot_count, time_since_entry,
                                   shot_model = shot_2_goal_no_pass, shot_mesh = shot_2_goal_np_shot_mesh, time_mesh = shot_2_goal_np_time_mesh) {
  
  # Generate pass time decay function
  shot_time_decay = case_when(
    # 0 seconds since last shot --> 1
    time_since_last_shot == 0 ~ 1,
    # 1-9 seconds since last shot --> 1 - log10(seconds)
    time_since_last_shot < 10 ~ 1 - log10(time_since_last_shot),
    # 10+ seconds since last shot --> 0
    time_since_last_shot >= 10 ~ 0,
    TRUE ~ 0
  )
  
  # Take n samples from the posterior distribution
  shot_sampling = inla.posterior.sample(n = n_samples, result = shot_model)
  
  # Find the rownames that correspond to our model parameters
  x_names = rownames(shot_sampling[[1]]$latent)
  
  idx = lapply(c("Intercept", "detail_3", "shot_time_decay", "shot_count", "w"), 
               function(nam) which(substr(x_names, 1, nchar(nam)) == nam))
  
  # Extract the sample values for each parameter
  mat.samples = sapply(shot_sampling, function(spl)
    c(Intercept = spl$latent[idx[[1]]], detail_3 = spl$latent[idx[[2]]], shot_time_decay = spl$latent[idx[[3]]], 
      shot_count = spl$latent[idx[[4]]], w = spl$latent[idx[[5]]]))
  
  # Calculate the spatial weight matrix at our xy-location and time since entry
  shot_A = inla.spde.make.A(shot_mesh, loc = cbind(shot_x, shot_y), group = time_since_entry, group.mesh = time_mesh, n.group = 5)
  
  # Output the value of the link function for the sample(s)
  out = as.matrix(cbind(Intercept = 1, detail_3 = detail_3, shot_time_decay = shot_time_decay, 
                        shot_count = shot_count, w = shot_A) %*% mat.samples)
  
  # Convert from link to response
  out = exp(out) / (1+exp(out))
  
  out
}
