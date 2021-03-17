
### LOAD PACKAGES ###

#################################################################################
library(INLA)
library(gtools)
#################################################################################


### MOVEMENT AND TIME MODEL SAMPLING ###

#################################################################################

# Predict X Location

gen_samp_x_location <- function(n_samples, x_loc, y_loc, prev_transition, transition, time_since_entry, 
                                pass_type = "none", samples = x_prediction_parameters, mesh = xy_mesh){
  
  prev_behind_net = ifelse(x_loc >= 189, 1, 0)
  
  transition = case_when(
    transition == "Pass" & pass_type == "Direct" ~ "Direct",
    transition == "Pass" & pass_type == "Indirect" ~ "Indirect",
    transition == "Pass" ~ "Direct",
    TRUE ~ transition
  )
  
  direct_pass = 0.68
  
  prev_shot = ifelse(prev_transition == "Shot", 1, 0)
  prev_recovery = ifelse(prev_transition == "Recovery", 1, 0)
  prev_zone_entry = ifelse(prev_transition == "Zone Entry", 1, 0)
  
  time_int_shot = ifelse(transition == "Shot", time_since_entry, 0)
  
  NGroups <- 5
  
  group_mat <- data.frame(Transition = c("Turnover","Recovery", "Shot", "Direct", "Indirect"), Number = c(1:5))
  
  group_n <- group_mat[which(group_mat$Transition == transition),2]
  
  HostA_x_pred <- inla.spde.make.A(mesh = mesh, 
                                   loc = cbind(x_loc,y_loc), 
                                   group = group_n,
                                   n.group = NGroups) 
  
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out <- as.matrix(cbind(Intercept=1, direct_pass = direct_pass, prev_behind_net = prev_behind_net, prev_shot,
                         prev_recovery = prev_recovery, prev_zone_entry = prev_zone_entry,
                         time_since_entry = time_since_entry, w = HostA_x_pred) %*% samples[,col.num])
  
  out
  
}


#################################################################################

# Predict Y Location

gen_samp_y_location <- function(n_samples, x_prev, y_prev, x_loc, prev_transition, transition, time_since_entry,
                                pass_type = "none", samples = y_prediction_parameters, mesh = xy_mesh){
  
  prev_behind_net = ifelse(x_prev >= 189, 1, 0)
  
  transition = case_when(
    transition == "Pass" & pass_type == "Direct" ~ "Direct",
    transition == "Pass" & pass_type == "Indirect" ~ "Indirect",
    transition == "Pass" ~ "Direct",
    TRUE ~ transition
  )
  
  direct_pass = 0.68
  
  prev_shot = ifelse(prev_transition == "Shot", 1, 0)
  prev_recovery = ifelse(prev_transition == "Recovery", 1, 0)
  prev_turnover = ifelse(prev_transition == "Turnover", 1, 0)
  prev_zone_entry = ifelse(prev_transition == "Zone Entry", 1, 0)
  
  time_int_shot = ifelse(transition == "Shot", time_since_entry, 0)
  
  prev_left_side = ifelse(y_prev > 42.5, 1, 0)
  
  x_normalized = (x_loc - 156.5)/27.4
  
  x_normalized_int_right = x_normalized * (1 - prev_left_side)
  
  NGroups <- 5
  
  group_mat <- data.frame(Transition = c("Turnover","Recovery","Shot", "Direct", "Indirect"), Number = c(1:5))
  
  group_n <- group_mat[which(group_mat$Transition == transition),2]
  
  HostA_y_pred <- inla.spde.make.A(mesh = mesh, # Leave
                                   loc = cbind(x_prev, y_prev), # Leave
                                   group = group_n,# this must be a numeric value counting from 1. If the groups variable is a factor, this will happen by default.
                                   n.group = NGroups) 
  
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out <- as.matrix(cbind(Intercept=1, prev_behind_net = prev_behind_net, prev_recovery = prev_recovery,
                         prev_shot = prev_shot, prev_zone_entry = prev_zone_entry, time_since_entry = time_since_entry,
                         x_normalized = x_normalized, x_normalized_int_right = x_normalized_int_right, w = HostA_y_pred) %*% samples[,col.num])
  
  out
  
}


#################################################################################

# Predict elapsed time

gen_samp_time <- function(n_samples, x_loc, y_loc, transition, prev_x_loc, prev_y_loc, prev_transition, 
                          samples = time_to_event_parameters, mesh = time_mesh){
  
  NGroups <- 4
  
  Mesh_time <- mesh
  
  group_mat <- data.frame(Transition = c("Turnover","Recovery", "Shot", "Pass"), Number = c(1:4))
  
  group_n <- group_mat[which(group_mat$Transition == transition),2]
  
  Host_time <- inla.spde.make.A(Mesh_time, 
                                loc = cbind(x_loc,y_loc), 
                                group = group_n,
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
  
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out <- as.matrix(cbind(Intercept = 1, distance_last_event = distance_last_event, prev_direct_pass = prev_direct_pass,
                         prev_indirect_pass = prev_indirect_pass, prev_turnover = prev_turnover,
                         prev_shot = prev_shot, int_direct_dist = int_direct_dist, int_indirect_dist = int_indirect_dist,
                         int_turnover_dist = int_turnover_dist, int_recovery_dist = int_recovery_dist,
                         int_shot_dist = int_shot_dist, w = Host_time) %*% samples[,col.num])
  
  exp(out)
  
}


#################################################################################

# Predict direct/indirect passes

gen_samp_direct_pass <- function(n_samples, x_loc, y_loc, prev_event, score_state, 
                                 samples = direct_pass_parameters, mesh = direct_mesh){
  
  prev_pass <-   ifelse(prev_event %in% c("Pass", "Failed Pass"),1,0)
  
  prev_recovery <- ifelse(prev_event == "Recovery",1,0)
  
  is_lead <- ifelse(score_state == "Lead",1,0)
  
  is_deficit <- ifelse(score_state =="Deficit",1,0)
  
  HostA_pass <- inla.spde.make.A(direct_pass_mesh, 
                                 loc = cbind(x_loc,y_loc)) 
  
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out <- as.matrix(cbind(Intercept=1,prev_pass = prev_pass, prev_recovery = prev_recovery, is_lead = is_lead, 
                         is_deficit = is_deficit, w = HostA_pass) %*% samples[,col.num])
  
  exp(out)/(1 + exp(out))
}


#################################################################################


# Predict pass target xy-location

gen_samp_pass_target <- function(n_samples, x_loc, y_loc, pass_type, samples_x = pass_x_target_parameters, 
                                 samples_y = pass_y_target_parameters, mesh = pass_xy_mesh){
  
  NGroups <- 2
  
  group_mat <- data.frame(Pass_type = c("Direct", "Indirect"), Number = c(1:2))
  
  group_n <- group_mat[which(group_mat$Pass_type == pass_type),2]
  
  HostA_x_targ <- inla.spde.make.A(pass_target_mesh, 
                                   loc = cbind(x_loc,y_loc), 
                                   group = group_n,
                                   n.group = NGroups) 
  
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_x <- as.matrix(cbind(Intercept=1, w = HostA_x_targ) %*% samples_x[,col.num])
  
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_y <- as.matrix(cbind(Intercept=1, w = HostA_x_targ) %*% samples_y[,col.num])
  
  list(out_x,out_y)
  
}



#################################################################################

# Predict entry type decision

gen_samp_pass_completion = function(n_samples, x_loc, y_loc, target_x, target_y, time_since_last_pass, is_direct = is_direct,
                                    pass_count = n_passes_prev, samples = pass_completion_parameters, start_mesh = pass_completion_start_mesh,
                                    end_mesh = pass_completion_end_mesh) {
  
  require(LearnGeom)
  require(raster, exclude = "select")
  
  # Generate pass time decay function
  pass_time_decay = case_when(
    # 0 seconds since last pass --> 1
    time_since_last_pass == 0 ~ 1,
    # 1-9 seconds since last pass --> 1 - log10(seconds)
    time_since_last_pass < 10 ~ 1 - log10(time_since_last_pass),
    # 10+ seconds since last pass --> 0
    time_since_last_pass >= 10 ~ 0,
    TRUE ~ 0
  )
  
  # Calculate pass distance
  pass_distance = pointDistance(p1 = c(x_loc, y_loc), p2 = c(target_x, target_y), lonlat =  FALSE)
  
  # Calculate angle
  if (!((x_loc == target_x) & (y_loc == target_y))) {
    angle = Angle(A = c(x_loc, 95), B = c(x_loc, y_loc), C = c(target_x, target_y))
  } else {angle = 0}
  
  angle = case_when(
    (x_loc > target_x) & (y_loc > target_y) ~ abs(angle - 180),
    (x_loc > target_x) & (y_loc <= target_y) ~ abs(-angle),
    (x_loc <= target_x) & (y_loc > target_y) ~ abs(180 - angle),
    (x_loc <= target_x) & (y_loc <= target_y) ~ abs(angle),
  )
  
  # Determine if the pass was forwards or backwards
  is_forwards = ifelse(x_loc <= target_x, 1, 0)
  is_backwards = 1 - is_forwards
  
  # Determine if the pass was from behind the net
  from_behind_net = ifelse(x_loc >= 189, 1, 0)
  
  # Convert is_direct from binary to INLA group
  is_direct = ifelse(is_direct == 0, 2, 1)
  
  # Calculate the spatial weight matrices
  start_A = inla.spde.make.A(start_mesh, loc = cbind(x_loc, y_loc), group = is_direct, n.group = 2)
  end_A = inla.spde.make.A(end_mesh, loc = cbind(target_x, target_y), group = is_direct, n.group = 2)
     
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  # Output the value of the link function for the sample(s)
  out = as.matrix(cbind(Intercept = 1, pass_time_decay = pass_time_decay, pass_distance = pass_distance,
                        `angle:is_forwards` = angle*is_forwards, `angle:is_backwards` = angle*is_backwards,
                        pass_count = pass_count, from_behind_net = from_behind_net, w = start_A, g = end_A) %*% samples[,col.num])
  
  # Convert from link to response
  out = exp(out) / (1 + exp(out))
  
  return(out)
  
}



#################################################################################

# Predict entry type decision

gen_samp_entry_decision = function(n_samples, y_loc, offensive_score_diff, period, samples = entry_decision_parameters) {
  
  # Create a 1D inla mesh with knots from 0-85
  Mesh = inla.mesh.1d(0:85)
  
  # Calculate the spatial weight matrix at our y-location
  shot_A = inla.mesh.1d.A(Mesh, loc = y_loc)
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  # Output the value of the link function for the sample(s)
  out = as.matrix(cbind(Intercept = 1, period = period, 
                        `offensive_score_diff:period` = offensive_score_diff*period, i = shot_A) %*% samples[,col.num])
  
  # Convert from link to response
  out_controlled = exp(out) / (1 + exp(out))
  
  return(out_controlled)
  
}


#################################################################################

# Predict if a shot will be a one-timer

gen_samp_one_timer <- function(n_samples, shot_x, shot_y, pass_x, pass_y, detail_3, time_since_entry, time_since_last_pass, 
                               samples = one_timer_parameters, shot_mesh = one_timer_shot_mesh, pass_mesh = one_timer_pass_mesh, 
                               time_mesh = one_timer_time_mesh, time_mesh_random = one_timer_time_mesh_random) {
  
  # Calculate the spatial weight matrix for both our shot and pass xy-locations and time since entry
  shot_A = inla.spde.make.A(shot_mesh, loc = cbind(shot_x, shot_y), group = time_since_entry, group.mesh = time_mesh_random, n.group = 5)
  pass_A = inla.spde.make.A(pass_mesh, loc = cbind(pass_x, pass_y), group = time_since_last_pass, group.mesh = time_mesh, n.group = 5)
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  # Output the value of the link function for the sample(s)
  out = as.matrix(cbind(Intercept = 1, detail_3 = detail_3, g = pass_A, w = shot_A) %*% samples[,col.num])
  
  # Convert from link to response
  out = exp(out) / (1+exp(out))
  
  return(out)
  
}


#################################################################################

# Predict if there is traffic on a shot that is preceded by a pass

gen_samp_traffic_with_pass <- function(n_samples, shot_x, shot_y, pass_x, pass_y, time_since_entry, samples = traffic_wp_parameters, 
                                       shot_mesh = traffic_wp_shot_mesh, pass_mesh = traffic_wp_pass_mesh, time_mesh = traffic_wp_time_mesh) {
  
  # Calculate the spatial weight matrix for both our shot and pass xy-locations and time since entry
  shot_A = inla.spde.make.A(shot_mesh, loc = cbind(shot_x, shot_y), group = time_since_entry, group.mesh = time_mesh, n.group = 5)
  pass_A = inla.spde.make.A(pass_mesh, loc = cbind(pass_x, pass_y), group = time_since_entry, group.mesh = time_mesh, n.group = 5)
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  # Output the value of the link function for the sample(s)
  out = as.matrix(cbind(Intercept = 1, g = pass_A, w = shot_A) %*% samples[,col.num])
  
  # Convert from link to response
  out = exp(out) / (1+exp(out))
  
  return(out)
  
}


#################################################################################

# Predict if there is traffic on a shot that is NOT preceded by a pass

gen_samp_traffic_no_pass <- function(n_samples, shot_x, shot_y, time_since_entry, samples = traffic_np_parameters, 
                                     shot_mesh = traffic_np_shot_mesh, time_mesh = traffic_np_time_mesh) {
  
  # Calculate the spatial weight matrix for both our shot and pass xy-locations and time since entry
  shot_A = inla.spde.make.A(shot_mesh, loc = cbind(shot_x, shot_y), group = time_since_entry, group.mesh = time_mesh, n.group = 5)
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  # Output the value of the link function for the sample(s)
  out = as.matrix(cbind(Intercept = 1, w = shot_A) %*% samples[,col.num])
  
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

dump_transition_probabilites <- function(n_samples, x_loc, y_loc, score_state, samples_recovery = dumped_2_recovery_parameters, 
                                         samples_turnover = dumped_2_turnover_parameters, dump_mesh = dump_mesh){
  
  group_n <- ifelse(score_state =="Deficit",1,ifelse(score_state == "Close",2,3))
  
  A_dump_recovery = inla.spde.make.A(dump_mesh, loc = cbind(x_loc,y_loc),
                                     group = group_n,
                                     n.group = 3)
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_recovery <- exp(as.matrix(cbind(Intercept = 1, w = A_dump_recovery) %*% samples_recovery[,col.num]))
  
  
  #################################
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_turnover <- exp(as.matrix(cbind(Intercept=1, w = A_dump_recovery) %*% samples_turnover[,col.num]))
  
  out_probabilities <- data.frame(probability_recover = out_recovery/(out_recovery + out_turnover), probability_turnover = out_turnover/(out_recovery + out_turnover))
  
  out_probabilities
  
}


#################################################################################

# Controlled entry transition probabilities

controlled_transition_probabilities = function(n_samples, y_loc, samples_shot = controlled_2_shot_parameters, 
                                               samples_pass = controlled_2_pass_parameters, 
                                               samples_turnover = controlled_2_turnover_parameters) {
  
  # Determine if the entry was near the boards
  is_near_boards = ifelse(y_loc < 10 | y_loc > 75, 1, 0)
  
  # Calculate the mesh (same for all transitions)
  Mesh = inla.mesh.1d(0:85)
  
  # Calculate the spatial weight matrix at our xy-location (same for all transitions)
  control_A = inla.mesh.1d.A(Mesh, loc = y_loc)
  
  ##############################
  ## CONTROLLED ENTRY TO SHOT ##
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  # Output the value of the link function for the sample(s)
  out_shot = as.matrix(cbind(Intercept = 1, is_near_boards = is_near_boards, i = control_A) %*% samples_shot[,col.num])
  
  # Convert link to response
  out_shot = exp(out_shot)
  
  ##############################
  ## CONTROLLED ENTRY TO PASS ##
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  # Output the value of the link function for the sample(s)
  out_pass = as.matrix(cbind(Intercept = 1, is_near_boards = is_near_boards, i = control_A) %*% samples_pass[,col.num])
  
  # Convert link to response
  out_pass = exp(out_pass)
  
  ##################################
  ## CONTROLLED ENTRY TO TURNOVER ##
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  # Output the value of the link function for the sample(s)
  out_turnover = as.matrix(cbind(Intercept = 1, is_near_boards = is_near_boards, i = control_A) %*% samples_turnover[,col.num])
  
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

pass_transition_probabilities <- function(n_samples,x_loc,y_loc, time_since_entry, samples_pass = pass_2_pass_parameters, 
                                          samples_shot = pass_2_shot_parameters, samples_turnover = pass_2_turnover_parameters, 
                                          mesh = rtp_mesh){
  
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
  
  A_pass = inla.spde.make.A(mesh = mesh, 
                            loc = cbind(x_loc,y_loc),
                            group = entry_time_group,
                            n.group = 4)
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_pass <- exp(as.matrix(cbind(Intercept=1, w = A_pass)%*% samples_pass[,col.num]))
  
  #########################################################################
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_shot <- exp(as.matrix(cbind(Intercept=1, w = A_pass)%*% samples_shot[,col.num]))
  
  ############################################################################
  ############################################################################
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_turnover <- exp(as.matrix(cbind(Intercept=1, w = A_pass)%*% samples_turnover[,col.num]))
  
  ##################################################################################
  
  out_probabilities <- data.frame(probability_pass = out_pass/(out_pass + out_shot + out_turnover),
                                  probability_turnover = out_turnover/(out_pass+ out_shot +  out_turnover), 
                                  probability_shot = out_shot/(out_pass+ out_shot +  out_turnover))
  
  out_probabilities
  
}



#################################################################################

# Failed Pass transition probabilities

failed_pass_transition_probabilities <- function(n_samples,x_loc,y_loc, time_since_entry, samples_recovery = fpass_2_recovery_parameters, 
                                                 samples_turnover = fpass_2_turnover_parameters, mesh = rtp_mesh){
  
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
  
  A_pass = inla.spde.make.A(mesh = mesh, 
                            loc = cbind(x_loc,y_loc),
                            group = entry_time_group,
                            n.group = 4)
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_recovery <- exp(as.matrix(cbind(Intercept=1, w = A_pass)%*% samples_recovery[,col.num]))
  
  ############################################################################
  ############################################################################
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_turnover <- exp(as.matrix(cbind(Intercept=1, w = A_pass)%*% samples_turnover[,col.num]))
  
  ##################################################################################
  
  out_probabilities <- data.frame(probability_recovery = out_recovery/(out_recovery + out_turnover),
                                  probability_turnover = out_turnover/(out_recovery + out_turnover))
  
  out_probabilities
  
}



#################################################################################

# Turnover transition probabilities

failed_pass_transition_probabilities <- function(n_samples, x_loc, y_loc, time_since_entry, samples_turnover = failed_pass_2_turnover_parameters, 
                                                 samples_recovery = failed_pass_2_recovery_parameters, mesh = rtp_mesh){
  
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
  
  fpass_A = inla.spde.make.A(mesh, loc = cbind(x_loc,y_loc),
                                group = entry_time_group,
                                n.group = 4)
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_recovery <- exp(as.matrix(cbind(Intercept=1, w = fpass_A)%*% samples_recovery[,col.num]))
  
  ####################################
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_turnover <- exp(as.matrix(cbind(Intercept=1, w = fpass_A)%*% samples_turnover[,col.num]))
  
  ########################
  
  out_probabilities <- data.frame(Probability_Recovery = out_recovery/(out_turnover + out_recovery),
                                  Probability_Turnover = out_turnover/(out_turnover + out_recovery))
  
  out_probabilities
  
}



#################################################################################

# Shot transition probabilities

shot_transition_probabilities <- function(n_samples, x_loc, y_loc, time_since_entry, samples_recovery = shot_2_recovery_parameters, 
                                          samples_turnover = shot_2_turnover_parameters, samples_whistle = shot_2_whistle_parameters, 
                                          mesh = shot_trans_mesh){
  
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
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  # Output the value of the link function for the sample(s)
  out_recovery <- as.matrix(cbind(Intercept=1, w = shot_A) %*% samples_recovery[,col.num])
  
  # Convert link to response
  out_recovery <- exp(out_recovery)
  
  
  ##########################
  #### SHOT TO TURNOVER ####
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  # Output the value of the link function for the sample(s)
  out_turnover <- as.matrix(cbind(Intercept = 1, w = shot_A)%*% samples_turnover[,col.num])
  
  # Convert link to response
  out_turnover <- exp(out_turnover)
  
  
  ##########################
  #### SHOT TO WHISTLE ####
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  # Output the value of the link function for the sample(s)
  out_whistle <- as.matrix(cbind(Intercept = 1, w = shot_A)%*% samples_whistle[,col.num])
  
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

recovery_transition_probabilities <- function(n_samples, x_loc, y_loc, time_since_entry, samples_pass = recovery_2_pass_parameters, 
                                              samples_shot = recovery_2_shot_parameters, samples_turnover = recovery_2_turnover_parameters,
                                              mesh = rtp_mesh){
  
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
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_pass <- exp(as.matrix(cbind(Intercept=1, w = A_recovery)%*% samples_pass[,col.num]))
  
  #########################################################################
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_shot <- exp(as.matrix(cbind(Intercept=1, w = A_recovery)%*% samples_shot[,col.num]))
  
  ############################################################################
  ############################################################################
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_turnover <- exp(as.matrix(cbind(Intercept=1, w = A_recovery)%*% samples_turnover[,col.num]))
  
  ##################################################################################
  
  out_probabilities <- data.frame(probability_pass = out_pass/(out_pass + out_shot + out_turnover),
                                  probability_turnover = out_turnover/(out_pass+ out_shot +  out_turnover), 
                                  probability_shot = out_shot/(out_pass+ out_shot +  out_turnover))
  
  out_probabilities
  
}


#################################################################################

# Turnover transition probabilities

turnover_transition_probabilities <- function(n_samples, x_loc, y_loc, time_since_entry, samples_exit = turnover_2_exit_parameters, 
                                              samples_recovery = turnover_2_recovery_parameters, mesh = rtp_mesh){
  
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
  
  A_turnover = inla.spde.make.A(mesh, loc = cbind(x_loc,y_loc), group = entry_time_group, n.group = 4)
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_exit <- exp(as.matrix(cbind(Intercept=1, w = A_turnover)%*% samples_exit[,col.num]))
  
  ####################################
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  out_recovery <- exp(as.matrix(cbind(Intercept=1, w = A_turnover)%*% samples_recovery[,col.num]))
  
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
                                     shot_count, time_since_entry, samples = shot_2_goal_wp_parameters, shot_mesh = shot_2_goal_wp_shot_mesh, 
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
  
  # Generate shot time decay function
  shot_time_decay = case_when(
    # 0 seconds since last shot --> 1
    time_since_last_shot == 0 ~ 1,
    # 1-9 seconds since last shot --> 1 - log10(seconds)
    time_since_last_shot < 10 ~ 1 - log10(time_since_last_shot),
    # 10+ seconds since last shot --> 0
    time_since_last_shot >= 10 ~ 0,
    TRUE ~ 0
  )
  
  # Generate pass time decay function
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
  
  # Calculate the spatial weight matrix for both our shot and pass xy-locations and time since entry
  shot_A = inla.spde.make.A(shot_mesh, loc = cbind(shot_x, shot_y), group = time_since_entry, group.mesh = time_mesh, n.group = 5)
  pass_A = inla.spde.make.A(pass_mesh, loc = cbind(pass_x, pass_y), group = time_since_entry, group.mesh = time_mesh, n.group = 5)
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  # Output the value of the link function for the sample(s)
  out = as.matrix(cbind(Intercept = 1, detail_3 = detail_3, detail_4 = detail_4, pass_distance = pass_distance, angle_change = angle_change,
                        pass_time_decay = pass_time_decay, shot_time_decay = shot_time_decay, 
                        from_behind_net = from_behind_net, g = pass_A, w = shot_A) %*% samples[,col.num])
  
  # Convert from link to response
  out = exp(out) / (1+exp(out))
  
  return(out)
  
}


#################################################################################

# Expected goals model for shots that are NOT preceded by a pass

expected_goals_no_pass <- function(n_samples, shot_x, shot_y, detail_3, time_since_last_shot, shot_count, time_since_entry,
                                   samples = shot_2_goal_np_parameters, shot_mesh = shot_2_goal_np_shot_mesh, time_mesh = shot_2_goal_np_time_mesh) {
  
  # Generate shot time decay function
  shot_time_decay = case_when(
    # 0 seconds since last shot --> 1
    time_since_last_shot == 0 ~ 1,
    # 1-9 seconds since last shot --> 1 - log10(seconds)
    time_since_last_shot < 10 ~ 1 - log10(time_since_last_shot),
    # 10+ seconds since last shot --> 0
    time_since_last_shot >= 10 ~ 0,
    TRUE ~ 0
  )
  
  # Calculate the spatial weight matrix at our xy-location and time since entry
  shot_A = inla.spde.make.A(shot_mesh, loc = cbind(shot_x, shot_y), group = time_since_entry, group.mesh = time_mesh, n.group = 5)
  
  # Weight samples via dirichlet distribution
  wgt = rdirichlet(1, rep(1/1000,1000))
  
  # Take a sample
  col.num = sample.int(n = 1000, size = n_samples, prob = wgt, replace = TRUE)
  
  # Output the value of the link function for the sample(s)
  out = as.matrix(cbind(Intercept = 1, detail_3 = detail_3, shot_time_decay = shot_time_decay, 
                        w = shot_A) %*% samples[,col.num])
  
  # Convert from link to response
  out = exp(out) / (1+exp(out))
  
  out
}

