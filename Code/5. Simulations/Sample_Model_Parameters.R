
#####################
#### LOAD MODELS ####
#####################

setwd(here::here())

## CONTROLLED ENTRIES ##
controlled_2_shot = readRDS("Models/Entry/Controlled_Entry_to_Shot_ES_model.Rda")
controlled_2_pass = readRDS("Models/Entry/Controlled_Entry_to_Pass_ES_model.Rda")
controlled_2_turnover = readRDS("Models/Entry/Controlled_Entry_to_Turnover_ES_model.Rda")

## DUMP-IN ENTRIES ##
dumped_2_recovery = readRDS("Models/Entry/Dump_to_Recovery_ES_model.Rda")
dumped_2_turnover = readRDS("Models/Entry/Dump_to_Turnover_ES_model.Rda")

## PASSES ##
pass_2_pass = readRDS("Models/Pass/Pass_to_Pass_ES_model.Rda")
pass_2_shot = readRDS("Models/Pass/Pass_to_Shot_ES_model.Rda")
pass_2_turnover = readRDS("Models/Pass/Pass_to_Turnover_ES_model.Rda")
fpass_2_recovery = readRDS("Models/Failed Pass/Failed_Pass_to_Recovery_ES_model.Rda")
fpass_2_turnover = readRDS("Models/Failed Pass/Failed_Pass_to_Turnover_ES_model.Rda")

## SHOTS ##
shot_2_recovery = readRDS("Models/Shot/Shot_to_Recovery_ES_model.Rda")
shot_2_turnover = readRDS("Models/Shot/Shot_to_Turnover_ES_model.Rda")
shot_2_whistle = readRDS("Models/Shot/Shot_to_Whistle_ES_model.Rda")
shot_2_goal_with_pass = readRDS("Models/Shot/Shot_to_Goal_ES_with_pass_model.Rda")
shot_2_goal_no_pass = readRDS("Models/Shot/Shot_to_Goal_ES_no_pass_model.Rda")

## RECOVERIES ##
recovery_2_pass = readRDS("Models/Recovery/Recovery_to_Pass_ES_model.Rda")
recovery_2_shot = readRDS("Models/Recovery/Recovery_to_Shot_ES_model.Rda")
recovery_2_turnover = readRDS("Models/Recovery/Recovery_to_Turnover_ES_model.Rda")

## TURNOVERS ##
turnover_2_recovery = readRDS("Models/Turnover/Turnover_to_Recovery_ES_model.Rda")
turnover_2_exit = readRDS("Models/Turnover/Turnover_to_Exit_ES_model.Rda")

## OTHER ##
pass_completion = readRDS("Models/Pass/Pass_Completion_ES_model.Rda")
direct_pass = readRDS("Models/Pass/Direct_Pass_ES_model.Rda")
entry_decision = readRDS("Models/Entry/Entry_Decision_ES_model.Rda")
x_prediction = readRDS("Models/Other/X_Location_ES_model.Rda")
y_prediction = readRDS("Models/Other/Y_Location_ES_model.Rda")
time_to_event = readRDS("Models/Other/Time_to_Event_ES_model.Rda")
pass_x_target = readRDS("Models/Other/Pass_X_Location_ES_model.Rda")
pass_y_target = readRDS("Models/Other/Pass_Y_Location_ES_model.Rda")
traffic_with_pass = readRDS("Models/Other/Traffic_with_pass_ES_model.Rda")
traffic_no_pass = readRDS("Models/Other/Traffic_no_pass_ES_model.Rda")
one_timer = readRDS("Models/Other/One_Timer_ES_model.Rda")





##############################################
#### GET SAMPLE PARAMETERS FOR ALL MODELS ####
##############################################

# Set number of samples to take from each model
n_samples = 1000



################################################################################

# Predict X Location

##########
# Take 1000 posterior samples from model
X_location_sampling <- inla.posterior.sample(n = n_samples, result = x_prediction)

# Find the rownames that correspond to our model parameters
xnames <- rownames(X_location_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', "direct_pass", "prev_behind_net", "prev_play", "prev_shot",
                "prev_recovery", "prev_zone_entry", "time_since_entry", 
                'w'), function(nam) ## for each effect
                  which(substr(xnames, 1, nchar(nam))==nam))

# Extract the sample values for each parameter
mat.samples <- sapply(X_location_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]], direct_pass = spl$latent[idx[[2]]], prev_behind_net = spl$latent[idx[[3]]], 
    prev_play = spl$latent[idx[[4]]], prev_shot = spl$latent[idx[[5]]],  prev_recovery = spl$latent[idx[[6]]], 
    prev_zone_entry = spl$latent[idx[[7]]], time_since_entry = spl$latent[idx[[8]]], 
    w = spl$latent[idx[[9]]]))

# Save for future use
saveRDS(mat.samples, "Sample Parameters/X_Location_Parameter_Samples.Rda")
##########



################################################################################

# Predict Y Location

##########
Y_location_sampling <- inla.posterior.sample(n=n_samples, result = y_prediction)

xnames <- rownames(Y_location_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', "prev_behind_net", "prev_play", "prev_recovery", "prev_shot", "prev_zone_entry", "time_zone_entry",
                "x_normalized", "x_normalized_int_right", 'w'), function(nam) ## for each effect
                  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples <- sapply(Y_location_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]], prev_behind_net=spl$latent[idx[[2]]], prev_play=spl$latent[idx[[3]]], prev_recovery=spl$latent[idx[[4]]],
    prev_shot=spl$latent[idx[[5]]], prev_zone_entry=spl$latent[idx[[6]]], time_since_entry=spl$latent[idx[[7]]], x_normalized=spl$latent[idx[[8]]], 
    x_normalized_int_right=spl$latent[idx[[9]]], w=spl$latent[idx[[10]]]))

saveRDS(mat.samples, "Sample Parameters/Y_Location_Parameter_Samples.Rda")
##########



################################################################################

# Predict Time Elapsed

##########
Time_sampling <- inla.posterior.sample(n=n_samples, result = time_to_event)

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

saveRDS(mat.samples, "Sample Parameters/Time_to_Event_Parameter_Samples.Rda")
##########



################################################################################

# Predict Direct vs Indirect Pass

##########
direct_sampling <- inla.posterior.sample(n=n_samples, result = direct_pass)

xnames <- rownames(direct_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'prev_pass','prev_recovery', 'is_lead','is_deficit', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples <- sapply(direct_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]], prev_pass = spl$latent[idx[[2]]], prev_recovery = spl$latent[idx[[3]]],
    is_lead = spl$latent[idx[[4]]], is_deficit = spl$latent[idx[[5]]], w = spl$latent[idx[[6]]]))

saveRDS(mat.samples, "Sample Parameters/Direct_Pass_Parameter_Samples.Rda")
##########



################################################################################

# Predict Pass Completion

##########
pass_completion_sampling <- inla.posterior.sample(n = n_samples, result = pass_completion)

xnames <- rownames(pass_completion_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'pass_time_decay', 'pass_distance', 'angle:is_forwards','angle:is_backwards',
                'pass_count', 'from_behind_net', 'w', 'g'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples <- sapply(pass_completion_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]], pass_time_decay = spl$latent[idx[[2]]], pass_distance = spl$latent[idx[[3]]],
    forward_angle = spl$latent[idx[[4]]], backward_angle = spl$latent[idx[[5]]], pass_count = spl$latent[idx[[6]]],
    from_behind_net = spl$latent[idx[[7]]],  w = spl$latent[idx[[8]]],  g = spl$latent[idx[[9]]]))

saveRDS(mat.samples, "Sample Parameters/Pass_Completion_Parameter_Samples.Rda")
##########



################################################################################

# Predict Pass Target XY-Location

##########
X_Pass_target_sampling <- inla.posterior.sample(n=n_samples, result = pass_x_target)

xnames <- rownames(X_Pass_target_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_x <- sapply(X_Pass_target_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

saveRDS(mat.samples_x, "Sample Parameters/Pass_X_Location_Parameter_Samples.Rda")

Y_Pass_target_sampling <- inla.posterior.sample(n=n_samples, result = pass_y_target)

xnames <- rownames(Y_Pass_target_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_y <- sapply(Y_Pass_target_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

saveRDS(mat.samples_y, "Sample Parameters/Pass_Y_Location_Parameter_Samples.Rda")
##########



################################################################################

# Predict Entry Decision (Controlled/Dumped)

##########
# Take n samples from the posterior distribution
entry_sampling = inla.posterior.sample(n = n_samples, result = entry_decision)

# Find the rownames that correspond to our model parameters
x_names = rownames(entry_sampling[[1]]$latent)

idx = lapply(c("Intercept", "offensive_score_diff", "period", "period:offensive_score_diff", "i"), function(nam)
  which(substr(x_names, 1, nchar(nam)) == nam))

# Extract the sample values for each parameter
mat.samples = sapply(entry_sampling, function(spl)
  c(Intercept = spl$latent[idx[[1]]], offensive_score_diff = spl$latent[idx[[2]]], 
    period = spl$latent[idx[[3]][[1]]], period_score_effect = spl$latent[idx[[4]]], i = spl$latent[idx[[5]]]))

# Save sample parameters
saveRDS(mat.samples, "Sample Parameters/Entry_Decision_Parameter_Samples.Rda")
##########



################################################################################

# Predict One-Timer

##########
# Take n samples from the posterior distribution
one_timer_sampling = inla.posterior.sample(n = n_samples, result = one_timer)

# Find the rownames that correspond to our model parameters
x_names = rownames(one_timer_sampling[[1]]$latent)

idx = lapply(c("Intercept", "detail_3", "g", "w"), function(nam)
  which(substr(x_names, 1, nchar(nam)) == nam))

# Extract the sample values for each parameter
mat.samples = sapply(one_timer_sampling, function(spl)
  c(Intercept = spl$latent[idx[[1]]], detail_3 = spl$latent[idx[[2]]], g = spl$latent[idx[[3]]], w = spl$latent[idx[[4]]]))

# Save sample parameters
saveRDS(mat.samples, "Sample Parameters/One_Timer_Parameter_Samples.Rda")
##########



################################################################################

# Predict if there is traffic (on a shot preceded by a pass)

##########
# Take n samples from the posterior distribution
traffic_sampling = inla.posterior.sample(n = n_samples, result = traffic_with_pass)

# Find the rownames that correspond to our model parameters
x_names = rownames(traffic_sampling[[1]]$latent)

idx = lapply(c("Intercept", "g", "w"), function(nam)
  which(substr(x_names, 1, nchar(nam)) == nam))

# Extract the sample values for each parameter
mat.samples = sapply(traffic_sampling, function(spl)
  c(Intercept = spl$latent[idx[[1]]], g = spl$latent[idx[[2]]], w = spl$latent[idx[[3]]]))

# Save sample parameters
saveRDS(mat.samples, "Sample Parameters/Traffic_with_pass_Parameter_Samples.Rda")
##########



################################################################################

# Predict if there is traffic (on a shot NOT preceded by a pass)

##########
# Take n samples from the posterior distribution
traffic_sampling = inla.posterior.sample(n = n_samples, result = traffic_no_pass)

# Find the rownames that correspond to our model parameters
x_names = rownames(traffic_sampling[[1]]$latent)

idx = lapply(c("Intercept", "w"), function(nam)
  which(substr(x_names, 1, nchar(nam)) == nam))

# Extract the sample values for each parameter
mat.samples = sapply(traffic_sampling, function(spl)
  c(Intercept = spl$latent[idx[[1]]], w = spl$latent[idx[[2]]]))

# Save sample parameters
saveRDS(mat.samples, "Sample Parameters/Traffic_no_pass_Parameter_Samples.Rda")
##########



################################################################################

# Dump-in entry transition probabilities

##########
Dump_2_Recovery_sampling <- inla.posterior.sample(n = n_samples, result = dumped_2_recovery)

xnames <- rownames(Dump_2_Recovery_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_rec <- sapply(Dump_2_Recovery_sampling, function(spl)
  c(Intercept = spl$latent[idx[[1]]], w = spl$latent[idx[[2]]]))

saveRDS(mat.samples_rec, "Sample Parameters/Dump_to_Recovery_Parameter_Samples.Rda")


Dump_2_Turnover_sampling <- inla.posterior.sample(n=n_samples, result = dumped_2_turnover)

xnames <- rownames(Dump_2_Turnover_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_turn <- sapply(Dump_2_Turnover_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

saveRDS(mat.samples_turn, "Sample Parameters/Dump_to_Turnover_Parameter_Samples.Rda")
##########



################################################################################

# Controlled entry transition probabilities

##########
# Take n samples from the posterior distribution
control_2_shot_sampling = inla.posterior.sample(n = n_samples, result = controlled_2_shot)

# Find the rownames that correspond to our model parameters
x_names = rownames(control_2_shot_sampling[[1]]$latent) 

idx = lapply(c("Intercept", "is_near_boards", "i"), function(nam)
  which(substr(x_names, 1, nchar(nam))==nam))

# Extract the sample values for each parameter
mat.samples_shot = sapply(control_2_shot_sampling, function(spl)
  c(Intercept = spl$latent[idx[[1]]], is_near_boards = spl$latent[idx[[2]]], i = spl$latent[idx[[3]]][1:86]))

# Save sample parameters for future use
saveRDS(mat.samples_shot, "Sample Parameters/Controlled_Entry_to_Shot_Parameter_Samples.Rda")


# Take n samples from the posterior distribution
control_2_pass_sampling = inla.posterior.sample(n = n_samples, result = controlled_2_pass)

# Find the rownames that correspond to our model parameters
x_names = rownames(control_2_pass_sampling[[1]]$latent) 

idx = lapply(c("Intercept", "is_near_boards", "i"), function(nam)
  which(substr(x_names, 1, nchar(nam))==nam))

# Extract the sample values for each parameter
mat.samples_pass = sapply(control_2_pass_sampling, function(spl)
  c(Intercept = spl$latent[idx[[1]]], is_near_boards = spl$latent[idx[[2]]], i = spl$latent[idx[[3]][1:86]]))

# Save sample parameters for future use
saveRDS(mat.samples_pass, "Sample Parameters/Controlled_Entry_to_Pass_Parameter_Samples.Rda")


# Take n samples from the posterior distribution
control_2_turnover_sampling = inla.posterior.sample(n = n_samples, result = controlled_2_turnover)

# Find the rownames that correspond to our model parameters
x_names = rownames(control_2_turnover_sampling[[1]]$latent) 

idx = lapply(c("Intercept", "is_near_boards", "i"), function(nam)
  which(substr(x_names, 1, nchar(nam))==nam))

# Extract the sample values for each parameter
mat.samples_turnover = sapply(control_2_turnover_sampling, function(spl)
  c(Intercept = spl$latent[idx[[1]]], is_near_boards = spl$latent[idx[[2]]], i = spl$latent[idx[[3]][1:86]]))

# Save sample parameters for future use
saveRDS(mat.samples_turnover, "Sample Parameters/Controlled_Entry_to_Turnover_Parameter_Samples.Rda")
##########



################################################################################

# Pass transition probabilities

##########
Pass_2_Pass_sampling <- inla.posterior.sample(n = n_samples, result = pass_2_pass)

xnames <- rownames(Pass_2_Pass_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_pass <- sapply(Pass_2_Pass_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

# Save sample parameters for future use
saveRDS(mat.samples_pass, "Sample Parameters/Pass_to_Pass_Parameter_Samples.Rda")


Pass_2_Shot_sampling <- inla.posterior.sample(n=n_samples, result = pass_2_shot)

xnames <- rownames(Pass_2_Shot_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_shot <- sapply(Pass_2_Shot_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

# Save sample parameters for future use
saveRDS(mat.samples_shot, "Sample Parameters/Pass_to_Shot_Parameter_Samples.Rda")


Pass_2_Turnover_sampling <- inla.posterior.sample(n=n_samples, result = pass_2_turnover)

xnames <- rownames(Pass_2_Turnover_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_turnover <- sapply(Pass_2_Turnover_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

# Save sample parameters for future use
saveRDS(mat.samples_turnover, "Sample Parameters/Pass_to_Turnover_Parameter_Samples.Rda")
##########



################################################################################

# Failed Pass transition probabilities

##########
Failed_Pass_2_Turnover_sampling <- inla.posterior.sample(n=n_samples, result = fpass_2_turnover)

xnames <- rownames(Failed_Pass_2_Turnover_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_turnover <- sapply(Failed_Pass_2_Turnover_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

# Save sample parameters for future use
saveRDS(mat.samples_turnover, "Sample Parameters/Failed_Pass_to_Turnover_Parameter_Samples.Rda")


Failed_Pass_2_Recovery_sampling <- inla.posterior.sample(n=n_samples, result = fpass_2_recovery)

xnames <- rownames(Failed_Pass_2_Recovery_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_recovery <- sapply(Failed_Pass_2_Recovery_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

# Save sample parameters for future use
saveRDS(mat.samples_recovery, "Sample Parameters/Failed_Pass_to_Recovery_Parameter_Samples.Rda")
##########



################################################################################

# Shot transition probabilities

##########
# Take n samples from the posterior distribution
shot_2_recovery_sampling <- inla.posterior.sample(n = n_samples, result = shot_2_recovery)

# Find the rownames that correspond to our model parameters
xnames <- rownames(shot_2_recovery_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

# Extract the sample values for each parameter
mat.samples_recovery <- sapply(shot_2_recovery_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

# Save sample parameters for future use
saveRDS(mat.samples_recovery, "Sample Parameters/Shot_to_Recovery_Parameter_Samples.Rda")


# Take n samples from the posterior distribution
shot_2_turnover_sampling <- inla.posterior.sample(n = n_samples, result = shot_2_turnover)

# Find the rownames that correspond to our model parameters
xnames <- rownames(shot_2_turnover_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

# Extract the sample values for each parameter
mat.samples_turnover <- sapply(shot_2_turnover_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

# Save sample parameters for future use
saveRDS(mat.samples_turnover, "Sample Parameters/Shot_to_Turnover_Parameter_Samples.Rda")


# Take n samples from the posterior distribution
shot_2_whistle_sampling <- inla.posterior.sample(n = n_samples, result = shot_2_whistle)

# Find the rownames that correspond to our model parameters
xnames <- rownames(shot_2_whistle_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

# Extract the sample values for each parameter
mat.samples_whistle <- sapply(shot_2_whistle_sampling, function(spl)
  c(Intercept = spl$latent[idx[[1]]], w = spl$latent[idx[[2]]]))

# Save sample parameters for future use
saveRDS(mat.samples_whistle, "Sample Parameters/Shot_to_Whistle_Parameter_Samples.Rda")
##########



################################################################################

# Recovery transition probabilities

##########
Recovery_2_Pass_sampling <- inla.posterior.sample(n=n_samples, result = recovery_2_pass)

xnames <- rownames(Recovery_2_Pass_sampling[[1]]$latent)

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_pass <- sapply(Recovery_2_Pass_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

# Save sample parameters for future use
saveRDS(mat.samples_pass, "Sample Parameters/Recovery_to_Pass_Parameter_Samples.Rda")


Recovery_2_Shot_sampling <- inla.posterior.sample(n=n_samples, result = recovery_2_shot)

xnames <- rownames(Recovery_2_Shot_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_shot <- sapply(Recovery_2_Shot_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

# Save sample parameters for future use
saveRDS(mat.samples_shot, "Sample Parameters/Recovery_to_Shot_Parameter_Samples.Rda")


Recovery_2_Turnover_sampling <- inla.posterior.sample(n=n_samples, result = recovery_2_turnover)

xnames <- rownames(Recovery_2_Turnover_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_turnover <- sapply(Recovery_2_Turnover_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

# Save sample parameters for future use
saveRDS(mat.samples_turnover, "Sample Parameters/Recovery_to_Turnover_Parameter_Samples.Rda")
##########



################################################################################

# Turnover transition probabilities

##########
Turnover_2_Exit_sampling <- inla.posterior.sample(n=n_samples, result = turnover_2_exit)

xnames <- rownames(Turnover_2_Exit_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_exit <- sapply(Turnover_2_Exit_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

# Save sample parameters for future use
saveRDS(mat.samples_exit, "Sample Parameters/Turnover_to_Exit_Parameter_Samples.Rda")


Turnover_2_Recovery_sampling <- inla.posterior.sample(n=n_samples, result = turnover_2_recovery)

xnames <- rownames(Turnover_2_Recovery_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', 'w'), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_recovery <- sapply(Turnover_2_Recovery_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]],w=spl$latent[idx[[2]]]))

# Save sample parameters for future use
saveRDS(mat.samples_recovery, "Sample Parameters/Turnover_to_Recovery_Parameter_Samples.Rda")
##########



################################################################################

# Expected goals without pass

##########
# Take n samples from the posterior distribution
shot_np_sampling = inla.posterior.sample(n = n_samples, result = shot_2_goal_no_pass)

# Find the rownames that correspond to our model parameters
x_names = rownames(shot_np_sampling[[1]]$latent)

idx = lapply(c("Intercept", "detail_3", "shot_time_decay", "w"), 
             function(nam) which(substr(x_names, 1, nchar(nam)) == nam))

# Extract the sample values for each parameter
mat.samples = sapply(shot_np_sampling, function(spl)
  c(Intercept = spl$latent[idx[[1]]], detail_3 = spl$latent[idx[[2]]], 
    shot_time_decay = spl$latent[idx[[3]]], w = spl$latent[idx[[4]]]))

# Save sample parameters for future use
saveRDS(mat.samples, "Sample Parameters/Shot_to_Goal_no_pass_Parameter_Samples.Rda")
##########



################################################################################

# Expected goals with pass

##########
# Take n samples from the posterior distribution
shot_wp_sampling = inla.posterior.sample(n = n_samples, result = shot_2_goal_with_pass)

# Find the rownames that correspond to our model parameters
x_names = rownames(shot_wp_sampling[[1]]$latent)

idx = lapply(c("Intercept", "detail_3", "detail_4", "pass_distance", "angle_change", "pass_time_decay",
               "shot_time_decay", "entry_time_decay", "from_behind_net", "g", "w"), function(nam)
                 which(substr(x_names, 1, nchar(nam)) == nam))

# Extract the sample values for each parameter
mat.samples = sapply(shot_wp_sampling, function(spl)
  c(Intercept = spl$latent[idx[[1]]], detail_3 = spl$latent[idx[[2]]], detail_4 = spl$latent[idx[[3]]], 
    pass_distance = spl$latent[idx[[4]]], angle_change = spl$latent[idx[[5]]], pass_time_decay = spl$latent[idx[[6]]], 
    shot_time_decay = spl$latent[idx[[7]]], entry_time_decay = spl$latent[idx[[8]]], from_behind_net = spl$latent[idx[[9]]], 
    g = spl$latent[idx[[10]]], w = spl$latent[idx[[11]]]))

# Save sample parameters for future use
saveRDS(mat.samples, "Sample Parameters/Shot_to_Goal_with_pass_Parameter_Samples.Rda")
##########









