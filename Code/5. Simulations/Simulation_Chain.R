
#####################
### LOAD PACKAGES ###
#####################

library(foreach) # To run for loop in parallel

library(doParallel) # To run chain in parallel

library(doFuture) # To run chain in parallel

library(here) # To set working directory

library(INLA) # To run various INLA functions

library(tidyverse) # For various data manipulation functions

source("Code/0. Functions/Sampling-Functions.R")



####################################
#### LOAD MESHES AND PARAMETERS ####
####################################

setwd(here::here())

########################
### INLA SPDE MESHES ###

## DUMP-IN ENTRIES ##
dump_mesh = readRDS("Models/Meshes/Dump_In_Entry_Mesh.Rda")

## PASSES, FAILED PASSES, RECOVERIES, AND TURNOVERS ##
rtp_mesh = readRDS("Models/Meshes/Recov_Turn_Pass_Mesh.Rda")

## SHOTS ##
shot_trans_mesh = readRDS("Models/Meshes/Shot_Transition_Mesh.Rda")
shot_2_goal_wp_shot_mesh = readRDS("Models/Meshes/Shot_to_Goal_with_pass_ES_Shot_Mesh.Rda")
shot_2_goal_wp_pass_mesh = readRDS("Models/Meshes/Shot_to_Goal_with_pass_ES_Pass_Mesh.Rda")
shot_2_goal_wp_time_mesh = readRDS("Models/Meshes/Shot_to_Goal_with_pass_ES_Time_Mesh.Rda")
shot_2_goal_np_shot_mesh = readRDS("Models/Meshes/Shot_to_Goal_no_pass_ES_Shot_Mesh.Rda")
shot_2_goal_np_time_mesh = readRDS("Models/Meshes/Shot_to_Goal_no_pass_ES_Time_Mesh.Rda")

## MOVEMENT AND TIME ##
xy_mesh = readRDS("Models/Meshes/Mesh_x_y_ES_prediction.Rda")
pass_target_mesh = readRDS("Models/Meshes/Pass_XY_Location_ES_Mesh.Rda")
time_mesh = readRDS("Models/Meshes/Time_to_Event_ES_Mesh.Rda")
pass_completion_start_mesh = readRDS("Models/Meshes/Pass_Completion_Start_ES_Mesh.Rda")
pass_completion_end_mesh = readRDS("Models/Meshes/Pass_Completion_End_ES_Mesh.Rda")
direct_pass_mesh = readRDS("Models/Meshes/Direct_Pass_ES_Mesh.Rda")
traffic_wp_shot_mesh = readRDS("Models/Meshes/Traffic_with_pass_ES_Shot_Mesh.Rda")
traffic_wp_pass_mesh = readRDS("Models/Meshes/Traffic_with_pass_ES_Pass_Mesh.Rda")
traffic_wp_time_mesh = readRDS("Models/Meshes/Traffic_with_pass_ES_Time_Mesh.Rda")
traffic_np_shot_mesh = readRDS("Models/Meshes/Traffic_no_pass_ES_Shot_Mesh.Rda")
traffic_np_time_mesh = readRDS("Models/Meshes/Traffic_no_pass_ES_Time_Mesh.Rda")
one_timer_shot_mesh = readRDS("Models/Meshes/One_Timer_ES_Shot_Mesh.Rda")
one_timer_pass_mesh = readRDS("Models/Meshes/One_Timer_ES_Pass_Mesh.Rda")
one_timer_time_mesh = readRDS("Models/Meshes/One_Timer_ES_Time_Mesh.Rda")
one_timer_time_mesh_random = readRDS("Models/Meshes/One_Timer_ES_Time_Mesh_Random.Rda")


#########################
### SAMPLE PARAMETERS ###

## ENTRIES ##
controlled_2_shot_parameters = readRDS("Sample Parameters/Controlled_Entry_to_Shot_Parameter_Samples.Rda")
controlled_2_pass_parameters = readRDS("Sample Parameters/Controlled_Entry_to_Pass_Parameter_Samples.Rda")
controlled_2_turnover_parameters = readRDS("Sample Parameters/Controlled_Entry_to_Turnover_Parameter_Samples.Rda")
dumped_2_recovery_parameters = readRDS("Sample Parameters/Dump_to_Recovery_Parameter_Samples.Rda")
dumped_2_turnover_parameters = readRDS("Sample Parameters/Dump_to_Turnover_Parameter_Samples.Rda")

## PASSES ##
pass_2_pass_parameters = readRDS("Sample Parameters/Pass_to_Pass_Parameter_Samples.Rda")
pass_2_shot_parameters = readRDS("Sample Parameters/Pass_to_Shot_Parameter_Samples.Rda")
pass_2_turnover_parameters = readRDS("Sample Parameters/Pass_to_Turnover_Parameter_Samples.Rda")
failed_pass_2_turnover_parameters = readRDS("Sample Parameters/Failed_Pass_to_Turnover_Parameter_Samples.Rda")
failed_pass_2_recovery_parameters = readRDS("Sample Parameters/Failed_Pass_to_Recovery_Parameter_Samples.Rda")

## SHOTS ##
shot_2_recovery_parameters = readRDS("Sample Parameters/Shot_to_Recovery_Parameter_Samples.Rda")
shot_2_turnover_parameters = readRDS("Sample Parameters/Shot_to_Turnover_Parameter_Samples.Rda")
shot_2_whistle_parameters = readRDS("Sample Parameters/Shot_to_Whistle_Parameter_Samples.Rda")
shot_2_goal_wp_parameters = readRDS("Sample Parameters/Shot_to_Goal_with_pass_Parameter_Samples.Rda")
shot_2_goal_np_parameters = readRDS("Sample Parameters/Shot_to_Goal_no_pass_Parameter_Samples.Rda")

## RECOVERIES ##
recovery_2_shot_parameters = readRDS("Sample Parameters/Recovery_to_Shot_Parameter_Samples.Rda")
recovery_2_pass_parameters = readRDS("Sample Parameters/Recovery_to_Pass_Parameter_Samples.Rda")
recovery_2_turnover_parameters = readRDS("Sample Parameters/Recovery_to_Turnover_Parameter_Samples.Rda")

## TURNOVERS ##
turnover_2_recovery_parameters = readRDS("Sample Parameters/Turnover_to_Recovery_Parameter_Samples.Rda")
turnover_2_exit_parameters = readRDS("Sample Parameters/Turnover_to_Exit_Parameter_Samples.Rda")

## MOVEMENT AND TIME ##
entry_decision_parameters = readRDS("Sample Parameters/Entry_Decision_Parameter_Samples.Rda")
x_prediction_parameters = readRDS("Sample Parameters/X_Location_Parameter_Samples.Rda")
y_prediction_parameters = readRDS("Sample Parameters/Y_Location_Parameter_Samples.Rda")
time_to_event_parameters = readRDS("Sample Parameters/Time_to_Event_Parameter_Samples.Rda")
direct_pass_parameters = readRDS("Sample Parameters/Direct_Pass_Parameter_Samples.Rda")
pass_completion_parameters = readRDS("Sample Parameters/Pass_Completion_Parameter_Samples.Rda")
pass_x_target_parameters = readRDS("Sample Parameters/Pass_X_Location_Parameter_Samples.Rda")
pass_y_target_parameters = readRDS("Sample Parameters/Pass_Y_Location_Parameter_Samples.Rda")
one_timer_parameters = readRDS("Sample Parameters/One_Timer_Parameter_Samples.Rda")
traffic_wp_parameters = readRDS("Sample Parameters/Traffic_with_pass_Parameter_Samples.Rda")
traffic_np_parameters = readRDS("Sample Parameters/Traffic_no_pass_Parameter_Samples.Rda")




#####################
### PARALLELIZE R ###
#####################

### SET UP SYSTEM TO RUN IN PARALLEL ###
cores = detectCores()

system <- "Windows" # Change this to Linux or MAC if not on windows

if(system =="Windows"){

  cl <- parallel::makeCluster(cores)
  registerDoFuture()
  plan(multisession, workers = cl)

}else{
  options('mc.cores' = cores)
  registerDoParallel(cores)
}




#####################################################
### EXPECTED POSSESSION VALUE SIMULATION FUNCTION ###
#####################################################

x_goal_sample <- function(n_samples, x_loc, y_loc, prev_transition, n_passes_prev, n_shots, time_in_period, time_since_entry, 
                          period, offensive_score_difference, before_entry_indicator, time_since_last_shot, xg_samples, event_id){
  
  # Load in sampling functions
  source("Code/0. Functions/Sampling-Functions.R")
  
  # The chain ends when either the period is up (reach 1200 seconds) or a whistle or an exit
  xseq <- c(1:n_samples)
  
  # Create a vector to store our total xG from a simulation
  xgs <- vector(length = n_samples)
  
  # Set the score state
  score_state = case_when(
    (period %in% c(1,2) & offensive_score_difference >= 2) | (period == 3 & offensive_score_difference >= 1) ~ "Lead",
    (period %in% c(1,2) & offensive_score_difference <= -2) | (period == 3 & offensive_score_difference <= -1) ~ "Deficit",
    TRUE ~ "Close"
  )
  
  prev_transition_og = prev_transition
  
  x_loc_og <- x_loc
  y_loc_og <- y_loc
  n_passes_prev_og <- n_passes_prev
  n_shots_og <- n_shots
  time_in_period_og <- time_in_period
  time_since_entry_og <- time_since_entry
  time_since_last_shot_og <- time_since_last_shot
  
  # Wrap in future_lapply so we can run on multiple cores
  foreach(mm = 1:n_samples, .combine = 'rbind')%dopar%{
    
    first_play = 1
    
    prev_transition = prev_transition_og
    x_loc <- x_loc_og
    y_loc <- y_loc_og
    n_passes_prev <- n_passes_prev_og
    time_in_period <- time_in_period_og
    time_since_entry <- time_since_entry_og
    time_since_last_shot <- time_since_last_shot_og
    n_shots <- n_shots_og
    
    entry = NA
    shots = 0
    passes = 0
    recoveries = 0
    turnovers = 0
    turnovers = 0
    sequence = c()
    
    if (before_entry_indicator == 0) {
      transition = prev_transition_og
    }
    
    if (time_since_last_shot %in% NA) {
      time_since_last_shot = 100
    }
    
    xg <- 0
    
    xg_mult <- 1
    
    x_prev <- x_loc
    y_prev <- y_loc
    
    
    # Begin with an entry (if we aren't in the zone already)
    if(before_entry_indicator == 1){
      
      # Initialize time_since_entry and time_since_last_shot
      time_since_entry <- 0
      time_since_last_shot <- 100
      
      ### ENTRY DECISION ###
      
      # Get a sample probability of entering the zone with control
      control_prob <- gen_samp_entry_decision(n_samples = 1, y_loc = y_loc, offensive_score_diff = offensive_score_difference, period = period)
      
      # Using this probability simulate if we enter with control or not
      control_ind <- rbinom(1,1,control_prob)
      
      # Simulate the outputted entry type
      if(control_ind == 1){
        
        ### CONTROLLED ENTRY ###
        
        # Determine probabilities of transitioning from controlled entry to shot, pass or turnover
        trans_prob <- controlled_transition_probabilities(n_samples = 1, y_loc = y_loc)
        
        # Take a multinomial sample to see which we move to
        trans_real <- t(rmultinom(1,1,trans_prob))
        
        # Transition to our next state
        transition <- c("Shot", "Pass","Turnover")[which(trans_real == 1)]
        
        # Set controlled entry as the previous event
        prev_transition <- "Controlled Entry"
        
        # Set our xy-locations as our previous locations now
        x_prev = x_loc
        y_prev = y_loc
        
        first_play = 0
        entry = "Controlled"
        sequence = append(sequence, "Zone Entry")
        
        
      }else{
        
        ### DUMP-IN ENTRY ###
        
        # Determine probabilities of transitioning from dump-in to recovery or turnover
        trans_prob = dump_transition_probabilites(n_samples = 1, x_loc = x_loc, y_loc = y_loc, score_state = score_state, 
                                                  samples_recovery = dumped_2_recovery_parameters, 
                                                  samples_turnover = dumped_2_turnover_parameters, dump_mesh = dump_mesh)
        
        # Take a multinomial sample to see which we move to
        trans_real <- t(rmultinom(1, 1, trans_prob))
        
        # Transition to our next state
        transition <- c("Recovery", "Turnover")[which(trans_real == 1)]
        
        # Set the entry as the previous transition
        prev_transition = "Dump-In Entry"
        
        # Set our xy-locations as our previous locations now
        x_prev = x_loc
        y_prev = y_loc
        
        first_play = 0
        entry = "Dumped"
        sequence = append(sequence, "Zone Entry")
        
      }
    }
    
    
    # Now we go continue to loop through the possible shot/pass/turnover/recovery transitions until there is an exit, whistle, or the period ends
    while((time_in_period < 1200) & !(transition %in% c("Exit","Whistle"))){
      
      if(transition == "Shot"){
        
        ###########################################
        ############### SHOT EVENTS ###############
        ###########################################
        
        ############################
        # UPDATE LOCATION AND TIME #
        ############################
        
        # Determine the xy-coordinates where the shot will occur from
        x_loc = rnorm(1, gen_samp_x_location(n_samples = 1, x_loc = x_prev, y_loc = y_prev, prev_transition = prev_transition,
                                             time_since_entry = time_since_entry,
                                             transition = "Shot"), 13)
        
        x_loc = case_when(
          x_loc > 200 ~ 200,
          x_loc < 125 ~ 125,
          TRUE ~ x_loc
        )
        
        y_loc <- rnorm(1,gen_samp_y_location(n_samples = 1, x_prev = x_prev, y_prev = y_prev, x_loc = x_loc, prev_transition = prev_transition, 
                                             transition = "Shot", time_since_entry = time_since_entry),22)
        
        y_loc = case_when(
          y_loc > 85 ~ 85,
          y_loc < 0 ~ 0,
          TRUE ~ y_loc
        )
        
        # Determine the time that has lapsed between the last two events
        time_sample = gen_samp_time(n_samples = 1, x_loc = x_loc, y_loc = y_loc, transition = "Shot",
                                    prev_x_loc = x_prev, prev_y_loc = y_prev, prev_transition = prev_transition)
        time_jump = rpois(1, time_sample)
        
        # Add on to our temporal variables
        time_since_entry = time_since_entry + time_jump
        time_since_last_shot = time_since_last_shot + time_jump
        time_in_period <- time_in_period + time_jump
        
        
        ####################
        ## EXPECTED GOALS ##
        ####################
        
        if(prev_transition =="Pass"){
          
          ## ...WITH PRE-SHOT MOVEMENT ##
          
          # Simulate if there is traffic
          traffic_prob = gen_samp_traffic_with_pass(n_samples = 1, shot_x = x_loc, shot_y = y_loc, pass_x = x_prev, pass_y = y_prev, 
                                                    time_since_entry = time_since_entry)
          
          is_traffic = rbinom(1, 1, traffic_prob)
          
          # Simulate if there is a one-timer
          one_t_prob = gen_samp_one_timer(n_samples = 1, shot_x = x_loc, shot_y = y_loc, pass_x = x_prev, pass_y = y_prev, 
                                          detail_3 = is_traffic, time_since_entry = time_since_entry, time_since_last_pass = time_jump)
          
          is_one_timer = rbinom(1, 1, one_t_prob)
          
          # Generate the expected goals
          goal_prob = expected_goals_with_pass(n_samples = xg_samples, shot_x = x_loc, shot_y = y_loc, pass_x = x_prev, pass_y = y_prev, 
                                               detail_3 = is_traffic, detail_4 = is_one_timer, time_since_last_pass = time_jump, 
                                               time_since_last_shot = time_since_last_shot, shot_count = n_shots, 
                                               time_since_entry = time_since_entry) %>% mean()
          
          if (first_play != 1) {
            # Add our expected goals
            xg = xg + goal_prob * xg_mult
            
            # Reduce our xG multiplier
            xg_mult = xg_mult * (1 - goal_prob)
          }
          
          first_play = 0
          
          
        }else{
          
          ## ...WITHOUT PRE-SHOT MOVEMENT ##
          
          # Simulate if there is traffic
          traffic_prob = gen_samp_traffic_no_pass(n_samples = 1, shot_x = x_loc, shot_y = y_loc, time_since_entry = time_since_entry)
          
          is_traffic = rbinom(1, 1, traffic_prob)
          
          # Generate the expected goals
          goal_prob = expected_goals_no_pass(n_samples = xg_samples, shot_x = x_loc, shot_y = y_loc, detail_3 = is_traffic,
                                             time_since_last_shot = time_since_last_shot, shot_count = n_shots, 
                                             time_since_entry = time_since_entry) %>% mean()
          
          if (first_play != 1) {
            # Add our expected goals
            xg = xg + goal_prob * xg_mult
            
            # Reduce our xG multiplier
            xg_mult = xg_mult * (1 - goal_prob)
          }
          
          first_play = 0
          
        }
        
        ########################################
        # DETERMINE NEXT TRANSITION TRANSITION #
        ########################################
        
        # Determine probabilities of transitioning from shot to recovery, turnover or whistle
        trans_prob <- shot_transition_probabilities(n_samples = 1, x_loc = x_loc, y_loc = y_loc, time_since_entry = time_since_entry)
        
        # Take a multinomial sample to see which we move to
        trans_real <- t(rmultinom(1, 1, trans_prob))
        
        # Transition to our next state
        transition <- c("Recovery", "Turnover", "Whistle")[which(trans_real == 1)]
        
        
        ######################################
        # UPDATE FUNCTIONS OF TIME AND SPACE #
        ######################################
        
        # Reset time since last shot
        time_since_last_shot = 0
        
        # Add to the shot count
        n_shots = n_shots + 1
        
        # Update previous xy-coordinates
        x_prev = x_loc
        y_prev = y_loc
        
        # Update previous transition
        prev_transition = "Shot"
        shots = shots + 1
        sequence = append(sequence, "Shot")
        
        
      }else{
        
        if(transition =="Pass"){
          
          ###########################################
          ############### PASS EVENTS ###############
          ###########################################
          
          # Reset pass count if the previous play wasn't a pass
          if(prev_transition != "Pass") {
            n_passes_prev = 0
          }
          
          ############################
          # UPDATE LOCATION AND TIME #
          ############################
          
          # Determine the xy-coordinates where the shot will occur from
          x_loc = rnorm(1, gen_samp_x_location(n_samples = 1, x_loc = x_prev, y_loc = y_prev, prev_transition = prev_transition,
                                               time_since_entry = time_since_entry,transition = "Pass"), 13)
          
          x_loc = case_when(
            x_loc > 200 ~ 200,
            x_loc < 125 ~ 125,
            TRUE ~ x_loc
          )
          
          y_loc <- rnorm(1,gen_samp_y_location(n_samples = 1, x_prev = x_prev, y_prev = y_prev, x_loc = x_loc, prev_transition = prev_transition, 
                                               transition = "Pass", time_since_entry = time_since_entry),22)
          
          y_loc = case_when(
            y_loc > 85 ~ 85,
            y_loc < 0 ~ 0,
            TRUE ~ y_loc
          )
          
          # Determine the time that has lapsed between the last two events
          time_sample = gen_samp_time(n_samples = 1, x_loc = x_loc, y_loc = y_loc, transition = "Pass",
                                      prev_x_loc = x_prev, prev_y_loc = y_prev, prev_transition = prev_transition)
          time_jump = rpois(1, time_sample)
          
          # Add on to our temporal variables
          time_since_entry = time_since_entry + time_jump
          time_since_last_shot = time_since_last_shot + time_jump
          time_in_period <- time_in_period + time_jump
          
          
          ###########################
          # DETERMINE PASS ENDPOINT #
          ###########################
          
          # Determine if the pass target was direct or indirect
          direct_prob = gen_samp_direct_pass(n_samples = 1, x_loc = x_loc, y_loc = y_loc, prev_event = prev_transition, score_state = score_state)
          
          is_direct = rbinom(1, 1, direct_prob)
          
          # Set pass_type as direct/indirect
          pass_type = ifelse(is_direct == 1, "Direct", "Indirect")
          
          # Determine the pass target location
          pass_target = gen_samp_pass_target(n_samples = 1, x_loc = x_loc, y_loc = y_loc, pass_type = pass_type)
          pass_end_x = rnorm(1,pass_target[[1]],22)
          pass_end_y = rnorm(1,pass_target[[2]],22)
          
          
          pass_end_x = case_when(
            pass_end_x > 200 ~ 200,
            pass_end_x < 125 ~ 125,
            TRUE ~ pass_end_x
          )
          
          pass_end_y = case_when(
            pass_end_y > 85 ~ 85,
            pass_end_y < 0 ~ 0,
            TRUE ~ pass_end_y
          )
          
          # Determine the time since the last pass (if the pass occurred directly before it)
          time_since_last_pass = ifelse(prev_transition == "Pass", 100, time_jump)
          
          # Determine if the pass is complete
          completion_prob = gen_samp_pass_completion(n_samples = 1, x_loc = x_loc, y_loc = y_loc, target_x = pass_end_x,
                                                     target_y = pass_end_y, time_since_last_pass = time_since_last_pass,
                                                     is_direct = is_direct, pass_count = n_passes_prev)
          
          is_complete = rbinom(1, 1, completion_prob)
          
          
          ########################################
          # DETERMINE NEXT TRANSITION TRANSITION #
          ########################################
          
          if (is_complete == 1) {
            
            # Determine next event to transition to
            trans_prob <- pass_transition_probabilities(n_samples = 1, x_loc = pass_end_x, y_loc = pass_end_y, 
                                                        time_since_entry = time_since_entry)
            
            # Take a multinomial sample to see which we move to
            trans_real <- t(rmultinom(1, 1, trans_prob))
            
            # Transition to our next state
            transition <- c("Pass", "Shot", "Turnover")[which(trans_real == 1)]
            
            # Add pass
            sequence = append(sequence, "Pass")
            
          } else {
            
            # Determine next event to transition to
            trans_prob <- failed_pass_transition_probabilities(n_samples = 1, x_loc = pass_end_x, y_loc = pass_end_y, 
                                                               time_since_entry = time_since_entry)
            
            # Take a multinomial sample to see which we move to
            trans_real <- t(rmultinom(1, 1, trans_prob))
            
            # Transition to our next state
            transition <- c("Recovery", "Turnover")[which(trans_real == 1)]
            
            # Add failed pass
            sequence = append(sequence, "Failed Pass")
            
          }
          
          ######################################
          # UPDATE FUNCTIONS OF TIME AND SPACE #
          ######################################
          
          # Update all functions of time and space
          n_passes_prev <- n_passes_prev + 1
          
          # Update previous xy-coordinates
          x_prev = x_loc
          y_prev = y_loc
          
          # Update previous transition
          prev_transition = "Pass"
          
          first_play = 0
          passes = passes + 1
          
          
        }else{
          
          if(transition =="Turnover"){
            
            ###########################################
            ############# TURNOVER EVENTS #############
            ###########################################
            
            ############################
            # UPDATE LOCATION AND TIME #
            ############################
            
            # Determine the xy-coordinates where the shot will occur from
            x_loc = rnorm(1, gen_samp_x_location(n_samples = 1, x_loc = x_prev, y_loc = y_prev, prev_transition = prev_transition,
                                                 time_since_entry = time_since_entry,transition = "Turnover"), 13)
            
            x_loc = case_when(
              x_loc > 200 ~ 200,
              x_loc < 125 ~ 125,
              TRUE ~ x_loc
            )
            
            y_loc <- rnorm(1,gen_samp_y_location(n_samples = 1, x_prev = x_prev, y_prev = y_prev, x_loc = x_loc, prev_transition = prev_transition, 
                                                 transition = "Turnover", time_since_entry = time_since_entry),22)
            
            y_loc = case_when(
              y_loc > 85 ~ 85,
              y_loc < 0 ~ 0,
              TRUE ~ y_loc
            )
            
            # Determine the time that has lapsed between the last two events
            time_sample = gen_samp_time(n_samples = 1, x_loc = x_loc, y_loc = y_loc, transition = "Turnover",
                                        prev_x_loc = x_prev, prev_y_loc = y_prev, prev_transition = prev_transition)
            time_jump = rpois(1, time_sample)
            
            # Add on to our temporal variables
            time_since_entry = time_since_entry + time_jump
            time_since_last_shot = time_since_last_shot + time_jump
            time_in_period <- time_in_period + time_jump
            
            
            ########################################
            # DETERMINE NEXT TRANSITION TRANSITION #
            ########################################
            
            # Determine next event to transition to
            trans_prob <- turnover_transition_probabilities(n_samples = 1, x_loc = x_loc, y_loc = y_loc, time_since_entry = time_since_entry)
            
            # Take a multinomial sample to see which we move to
            trans_real <- t(rmultinom(1, 1, trans_prob))
            
            # Transition to our next state
            transition <- c("Exit", "Recovery")[which(trans_real == 1)]
            
            
            ######################################
            # UPDATE FUNCTIONS OF TIME AND SPACE #
            ######################################
            
            # Update previous xy-coordinates
            x_prev = x_loc
            y_prev = y_loc
            
            # Update previous transition
            prev_transition = "Turnover"
            
            first_play = 0
            turnovers = turnovers + 1
            sequence = append(sequence, "Turnover")
            
          }else{
            
            ###########################################
            ############# RECOVERY EVENTS #############
            ###########################################
            
            ############################
            # UPDATE LOCATION AND TIME #
            ############################
            
            # Determine the xy-coordinates where the shot will occur from
            x_loc = rnorm(1, gen_samp_x_location(n_samples = 1, x_loc = x_prev, y_loc = y_prev, prev_transition = prev_transition,
                                                 time_since_entry = time_since_entry,transition = "Recovery"), 13)
            
            x_loc = case_when(
              x_loc > 200 ~ 200,
              x_loc < 125 ~ 125,
              TRUE ~ x_loc
            )
            
            y_loc <- rnorm(1,gen_samp_y_location(n_samples = 1, x_prev = x_prev, y_prev = y_prev, x_loc = x_loc, prev_transition = prev_transition, 
                                                 transition = "Recovery", time_since_entry = time_since_entry),22)
            
            y_loc = case_when(
              y_loc > 85 ~ 85,
              y_loc < 0 ~ 0,
              TRUE ~ y_loc
            )
            
            # Determine the time that has lapsed between the last two events
            time_sample = gen_samp_time(n_samples = 1, x_loc = x_loc, y_loc = y_loc, transition = "Recovery",
                                        prev_x_loc = x_prev, prev_y_loc = y_prev, prev_transition = prev_transition)
            time_jump = rpois(1, time_sample)
            
            # Add on to our temporal variables
            time_since_entry = time_since_entry + time_jump
            time_since_last_shot = time_since_last_shot + time_jump
            time_in_period <- time_in_period + time_jump
            
            
            ########################################
            # DETERMINE NEXT TRANSITION TRANSITION #
            ########################################
            
            # Determine next event to transition to
            trans_prob <- recovery_transition_probabilities(n_samples = 1, x_loc = x_loc, y_loc = y_loc, time_since_entry = time_since_entry)
            
            # Take a multinomial sample to see which we move to
            trans_real <- t(rmultinom(1, 1, trans_prob))
            
            # Transition to our next state
            transition <- c("Pass", "Turnover", "Shot")[which(trans_real == 1)]
            
            
            ######################################
            # UPDATE FUNCTIONS OF TIME AND SPACE #
            ######################################
            
            # Update previous xy-coordinates
            x_prev = x_loc
            y_prev = y_loc
            
            # Update previous transition
            prev_transition = "Recovery"
            
            first_play = 0
            recoveries = recoveries + 1
            sequence = append(sequence, "Recovery")
            
            
          }
          
          if (transition %in% c("Whistle", "Exit")) {
            sequence = append(sequence, transition)
          }
          
        }
        
      }
      
    } # This is the end of the while Loop!!!!!
    
    # Add our tallied expected goals to xgs
    #xgs[mm] = xg
    
    # Tally total shots
    
    # Tally total passes
    
    # Tally other transitions
    
    data.frame(event_id = event_id, observation = mm, xPV = xg, x_loc = x_loc, y_loc = y_loc, prev_transition = prev_transition, transition = transition,
               time_in_period = time_in_period, time_since_last_shot = time_since_last_shot, time_since_entry = time_since_entry,
               entry = entry, shots = shots, passes = passes, recoveries = recoveries, turnovers = turnovers) %>%
      mutate(sequence = map(.x = observation, ~sequence))
    
  } # End of future_lapply
} # End of function




