
#####################
### LOAD PACKAGES ###
#####################

library(foreach) # To run for loop in parallel

library(doParallel) # To run chain in parallel

library(doFuture) # To run chain in parallel

library(here) # To set working directory

library(INLA) # To run various INLA functions

library(tidyverse) # For various data manipulation functions

source("Code/2. Functions/Sampling-Functions.R")



#####################
#### LOAD MODELS ####
#####################

setwd(here::here())

#########################
### TRANSITION MODELS ###

## CONTROLLED ENTRIES ##

# ...to Shot
controlled_2_shot = readRDS("Models/Entry/Controlled_Entry_to_Shot_ES_model.Rda")

# ...to Pass
controlled_2_play = readRDS("Models/Entry/Controlled_Entry_to_Play_ES_model.Rda")

# ...to Turnover
controlled_2_turnover = readRDS("Models/Entry/Controlled_Entry_to_Turnover_ES_model.Rda")


## DUMP-IN ENTRIES ##

# ...to Recovery
dumped_2_recovery = readRDS("Models/Entry/Dump_to_Recovery_ES_model.Rda")
dumped_2_recovery_mesh = readRDS("Models/Meshes/Dump_In_Entry_Mesh.Rda")

# ...to Turnover
dumped_2_turnover = readRDS("Models/Entry/Dump_to_Turnover_ES_model.Rda")
dumped_2_turnover_mesh = readRDS("Models/Meshes/Dump_In_Entry_Mesh.Rda")

## PASSES ##

# ...to Pass
play_2_play = readRDS("Models/Pass/Play_to_Play_ES_model.Rda")
play_2_play_mesh = readRDS("Models/Meshes/Pass_Transition_Mesh.Rda")

# ...to Shot
play_2_shot = readRDS("Models/Pass/Play_to_Shot_ES_model.Rda")
play_2_shot_mesh = readRDS("Models/Meshes/Pass_Transition_Mesh.Rda")

# ...to Turnover
play_2_turnover = readRDS("Models/Pass/Play_to_Turnover_ES_model.Rda")
play_2_turnover_mesh = readRDS("Models/Meshes/Pass_Transition_Mesh.Rda")


## SHOTS ##

# ...to Recovery
shot_2_recovery = readRDS("Models/Shot/Shot_to_Recovery_ES_model.Rda")
shot_2_recovery_mesh = readRDS("Models/Meshes/Shot_Transition_Mesh.Rda")

# ...to Turnover
shot_2_turnover = readRDS("Models/Shot/Shot_to_Turnover_ES_model.Rda")
shot_2_turnover_mesh = readRDS("Models/Meshes/Shot_Transition_Mesh.Rda")

# ...to Whistle
shot_2_whistle = readRDS("Models/Shot/Shot_to_Whistle_ES_model.Rda")
shot_2_whistle_mesh = readRDS("Models/Meshes/Shot_Transition_Mesh.Rda")


## RECOVERIES ##

# ...to Pass
recovery_2_play = readRDS("Models/Recovery/Recovery_to_Play_ES_model.Rda")
recovery_2_play_mesh = readRDS("Models/Meshes/Recov_Turnover_Transition_Mesh.Rda")

# ...to Shot
recovery_2_shot = readRDS("Models/Recovery/Recovery_to_Shot_ES_model.Rda")
recovery_2_shot_mesh = readRDS("Models/Meshes/Recov_Turnover_Transition_Mesh.Rda")

# ...to Turnover
recovery_2_turnover = readRDS("Models/Recovery/Recovery_to_Turnover_ES_model.Rda")
recovery_2_turnover_mesh = readRDS("Models/Meshes/Recov_Turnover_Transition_Mesh.Rda")


## TURNOVERS ##

# ...to Recovery
turnover_2_recovery = readRDS("Models/Turnover/Turnover_to_Recovery_ES_model.Rda")
turnover_2_recovery_mesh = readRDS("Models/Meshes/Recov_Turnover_Transition_Mesh.Rda")

# ...to Exit
turnover_2_exit = readRDS("Models/Turnover/Turnover_to_Exit_ES_model.Rda")
turnover_2_exit_mesh = readRDS("Models/Meshes/Recov_Turnover_Transition_Mesh.Rda")



####################
### VALUE MODELS ###

## SHOT TO GOAL WITH PASS ##
shot_2_goal_with_pass = readRDS("Models/Shot/Shot_to_Goal_ES_with_pass_model.Rda")
shot_2_goal_wp_shot_mesh = readRDS("Models/Meshes/Shot_to_Goal_with_pass_ES_Shot_Mesh.Rda")
shot_2_goal_wp_pass_mesh = readRDS("Models/Meshes/Shot_to_Goal_with_pass_ES_Pass_Mesh.Rda")
shot_2_goal_wp_time_mesh = readRDS("Models/Meshes/Shot_to_Goal_with_pass_ES_Time_Mesh.Rda")

## SHOT TO GOAL NO PASS ##
shot_2_goal_no_pass = readRDS("Models/Shot/Shot_to_Goal_ES_no_pass_model.Rda")
shot_2_goal_np_shot_mesh = readRDS("Models/Meshes/Shot_to_Goal_no_pass_ES_Shot_Mesh.Rda")
shot_2_goal_np_time_mesh = readRDS("Models/Meshes/Shot_to_Goal_no_pass_ES_Time_Mesh.Rda")


###################
### GLUE MODELS ###

## ENTRY DECISION ##
entry_decision = readRDS("Models/Entry/Entry_Decision_ES_model.Rda")

## X-LOCATION MODEL ##
x_prediction = readRDS("Models/Glue Models/X_Location_ES_model.Rda")
x_prediction_mesh = readRDS("Models/Meshes/Mesh_x_y_ES_prediction.Rda")

## Y-LOCATION MODEL ##
y_prediction = readRDS("Models/Glue Models/Y_Location_ES_model.Rda")
y_prediction_mesh = readRDS("Models/Meshes/Mesh_x_y_ES_prediction.Rda")

## TIME-TO-EVENT MODEL ##
time_to_event = readRDS("Models/Glue Models/Time_to_Event_ES_model.Rda")
time_to_event_mesh = readRDS("Models/Meshes/Time_to_Event_ES_Mesh.Rda")

## DIRECT/INDIRECT PASS ##
direct_pass = readRDS("Models/Glue Models/Direct_Pass_ES_model.Rda")
direct_pass_mesh = readRDS("Models/Meshes/Direct_Pass_ES_Mesh.Rda")

## PASS TARGET ##
pass_x_target = readRDS("Models/Glue Models/Pass_X_Location_ES_model.Rda")
pass_y_target = readRDS("Models/Glue Models/Pass_Y_Location_ES_model.Rda")
pass_target_mesh = readRDS("Models/Meshes/Mesh_pass_target.Rda")

## TRAFFIC WITH PASS ##
traffic_with_pass = readRDS("Models/Glue Models/Traffic_with_pass_ES_model.Rda")
traffic_wp_shot_mesh = readRDS("Models/Meshes/Traffic_with_pass_ES_Shot_Mesh.Rda")
traffic_wp_pass_mesh = readRDS("Models/Meshes/Traffic_with_pass_ES_Pass_Mesh.Rda")
traffic_wp_time_mesh = readRDS("Models/Meshes/Traffic_with_pass_ES_Time_Mesh.Rda")

## TRAFFIC NO PASS ##
traffic_no_pass = readRDS("Models/Glue Models/Traffic_no_pass_ES_model.Rda")
traffic_np_shot_mesh = readRDS("Models/Meshes/Traffic_no_pass_ES_Shot_Mesh.Rda")
traffic_np_time_mesh = readRDS("Models/Meshes/Traffic_no_pass_ES_Time_Mesh.Rda")

## ONE-TIMER ##
one_timer = readRDS("Models/Glue Models/One_Timer_ES_model.Rda")
one_timer_shot_mesh = readRDS("Models/Meshes/One_Timer_ES_Shot_Mesh.Rda")
one_timer_pass_mesh = readRDS("Models/Meshes/One_Timer_ES_Pass_Mesh.Rda")
one_timer_time_mesh = readRDS("Models/Meshes/One_Timer_ES_Time_Mesh.Rda")
one_timer_time_mesh_random = readRDS("Models/Meshes/One_Timer_ES_Time_Mesh_Random.Rda")


### SET UP SYSTEM TO RUN IN PARALLEL ###
system <- "Windows" # Change this to Linux or MAC if not on windows

if(system =="Windows"){

  cl <- parallel::makeCluster(cores)
  registerDoFuture()
  plan(multisession, workers = cl)

}else{
  options('mc.cores' = cores)
  registerDoParallel(cores)
}

### CREATE THE EXPECTED POSSESSION VALUE SIMULATION ###
x_goal_sample <- function(n_samples, x_loc, y_loc, prev_transition, n_passes_prev, n_shots, time_in_period, time_since_entry, 
                          period, offensive_score_difference, before_entry_indicator, time_since_last_shot, xg_samples, event_id){
  
  source("Code/2. Functions/Sampling-Functions.R")
  
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
      control_prob <- gen_samp_entry_decision(n_samples = 1, y_loc = y_loc, offensive_score_diff = offensive_score_difference, 
                                              period = period)
      
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
        transition <- c("Shot", "Play","Turnover")[which(trans_real == 1)]
        
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
        trans_prob = dump_transition_probabilites(n_samples = 1, x_loc = x_loc, y_loc = y_loc, prev_event = prev_transition, score_state = score_state)
        
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
        
        if(prev_transition =="Play"){
          
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
        trans_prob <- shot_transition_probabilities(n_samples = 1, x_loc = x_loc, y_loc = y_loc, time_since_entry = time_since_entry,
                                                    shot_2_recovery = shot_2_recovery, shot_2_turnover = shot_2_turnover,
                                                    shot_2_whistle = shot_2_whistle)
        
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
        
        if(transition =="Play"){
          
          ###########################################
          ############### PASS EVENTS ###############
          ###########################################
          
          # Reset pass count if the previous play wasn't a pass
          if(prev_transition != "Play") {
            n_passes_prev = 0
          }
          
          ############################
          # UPDATE LOCATION AND TIME #
          ############################
          
          
          
          # Determine the xy-coordinates where the shot will occur from
          x_loc = rnorm(1, gen_samp_x_location(n_samples = 1, x_loc = x_prev, y_loc = y_prev, prev_transition = prev_transition,
                                               time_since_entry = time_since_entry,transition = "Play"), 13)
          
          x_loc = case_when(
            x_loc > 200 ~ 200,
            x_loc < 125 ~ 125,
            TRUE ~ x_loc
          )
          
          y_loc <- rnorm(1,gen_samp_y_location(n_samples = 1, x_prev = x_prev, y_prev = y_prev, x_loc = x_loc, prev_transition = prev_transition, 
                                               transition = "Play", time_since_entry = time_since_entry),22)
          
          y_loc = case_when(
            y_loc > 85 ~ 85,
            y_loc < 0 ~ 0,
            TRUE ~ y_loc
          )
          
          # Determine the time that has lapsed between the last two events
          time_sample = gen_samp_time(n_samples = 1, x_loc = x_loc, y_loc = y_loc, transition = "Play",
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
          
          
          ########################################
          # DETERMINE NEXT TRANSITION TRANSITION #
          ########################################
          
          # Determine next event to transition to
          trans_prob <- pass_transition_probabilities(n_samples = 1, x_loc = pass_end_x, y_loc = pass_end_y, n_passes_prev = n_passes_prev,
                                                      pass_2_pass = play_2_play, pass_2_shot = play_2_shot, pass_2_turnover = play_2_turnover)
          
          # Take a multinomial sample to see which we move to
          trans_real <- t(rmultinom(1, 1, trans_prob))
          
          # Transition to our next state
          transition <- c("Play", "Shot", "Turnover")[which(trans_real == 1)]
          
          
          ######################################
          # UPDATE FUNCTIONS OF TIME AND SPACE #
          ######################################
          
          # Update all functions of time and space
          n_passes_prev <- n_passes_prev + 1
          
          # Update previous xy-coordinates
          x_prev = x_loc
          y_prev = y_loc
          
          # Update previous transition
          prev_transition = "Play"
          
          first_play = 0
          passes = passes + 1
          sequence = append(sequence, "Play")
          
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
            trans_prob <- turnover_transition_probabilities(n_samples = 1, x_loc = x_loc, y_loc = y_loc, time_since_entry = time_since_entry,
                                                            turnover_2_exit = turnover_2_exit, turnover_2_recovery = turnover_2_recovery)
            
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
            trans_prob <- recovery_transition_probabilities(n_samples = 1, x_loc = x_loc, y_loc = y_loc, time_since_entry = time_since_entry,
                                                            recovery_2_play = recovery_2_play, recovery_2_shot = recovery_2_shot,
                                                            recovery_2_turnover = recovery_2_turnover)
            
            # Take a multinomial sample to see which we move to
            trans_real <- t(rmultinom(1, 1, trans_prob))
            
            # Transition to our next state
            transition <- c("Play", "Turnover", "Shot")[which(trans_real == 1)]
            
            
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




