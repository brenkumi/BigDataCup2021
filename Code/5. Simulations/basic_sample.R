

# Required packages
library(tidyverse)
library(INLA)
library(doParallel)

options('mc.cores' = cores)
registerDoParallel(cores)

# Load in the data
done = readRDS(here::here("Data/all_samples_so_far.Rda")) %>%
  select(event_id) %>%
  unlist() %>%
  unique()


otters_data = read_csv(here::here("Data/otters_data_for_modelling.csv")) %>%
  filter(!(event_id %in% done))

zone_entries = otters_data %>%
  filter(prev_event == "Zone Entry")

zone_entries = zone_entries[sample(1:nrow(zone_entries), size = 500),]


passes = otters_data %>%
  filter(prev_event %in% c("Play", "Incomplete Play"))

passes = passes[sample(1:nrow(passes), size = 500),]


shots = otters_data %>%
  filter(prev_event == "Shot")

shots = shots[sample(1:nrow(shots), size = 500),]


turnovers = otters_data %>%
  filter(prev_event == "Turnover")

turnovers = turnovers[sample(1:nrow(turnovers), size = 500),]


recoveries = otters_data %>%
  filter(prev_event == "Recovery")

recoveries = recoveries[sample(1:nrow(recoveries), size = 500),]

otters_data = rbind(shots, passes, turnovers, recoveries) %>%
  bind_rows(fake_df)

# Create a data frame and list to store the data
#final_df = data.frame()

for (i in c(2)) {
  
  samp = x_goal_sample(n_samples = 1, 
                       x_loc = otters_data$same_direction_x[i], y_loc = otters_data$same_direction_y[i], 
                       prev_transition = otters_data$prev_event[i], 
                       n_passes_prev = otters_data$pass_count[i], 
                       n_shots = otters_data$shot_count[i], 
                       time_in_period = otters_data$seconds[i], 
                       time_since_entry = otters_data$time_since_entry[i], 
                       period = otters_data$period[i], 
                       before_entry_indicator = otters_data$before_entry_indicator[i], 
                       time_since_last_shot = otters_data$time_since_last_shot[i], 
                       xg_samples = 1,
                       offensive_score_difference = otters_data$offensive_score_diff[i],
                       event_id = otters_data$event_id[i])
  
  final_df = rbind(final_df, samp)
  
}

otdata_with_random = otters_data

a = otters_data[21:1906,]

a = a[sample(nrow(a)),] %>%
  filter(event_id < 10000000)



otters_data = a

### RUN SIMULATIONS ###

i = 1

repeat{
  
  if (i == nrow(otters_data)) break
  
  print(paste("Event", i))
  
  samp = try(x_goal_sample(n_samples = 1, 
                           x_loc = otters_data$same_direction_x[i], y_loc = otters_data$same_direction_y[i], 
                           prev_transition = otters_data$prev_event[i], 
                           n_passes_prev = otters_data$pass_count[i], 
                           n_shots = otters_data$shot_count[i], 
                           time_in_period = otters_data$seconds[i], 
                           time_since_entry = otters_data$time_since_entry[i], 
                           period = otters_data$period[i], 
                           before_entry_indicator = otters_data$before_entry_indicator[i], 
                           time_since_last_shot = otters_data$time_since_last_shot[i], 
                           xg_samples = 1,
                           offensive_score_difference = otters_data$offensive_score_diff[i],
                           event_id = otters_data$event_id[i]))
  
  if ('try-error' %in% class(samp)) {print(otters_data$event_id[i-1]); next}
  else final_df = rbind(final_df, samp)
  
  i <- i + 1
}



### SAVE UPDATED DATA ###

otters_data2 = read_csv(here::here("Data/otters_data_for_modelling.csv"))

final_df2 = merge(otters_data2, final_df, by = "event_id", all.y = TRUE)

total_data = readRDS(here::here("Data/all_samples_so_far.Rda")) %>%
  rbind(final_df2)

saveRDS(total_data, "Data/all_samples_so_far.Rda")
