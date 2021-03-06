
# PAV4 stuff
otters_with_values = readRDS(here::here("Data/otters_PAV.csv"))










# Load in in-zone posterior samples
posterior_samples_in_zone = readRDS(here::here("Models/Value/posterior_ex_samples_1000.Rda")) %>%
  as.data.frame()

for (i in 1:1000) {
  colnames(posterior_samples_in_zone)[i] = paste("xPV", i, sep = "_")
}


# Load in zone entry posterior samples
posterior_samples_entries = readRDS("entry_samples.Rda") %>%
  as.data.frame()

for (i in 1:1000) {
  colnames(posterior_samples_entries)[i] = paste("xPV", i, sep = "_")
}


# Add in in-zone samples
otters_posteriors_in_zone = read_csv(here::here("Data/otters_data_for_modelling.csv")) %>%
  filter(entry_strength == "ES") %>%
  mutate(time_since_entry.x = time_since_entry) %>%
  filter(event != "Zone Entry") %>%
  cbind(posterior_samples_in_zone)

# Add in entry samples
otters_posterior_entries = read_csv(here::here("Data/otters_data_for_modelling.csv")) %>%
  filter(entry_strength == "ES") %>%
  mutate(time_since_entry.x = time_since_entry) %>%
  filter(event == "Zone Entry") %>%
  cbind(posterior_samples_entries)

# Combine entries and in-zone events
otters_posteriors = rbind(otters_posteriors_in_zone, otters_posterior_entries) %>%
  arrange(event_id)

# Load in projected xPV following shot to whistle and shot to goal events
next_epv = readRDS(here::here("Data/otters_PAV_nextepv.Rda")) %>%
  select(event_id, next_xPV = next_xV_goals_whistles)

# Load in xG
with_pass_xG = read_csv(here::here("Data/with_pass_xG.csv")) %>% select(event_id, xG = mean)
no_pass_xG = read_csv(here::here("Data/no_pass_xG.csv")) %>% select(event_id, xG = mean)

xGs = rbind(with_pass_xG, no_pass_xG)



# Set to parallel computing
plan(multicore)

# Compute the PAV
otters_posteriors_long = otters_posteriors %>%
  rename(xPV = xV) %>%
  # Add in projected xPV
  merge(next_epv, by = "event_id", all.x = TRUE) %>%
  mutate(next_xPV = ifelse(is.na(next_xPV), 0, next_xPV)) %>%
  # Order by event_id
  arrange(event_id) %>%
  # Merge in xGs
  merge(xGs, by = "event_id", all.x = TRUE) %>%
  # Pivot data to lengthen out our samples
  pivot_longer(col = starts_with("xPV_"), names_prefix = "xPV_", names_to = "xPV_sample", values_to = "xPV") %>%
  # Move important columns to the front
  select(xPV, xG, next_xPV, everything()) %>%
  # Replace NAs with 0s
  mutate(xG = ifelse(is.na(xG), 0, xG)) %>%
  # Format the events to be used in our analysis
  mutate(clean_event = case_when(
    event == "Goal" ~ "Shot",
    event == "Play" ~ "Pass",
    event == "Incomplete Play" ~ "Failed Pass",
    TRUE ~ event
  ))

##########

otters_PAV_1 = otters_posteriors_long %>%
  filter(xPV_sample %in% 1:100) %>%
  # Nest the data
  group_by(entry_id) %>%
  nest() %>%
  # For each entry-to-exit sequence...
  mutate(data = future_map(data, ~.x %>%
                             # Calculate PAV for each event
                             mutate(PAV4 = case_when(
                               (event == "Goal") | (next_event == "Whistle") ~ xG + (1 - xG) * next_xPV - xPV,
                               next_event == "Exit" ~ -xPV,
                               TRUE ~ xG + (1 - xG) * lead(xPV) - xPV
                             ))), .progress = TRUE) %>%
  # Unnest and ungroup
  unnest() %>% ungroup() %>%
  # Move new important columns to the front
  select(clean_event, PAV4, everything())

##########

otters_PAV_2 = otters_posteriors_long %>%
  filter(xPV_sample %in% 101:200) %>%
  # Nest the data
  group_by(entry_id, xPV_sample) %>%
  nest() %>%
  # For each entry-to-exit sequence...
  mutate(data = future_map(data, ~.x %>%
                             # Calculate PAV for each event
                             mutate(PAV = case_when(
                               (event == "Goal") | (next_event == "Whistle") ~ xG + (1 - xG) * next_xPV - xPV,
                               next_event == "Exit" ~ -xPV,
                               TRUE ~ xG + (1 - xG) * lead(xPV) - xPV
                             ))), .progress = TRUE) %>%
  # Unnest and ungroup
  unnest() %>% ungroup() %>%
  # Move new important columns to the front
  select(clean_event, PAV, everything())

########## 2

otters_PAV_3 = otters_posteriors_long %>%
  filter(xPV_sample %in% 201:300) %>%
  # Nest the data
  group_by(entry_id, xPV_sample) %>%
  nest() %>%
  # For each entry-to-exit sequence...
  mutate(data = future_map(data, ~.x %>%
                             # Calculate PAV for each event
                             mutate(PAV = case_when(
                               (event == "Goal") | (next_event == "Whistle") ~ xG + (1 - xG) * next_xPV - xPV,
                               next_event == "Exit" ~ -xPV,
                               TRUE ~ xG + (1 - xG) * lead(xPV) - xPV
                             ))), .progress = TRUE) %>%
  # Unnest and ungroup
  unnest() %>% ungroup() %>%
  # Move new important columns to the front
  select(clean_event, PAV, everything())

##########

otters_PAV_4 = otters_posteriors_long %>%
  filter(xPV_sample %in% 301:400) %>%
  # Nest the data
  group_by(entry_id, xPV_sample) %>%
  nest() %>%
  # For each entry-to-exit sequence...
  mutate(data = future_map(data, ~.x %>%
                             # Calculate PAV for each event
                             mutate(PAV = case_when(
                               (event == "Goal") | (next_event == "Whistle") ~ xG + (1 - xG) * next_xPV - xPV,
                               next_event == "Exit" ~ -xPV,
                               TRUE ~ xG + (1 - xG) * lead(xPV) - xPV
                             ))), .progress = TRUE) %>%
  # Unnest and ungroup
  unnest() %>% ungroup() %>%
  # Move new important columns to the front
  select(clean_event, PAV, everything())

##########

otters_PAV_5 = otters_posteriors_long %>%
  filter(xPV_sample %in% 401:500) %>%
  # Nest the data
  group_by(entry_id, xPV_sample) %>%
  nest() %>%
  # For each entry-to-exit sequence...
  mutate(data = future_map(data, ~.x %>%
                             # Calculate PAV for each event
                             mutate(PAV = case_when(
                               (event == "Goal") | (next_event == "Whistle") ~ xG + (1 - xG) * next_xPV - xPV,
                               next_event == "Exit" ~ -xPV,
                               TRUE ~ xG + (1 - xG) * lead(xPV) - xPV
                             ))), .progress = TRUE) %>%
  # Unnest and ungroup
  unnest() %>% ungroup() %>%
  # Move new important columns to the front
  select(clean_event, PAV, everything())
  
##########


otters_PAV_combo = rbind(otters_PAV_1, otters_PAV_2, otters_PAV_3, otters_PAV_4, otters_PAV_5)


otters_PAV_nested = otters_PAV_combo %>%
  nest(cols = c(xPV_sample, xPV, PAV))

saveRDS(otters_PAV_nested, "Data/otters_PAV_500_samples.Rda")









otters_PAV_wide = otters_PAV_combo %>%
  select(Player, xPV_sample, PAV, entry_team, entry_id) %>%
  filter(abs(PAV) < 1) %>%
  filter(entry_team == "Erie Otters") %>%
  na.omit() %>%
  mutate(xPV_sample = as.character(xPV_sample) %>% as.numeric()) %>%
  group_by(Player, xPV_sample) %>%
  summarize(sum_PAV = sum(PAV)) %>%
  ungroup() %>%
  group_by(Player) %>%
  nest() %>%
  ungroup() %>%
  mutate(cred_int = map(data, ~HPDinterval(as.mcmc(.x$sum_PAV, prob = 0.93)))) %>%
  mutate(mean = map(data, ~mean(.x$sum_PAV))) %>%
  ungroup()


write.csv(otters_PAV_1 %>% select(-sequence), "Data/otters_PAV.csv", row.names = FALSE)
  