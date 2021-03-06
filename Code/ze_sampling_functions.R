



## STEP 1: CREATE DATA FRAME ##
otters_data = read_csv(here::here("Data/otters_data_for_modelling.csv")) %>%
  filter(entry_strength == "ES") %>%
  mutate(time_since_entry.x = time_since_entry) %>%
  filter(event == "Zone Entry") %>%
  mutate(is_near_boards = ifelse(same_direction_y <= 10 | same_direction_y >= 75, 1, 0)) %>%
  mutate(is_lead = ifelse(score_effect == "Lead", 1, 0)) %>%
  mutate(is_deficit = ifelse(score_effect == "Deficit", 1, 0)) %>%
  mutate(last_minute = ifelse(seconds > 1140, 1, 0)) %>%
  mutate(time_last_minute = ifelse(last_minute == 1, abs(seconds - 1200), 0)) %>%
  mutate(is_behind_net = ifelse(same_direction_x > 189, 1, 0)) %>%
  mutate(shot_time_decay = case_when(
    # 0 seconds since last shot --> 1
    time_since_last_shot == 0 ~ 1,
    # 1-9 seconds since last shot --> 1 - log10(seconds)
    time_since_last_shot < 10 ~ 1 - log10(time_since_last_shot),
    # 10+ seconds since last shot --> 0
    time_since_last_shot >= 10 ~ 0,
    TRUE ~ 0
  ))


## STEP 2: SAMPLE FROM GAMMA ##

ze_gamma_location_sampling <- inla.posterior.sample(n=1000, result = ze_gamma_model)

xnames <- rownames(ze_gamma_location_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', "is_near_boards","is_lead", "is_deficit", "time_last_minute", "last_minute", "i"), function(nam) ## for each effect
                  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples <- sapply(ze_gamma_location_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]], is_near_boards = spl$latent[idx[[2]]], 
    is_lead = spl$latent[idx[[3]]], is_deficit = spl$latent[idx[[4]]], time_last_minute  = spl$latent[idx[[5]]], 
    last_minute = spl$latent[idx[[6]]],
    i = spl$latent[idx[[7]]][1:86]))

Mesh = inla.mesh.1d(0:85)

model_A_matrix2 = inla.mesh.1d.A(Mesh, loc = c(otters_data$same_direction_y))


out = as.matrix(cbind(Intercept = 1, is_near_boards = otters_data$is_near_boards, is_lead = otters_data$is_lead,
                      is_deficit = otters_data$is_deficit, time_last_minute = otters_data$time_last_minute,
                      last_minute = otters_data$last_minute, i = model_A_matrix2) %*% mat.samples)

out2 = apply(out, 2, function(x){exp(x)})


## STEP 2: SAMPLE FROM BINOMIAL ##

ze_gamma_location_sampling <- inla.posterior.sample(n=1000, result = ze_binom_model)

xnames <- rownames(ze_gamma_location_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', "is_near_boards","is_lead", "is_deficit", "time_last_minute", "last_minute", "i"), function(nam) ## for each effect
  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples <- sapply(ze_gamma_location_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]], is_near_boards = spl$latent[idx[[2]]], 
    is_lead = spl$latent[idx[[3]]], is_deficit = spl$latent[idx[[4]]], time_last_minute  = spl$latent[idx[[5]]], 
    last_minute = spl$latent[idx[[6]]],
    i = spl$latent[idx[[7]]][1:86]))

Mesh = inla.mesh.1d(0:85)

model_A_matrix2 = inla.mesh.1d.A(Mesh, loc = c(otters_data$same_direction_y))


out = as.matrix(cbind(Intercept = 1, is_near_boards = otters_data$is_near_boards, is_lead = otters_data$is_lead,
                      is_deficit = otters_data$is_deficit, time_last_minute = otters_data$time_last_minute,
                      last_minute = otters_data$last_minute, i = model_A_matrix2) %*% mat.samples)

out_binom = apply(out,2,function(x){ 1/(1 +exp(x))})


## STEP 3: COMBINE ##

out_combo = out2 *out_binom

saveRDS(out_combo, "entry_samples.Rda")

a = apply(out_combo, 2, function(x) {sum(x>1)})
