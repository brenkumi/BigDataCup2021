library(inlabru)


in_gamma <- readRDS("In_Zone_Gamma_Model.Rda")
n_samples <- 1000


in_gamma_location_sampling <- inla.posterior.sample(n=n_samples, result = in_gamma)

xnames <- rownames(in_gamma_location_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', "shot_count", "is_lead", "is_deficit",
                "time_last_minute", "last_minute", "is_behind_net","is_pass","is_shot","is_recovery","is_turnover","is_shot_shot_time_decay","is_pass_pass_count", 
                'w'), function(nam) ## for each effect
                  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples <- sapply(in_gamma_location_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]], shot_count = spl$latent[idx[[2]]], 
    is_lead = spl$latent[idx[[3]]], is_deficit = spl$latent[idx[[4]]], time_last_minute  = spl$latent[idx[[5]]], 
    last_minute = spl$latent[idx[[6]]], is_behind_net = spl$latent[idx[[7]]],
    is_pass = spl$latent[idx[[8]][[1]]], is_shot = spl$latent[idx[[9]][[1]]], is_recovery = spl$latent[idx[[10]]],
    is_turnover = spl$latent[idx[[11]]], is_shot_shot_time_day = spl$latent[idx[[9]][[2]]], is_pass_pass_count = spl$latent[idx[[8]][[2]]],
    w = spl$latent[idx[[14]]]))


otters_data$is_lead <- ifelse(otters_data$score_effect == "Lead",1,0)
otters_data$is_deficit <- ifelse(otters_data$score_effect =="Deficit",1,0)
otters_data$last_minute <- ifelse(otters_data$seconds > 1140,1,0)
otters_data$time_last_minute <- ifelse(otters_data$last_minute == 1, abs(otters_data$seconds - 1200), 0)
otters_data$is_pass <- ifelse(otters_data$prev_event %in% c("Play", "Incomplete Play"), 1, 0)
otters_data$is_shot <- ifelse(otters_data$prev_event =="Shot",1,0)
otters_data$is_turnover <- ifelse(otters_data$prev_event %in% "Turnover", 1, 0)
otters_data$is_recovery <- ifelse(otters_data$prev_event %in% "Recovery", 1, 0)
otters_data$is_behind_net <- ifelse(otters_data$same_direction_x > 189, 1, 0)
otters_data$shot_time_decay <- ifelse(is.na(otters_data$time_since_last_shot),0, ifelse(otters_data$time_since_last_shot == 0,1, ifelse(otters_data$time_since_last_shot <10, 1 - log10(otters_data$time_since_last_shot),0)))


Mesh <- readRDS("Mesh_in_zone_gamma.Rda")

model_A_matrix2 = inla.spde.make.A(Mesh, loc = cbind(otters_data$same_direction_x, otters_data$same_direction_y)
                                                     , n.group = length(knots), group = otters_data$time_since_entry.x, group.mesh = time_Mesh)


out = as.matrix(cbind(Intercept = 1, shot_count = otters_data$shot_count, is_lead = otters_data$is_lead,
                      is_deficit = otters_data$is_deficit, time_last_minute = otters_data$time_last_minute,
                      last_minute = otters_data$last_minute, is_behind_net = otters_data$is_behind_net,
                      is_pass = otters_data$is_pass, is_shot = otters_data$is_shot, is_turnover = otters_data$is_turnover,
                      is_recovery = otters_data$is_recovery, is_shot_shot_time_day = otters_data$is_shot*otters_data$shot_time_decay,
                      is_pass_pass_count = otters_data$is_pass*otters_data$pass_count, w = model_A_matrix2) %*% mat.samples)

out2 <- apply(out,2,function(x){ exp(x)})

####################################################################################
####################################################################################

### Now the binomial samples

in_binomial <- readRDS("In_Zone_Binomial_Model.Rda")

in_binomial_location_sampling <- inla.posterior.sample(n=n_samples, result = in_binomial)

xnames <- rownames(in_binomial_location_sampling[[1]]$latent) 

idx <- lapply(c('Intercept', "shot_count", "is_lead", "is_deficit",
                "time_last_minute", "last_minute", "is_behind_net","is_pass","is_shot","is_recovery","is_turnover","is_shot_shot_time_decay","is_pass_pass_count", 
                'w'), function(nam) ## for each effect
                  which(substr(xnames, 1, nchar(nam))==nam)) ##

mat.samples_bin <- sapply(in_binomial_location_sampling, function(spl)
  c(Intercept=spl$latent[idx[[1]]], shot_count = spl$latent[idx[[2]]], 
    is_lead = spl$latent[idx[[3]]], is_deficit = spl$latent[idx[[4]]], time_last_minute  = spl$latent[idx[[5]]], 
    last_minute = spl$latent[idx[[6]]], is_behind_net = spl$latent[idx[[7]]],
    is_pass = spl$latent[idx[[8]][[1]]], is_shot = spl$latent[idx[[9]][[1]]], is_recovery = spl$latent[idx[[10]]],
    is_turnover = spl$latent[idx[[11]]], is_shot_shot_time_day = spl$latent[idx[[9]][[2]]], is_pass_pass_count = spl$latent[idx[[8]][[2]]],
    w = spl$latent[idx[[14]]]))




otters_data$is_lead <- ifelse(otters_data$score_effect == "Lead",1,0)
otters_data$is_deficit <- ifelse(otters_data$score_effect =="Deficit",1,0)
otters_data$last_minute <- ifelse(otters_data$seconds > 1140,1,0)
otters_data$time_last_minute <- ifelse(otters_data$last_minute == 1, abs(otters_data$seconds - 1200), 0)
otters_data$is_pass <- ifelse(otters_data$prev_event %in% c("Play", "Incomplete Play"), 1, 0)
otters_data$is_shot <- ifelse(otters_data$prev_event =="Shot",1,0)
otters_data$is_turnover <- ifelse(otters_data$prev_event %in% "Turnover", 1, 0)
otters_data$is_recovery <- ifelse(otters_data$prev_event %in% "Recovery", 1, 0)
otters_data$is_behind_net <- ifelse(otters_data$same_direction_x > 189, 1, 0)
otters_data$shot_time_decay <- ifelse(is.na(otters_data$time_since_last_shot),0, ifelse(otters_data$time_since_last_shot == 0,1, ifelse(otters_data$time_since_last_shot <10, 1 - log10(otters_data$time_since_last_shot),0)))


Mesh <- readRDS("Mesh_inbinomial.Rda")

model_A_matrix_bin = inla.spde.make.A(Mesh, loc = cbind(otters_data$same_direction_x, otters_data$same_direction_y), n.group = length(knots), group = otters_data$time_since_entry.x, group.mesh = time_Mesh)


out_bin = as.matrix(cbind(Intercept = 1, shot_count = otters_data$shot_count, is_lead = otters_data$is_lead,
                      is_deficit = otters_data$is_deficit, time_last_minute = otters_data$time_last_minute,
                      last_minute = otters_data$last_minute, is_behind_net = otters_data$is_behind_net,
                      is_pass = otters_data$is_pass, is_shot = otters_data$is_shot, is_turnover = otters_data$is_turnover,
                      is_recovery = otters_data$is_recovery, is_shot_shot_time_day = otters_data$is_shot*otters_data$shot_time_decay,
                      is_pass_pass_count = otters_data$is_pass*otters_data$pass_count, w = model_A_matrix_bin) %*% mat.samples_bin)

out_bin2 <- apply(out_bin,2,function(x){ 1/(1 +exp(x))})



try <- out_bin2*out2


saveRDS(try,"posterior_ex_samples_1000.Rda")


library(coda)

HPDinterval(as.mcmc(try[1,]), prob = 0.91)



