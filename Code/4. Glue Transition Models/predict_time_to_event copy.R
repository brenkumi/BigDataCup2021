#################################################
#################################################

### Predict the time passed

# Load packages
library(INLA) # To run INLA
library(ggregplot) # Custom functions for visualizing INLA
library(tidyverse) # For various data science functions
library(RColorBrewer) # For colour palettes
library(plyr) # For mapvalues() function
#############

otter <- read.csv("otters_data_For_modelling2.csv")


otter$no_previous <- ifelse(is.na(otter$prev_event)==TRUE,1,0)


otter$recovery <- ifelse(otter$event =="Recovery",1,0)
otter$shot <- ifelse(otter$event =="Shot",1,0)
otter$turnover <- ifelse(otter$event =="Turnover",1,0)
otter$pass <- ifelse(otter$event =="Play",1,0)



otter$inclusion <- otter$recover + otter$shot + otter$turnover + otter$pass

otter$prev_x <- NA
otter$prev_y <- NA

otter$prev_time <- NA


for(i in 1:nrow(otter)){
  otter$prev_x[i] <- ifelse(otter$no_previous[i]==0,otter$same_direction_x[i-1],NA)
  otter$prev_y[i] <- ifelse(otter$no_previous[i]==0,otter$same_direction_y[i-1],NA)
  otter$prev_time[i] <- ifelse(otter$no_previous[i] ==0, otter$seconds[i-1], NA)
}

otter$prev_pass <- ifelse(otter$prev_event == "Play",1,0)
otter$prev_direct_pass <- ifelse((otter$prev_event == "Play")&(otter$detail_1 == "Direct"),1,0)
otter$prev_indirect_pass <- ifelse((otter$prev_event == "Play")&(otter$detail_1 == "Indirect"),1,0)


otter$prev_recovery <- ifelse(otter$prev_event =="Recovery",1,0)
otter$prev_zone_entry <- ifelse(otter$prev_event == "Zone Entry",1,0)
otter$prev_shot <- ifelse(otter$prev_event == "Shot",1,0)
otter$prev_turnover <- ifelse(otter$prev_event == "Turnover",1,0)

otter$time_since_prev_event <- otter$seconds - otter$prev_time

otter$distance_last_event <- sqrt((otter$same_direction_x - otter$prev_x)^2 + (otter$same_direction_y - otter$prev_y)^2)

otter$passdir_int_distance <- otter$distance_last_event*otter$prev_direct_pass




##############################################################################
##############################################################################



otter_sub <- otter[otter$inclusion ==1,]

otter_sub$type_index <- mapvalues(otter_sub$event,from = unique(otter_sub$event), to =c(1:length(unique(otter_sub$event))))


summary(otter_sub$time_since_prev_event)


ES_time <- otter_sub[otter_sub$strength_state =="ES",]



model_coords <- cbind(ES_time$same_direction_x, ES_time$same_direction_y)


xrange = c(min(model_coords[,1]), max(model_coords[,1]))
yrange = c(min(model_coords[,2]), max(model_coords[,2]))

model_domain <- matrix(c(xrange[1], yrange[1],
                         xrange[2], yrange[1],
                         xrange[2], yrange[2],
                         xrange[1], yrange[2],
                         xrange[1], yrange[1]),
                       ncol = 2L, byrow = TRUE)




Mesh <- inla.mesh.2d(model_coords, max.edge = c(20,20), offset = 40, cutoff = 20, boundary = model_domain) # Set for computation ease rather than because it is a great mesh, fairly coarse

plot(Mesh)
Hosts.spde = inla.spde2.pcmatern(mesh = Mesh, prior.range = c(5, 0.8), prior.sigma = c(.5, .5))


NGroups <- 4

HostA2 <- inla.spde.make.A(Mesh, # Leave
                           loc = model_coords, # Leave
                           group = as.numeric(ES_time$type_index),# this must be a numeric value counting from 1. If the groups variable is a factor, this will happen by default.
                           n.group = NGroups) 

w.Host2 <- inla.spde.make.index(
  name    = 'w', 
  n.spde  = Hosts.spde$n.spde,
  n.group = NGroups)  



StackHost2 <- inla.stack(
  data = list(time = ES_time$time_since_prev_event), # specify the response variable
  
  
  A = list(1,1,1,1,1,1,1,1,1,1,1,1,HostA2), # Vector of Multiplication factors for random and fixed effects              
  
  effects = list(
    
    Intercept = rep(1, nrow(ES_time)), # specify the manual intercept!
    
    distance_last_event = ES_time$distance_last_event,
    
    prev_direct_pass= ES_time$prev_direct_pass,
    
    prev_indirect_pass = ES_time$prev_indirect_pass,
    
    prev_turnover = ES_time$prev_turnover,
    
    prev_recovery = ES_time$prev_recovery,
    
    prev_shot = ES_time$prev_shot,
    
    
    int_direct_dist = ES_time$distance_last_event*ES_time$prev_direct_pass,
    
    int_indirect_dist = ES_time$distance_last_event*ES_time$prev_indirect_pass,
    
    int_turnover_dist = ES_time$distance_last_event*ES_time$prev_turnover,
    
    int_recovery_dist = ES_time$distance_last_event*ES_time$prev_recovery,
    
    int_shot_dist = ES_time$distance_last_event*ES_time$prev_shot,
    # X = X, # attach the model matrix
    
    # ID = TestHosts$ID, # insert vectors of any random effects
    
    w = w.Host2)) 


f5 = as.formula(paste0("time ~ -1 + Intercept  + prev_indirect_pass + 
                       prev_turnover + prev_recovery + prev_shot + distance_last_event + int_direct_dist +
                       int_indirect_dist + int_turnover_dist + int_recovery_dist + int_shot_dist +", 
                       "f(w, model = Hosts.spde, 
group = w.group,                           # This bit is new! 
control.group = list(model = 'iid'))"))

Predict_time_to_next_event <- inla(f5,
                           family= "poisson",
                           data = inla.stack.data(StackHost2),
                           control.compute = list(dic = TRUE, config = TRUE),
                           control.predictor = list(A = inla.stack.A(StackHost2))
)

ggField(Predict_time_to_next_event, Mesh, Groups = 4) + 
  scale_fill_brewer(palette = "Greens") 

summary(Predict_time_to_next_event)

