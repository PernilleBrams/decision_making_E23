
# Model -- w. fatique and increased concentration (memory boost) after wrong guess
n_trials <- 500
G_learn <- array(NA, c(n_trials)) # success = 1, fail = 0
skill_level <- array(NA, c(n_trials))
theta_1 <- 0.5
alpha <- 0.01

fatigue <- array(NA, c(n_trials))
guess_prob <- array(NA, c(n_trials))

# Memory Effect parameters
memory_boost <- 0.05 # how much to boost theta_learn after a wrong guess
boost_duration <- 8 # how many trials the boost lasts for
trials_since_mistake <- Inf # track how many trials since last mistake

# First trial
skill_level[1] <- theta_1
fatigue[1] <- 0.01
guess_prob[1] <- skill_level[1]*(1-fatigue[1]) # will practically be .5
guess_prob[1] <- max(0, min(1, guess_prob[1])) # between 1 and 0
G_learn[1] <- rbinom(1,1,guess_prob[1])

# If a mistake is made on the first trial
if(G_learn[1] == 0) trials_since_mistake <- 1

for(t in 2:n_trials){
  skill_level[t] <- skill_level[t-1]^(1/(1+alpha))
  fatigue[t] <- 0 + (0.4/(1+exp(-0.04*(t-250)))^(1/1))
  
  guess_prob[t] <- skill_level[t]*(1-fatigue[t])
  
  # apply the memory effect if within the boost_duration since last mistake
  if(trials_since_mistake <= boost_duration){
    guess_prob[t] <- guess_prob[t] + memory_boost
  }
  
  # Ensure that guess prob between 0 and 1
  guess_prob[t] <- max(0, min(1, guess_prob[t]))
  
  G_learn[t] <- rbinom(1,1,guess_prob[t])
  
  # if a mistake is made, reset trials_since_mistake
  if(!is.na(G_learn[t]) && G_learn[t] == 0){
    
    trials_since_mistake <- 1
  } else {
    trials_since_mistake <- trials_since_mistake + 1
  }
}

# Plotting
colors <- ifelse(G_learn == 1, "red", "blue")
plot(skill_level, col = "forestgreen")+title("Skill level")
plot(guess_prob, col = colors)+title("Success = red, Fail = blue")

# plot(G_learn)
