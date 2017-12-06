library(dplyr)

# generate random cosine series
make_cosines = function(n_cosines) {
  x = seq(0, 2*pi, length=100)
  
  period = runif(n_cosines, min=pi/4, max=2*pi)
  phase = runif(n_cosines, min=0, max=2*pi)
  
  B = 2*pi / period
  C = phase * -B
  
  cosines = mapply(function(B, C) sin(B*x+C), B=period, C=phase)
  return(cosines)
}

# run the simulation
simulate = function(n_cosines, n_outputs, turnover, eta, pretraining=T, post_training=T) {
  # Initialize inputs and synaptic weights
  input = make_cosines(n_cosines=n_cosines)
  weights = replicate(n_outputs, runif(n_cosines, -1, 1))
  replacement_input = make_cosines(n_cosines=turnover)
  
  # Activate the output units pre-turnover
  response = input %*% weights
  
  # Pre-training
  if (pretraining) {
    dw = eta * t(input) %*% response
    weights = weights + dw # update weights
  }
  
  # Save weights and inputs
  inputs_pre_turnover = input
  weights_pre_turnover = weights
  
  # Synpase turnover
  input[,1:turnover] = replacement_input
  weights[1:turnover,] = runif(turnover*n_outputs, -1, 1) # reset replaced weights
  
  # Activate the output units post-turnover
  response = input %*% weights
  
  # Post-training
  if (post_training) {
    dw = eta * t(input) %*% response
    weights = weights + dw
  }
  
  # Sum synaptic potentials across replaced inputs
  PSP_pre_turnover = inputs_pre_turnover[,1:turnover] %*% weights_pre_turnover[1:turnover,]
  PSP_post_turnover = input[,1:turnover] %*% weights[1:turnover,]
  
  # Get correlation between PSPs pre and post-turnover
  correlation = cor(PSP_pre_turnover, PSP_post_turnover)
  return(correlation)
}