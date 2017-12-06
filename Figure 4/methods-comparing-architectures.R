library(dplyr); library(tidyr); library(ggplot2); library(cowplot); library(reshape2)
library(ggsci); library(ggthemes); library(reticulate); np = import("numpy")

# create a network object, the primary unit of the simulation
create_network = function(n_units, n_inputs, n_patterns) {
  create_inputs_patterns = function(n_patterns, n_inputs) {
    replicate(n_patterns, runif(n_inputs, -1, 1)) %>% t()
  }
  
  create_synapses = function(n_inputs, n_units) {
    rbinom(n_units*n_inputs, 1, 0.2) %>%
      matrix(n_inputs, n_units)
  }
  
  create_weights = function(n_inputs, n_units, synapses) {
    runif(n_units*n_inputs) %>%
      matrix(n_inputs, n_units) %>% 
      {.*synapses} 
  }
  
  X = create_inputs_patterns(n_patterns, n_inputs)
  synapses = create_synapses(n_inputs, n_units)
  weights = create_weights(n_inputs, n_units, synapses)
  
  net = list(
    X = X,
    synapses = synapses,
    weights = weights,
    y=NULL
  )
  return(net)
}

# Sum inputs and apply winner-takes-all process
activate = function(net, activation_method, winner_quantile=.9) {
  net$y = np$dot(net$X, net$weights)
  
  winner_takes_all = function(y, activation_method, winner_quantile=.9) {
    if (activation_method == "Winner-takes-all") {
      y = apply(y, 1, function(y) {
        ifelse(y > quantile(y, winner_quantile), y, 0) 
      }) %>% 
        t()
    }
    if (activation_method == "E%-max") {
      y = apply(y, 1, function(y) {
        ifelse(y >= max(y)*winner_quantile, y, 0) 
      }) %>% 
        t()
    }
    return(y)
  }
  
  net$y = winner_takes_all(net$y, activation_method, winner_quantile)
  return(net)
}

# update weights as dw = -w + tanh(w + eta*X_transpose*y)
update_weights = function(net, learning_rate=.1) {
  net$weights = tanh(net$weights + np$dot(t(net$X), net$y)*learning_rate)
  net$weights = net$weights * net$synapses
  return(net)
}

# synapse turnover, replace synapses and update weights appropriately
turnover = function(net, turnover=100, weight_range=c(-1, 1)) {
  synapses = net$synapses
  weights = net$weights
  
  # turnover synapses
  lost_synapses = which(synapses == T) %>% sample(turnover)
  synapses[lost_synapses] = F
  new_synapses = which(synapses == F) %>% sample(turnover)
  synapses[new_synapses] = T
  
  # turnover weights
  weights[lost_synapses] = 0
  weights[new_synapses] = runif(length(new_synapses), weight_range[1], weight_range[2])
  
  net$synapses = synapses
  net$weights = weights
  return(net)
}

adjust_activations_interleaving = function(n_activations, n_patterns) {
  n_activations = n_activations * n_patterns
  return(n_activations)
}

adjust_turnover_interleaving = function(turnover_percentage, n_patterns) {
  turnover_percentage = 1 - exp(log(1 - turnover_percentage) / n_patterns)
  return(turnover_percentage)
}

# memory metric - autocorrelation
get_autocorrelation = function(y, y_post) {
  correlations = cor(t(y), t(y_post))
  autocorrelations = diag(correlations)
  return(autocorrelations)
}

# memory metric - uniqueness
get_uniqueness = function(y, y_post) {
  correlations = cor(t(y), t(y_post))
  autocorrelations = diag(correlations)
  diag(correlations) = NA
  maxima = apply(correlations, 2, max, na.rm=T)
  uniqueness = autocorrelations - maxima
  return(uniqueness)
}