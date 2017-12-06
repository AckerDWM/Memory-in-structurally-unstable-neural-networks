setwd("/Users/danielacker/Google\ Drive/memory-amidst-structural-instability/Figure\ 3")

library(dplyr); library(tidyr)
source("methods-SRR.R")

# simulation function
sim = function(n_units, connection_probability, turnover_frac, I_list, n_reentries) {
  
  # Initialization
  synapses = create_synapses(                      # initialize adjacency matrix
    n_units, 
    P=connection_probability)
  weights = create_weights(n_units)                # initialize weight matrix
  n_synapses = sum(sum(synapses))
  n_turnover = floor(turnover_frac*n_synapses)     # number of synapses to replace
  weights = train_net(                             # train net on image I
    I_list=I_list, 
    n_iterations=3, 
    synapses=synapses, 
    weights=weights)
  
  # Reactivations
  results = matrix(NA, n_reentries, n_units)
  for (i in 1:n_reentries) {
    ## Replace synapses
    n_turnover = floor(turnover_frac*sum(sum(synapses)))
    turnover_list = turnover_synapses(synapses, turnover=n_turnover)
    synapses = turnover_list[[1]]
    lost_synapses = turnover_list[[2]]
    new_synapses = turnover_list[[3]]
    weights = turnover_weights(
      weights, lost_synapses, new_synapses, weight_range=c(-1, 1))
    
    Vm = reenter(weights)                          # perform reactivation as defined in text
    firing_rates = tanh(Vm)                        # apply transfer function
    weights = update_weights(firing_rates, weights, synapses)
    results[i,] = c(Vm) # save result
  }
  
  # Collect results
  df = results[c(1, n_reentries),] %>%             # Vm at end of 1st and final reactivation
    as.data.frame() %>%
    mutate(Reentry = c(1, n_reentries)) %>%
    gather(Cell, Vm, -Reentry) %>%
    mutate(                                        # tag result with parameters
      n_units = n_units,
      conn_prob=connection_probability,
      turnover=turnover_frac)
  
  return(df)
}
