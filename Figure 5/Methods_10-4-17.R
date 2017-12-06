library(plyr); library(dplyr); library(tidyr); library(ggplot2);
library(ggthemes); library(imager); library(reshape2); library(cowplot);
library(latex2exp); library(ggsci)
np = reticulate::import("numpy")

# load the grid cell firing rate library
load_input = function() {
  wd = getwd()
  setwd("/Users/danielacker/Google Drive/Computational_Project/place-field-synapse-turnover")
  grid = np$load("grid_cells-2d.npy") %>% t()
  setwd(wd)
  return(grid)
}

# create a place cell object
initialize_place_cells = function(n_units, n_inputs, initial_weight_value) {
  # create place cell synapses
  synapses = replicate( 
    n_units, 
    sample(
      c(
        rep(1, 1200), 
        rep(0, n_inputs-1200)
      )
    )
  )
  
  # create weight matrix
  weights = rep(initial_weight_value, n_inputs*n_units) %>%
    matrix(n_inputs, n_units) %>% 
    { synapses * . } # set weights to zero where there is no synapse
  
  # create place cells object
  place_cells = list(
    synapses=synapses,
    weights=weights,
    firing_rates=NULL
  )
}

# calculates place cell firing rates
calculate_firing_rates = function(place_cells, X, winner_quantile=.92) {
  y = np$dot(X, place_cells$weights) # sum inputs
  y = apply(y, 1, function(y) { # apply E%-max winner-takes-all
    ifelse(y >= max(y)*winner_quantile, y, 0) 
  }) %>% 
    t()
  place_cells$firing_rates = y
  return(place_cells)
}

# calculate change in weight
update_place_cell_weights = function(place_cells, X, learning_rule="LTP and LTD") {
  y = place_cells$firing_rates
  learning_rate = 2e-5
  
  X_minus_cell_means = apply(X, 2, function(x) x - mean(x))
  y_minus_cell_means = apply(y, 2, function(x) x - mean(x)) ##
  X_less = X_minus_cell_means*(X_minus_cell_means < 0)
  y_less = y_minus_cell_means*(y_minus_cell_means < 0)
  dw = np$dot(t(X_minus_cell_means), y_minus_cell_means) - 2 * np$dot(t(X_less), y_less) ##
  dw = dw*learning_rate
  if (learning_rule == "LTP-only") dw[dw < 0] = 0
  if (learning_rule == "LTD-only") dw[dw > 0] = 0
  if (learning_rule == "No learning") dw = 0
  dw = place_cells$synapses * dw
  place_cells$weights = place_cells$weights + dw
  place_cells$weights = place_cells$weights %>% 
    { .[. < 0] = 0; . } %>%
    { .[. > 2/1200] = 2/1200; . }
  return(place_cells)
}

# turnover 114/1200 synapses per cells
turnover = function(place_cells) {
  synapses = place_cells$synapses
  weights = place_cells$weights
  synapses_turned_over = matrix(0, nrow(synapses), ncol(synapses))
  weights_turned_over = matrix(0, nrow(weights), ncol(weights))
  for (column in 1:ncol(synapses)) {
    # turnover synapses
    synapses_cell = synapses[,column]
    n_turnover = 114 # set appropriately
    lost_synapses = which(synapses_cell == T) %>% sample(n_turnover)
    synapses_cell[lost_synapses] = F
    new_synapses = which(synapses_cell == F) %>% sample(n_turnover)
    synapses_cell[new_synapses] = T
    synapses_turned_over[,column] = synapses_cell
    
    # turnover weights
    weights_cell = weights[,column]
    weights_cell[lost_synapses] = 0
    weights_cell[new_synapses] = 1/1200
    weights_turned_over[,column] = weights_cell
  }
  place_cells$synapses = synapses_turned_over
  place_cells$weights = weights_turned_over
  return(place_cells)
}

# find place cells with one field wider than 5 px
get_centroids = function(response, min_field_width=5) {
  centroids = apply(response, 2, function(response_cell) {
    maximum = max(response_cell)
    runs = rle(response_cell > maximum*0.8)
    long_runs = (runs$lengths > min_field_width) & (runs$values == 1)
    if (sum(long_runs) == 1) {
      centroid = cumsum(runs$lengths)[long_runs] - runs$lengths[long_runs]/2
      return(centroid)
    }
    return(NA)
  })
  return(centroids)
}
