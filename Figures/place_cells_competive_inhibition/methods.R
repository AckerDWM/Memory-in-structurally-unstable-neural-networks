# load the grid cell firing rate library
load_input = function() {
  wd = getwd()
  setwd("/Users/danielacker/Google Drive/memory-amidst-structural-instability/Figures/grid_cells/grid_cells-2d.npy")
  grid = np$load("grid_cells-2d.npy") %>% t()
  setwd(wd)
  return(grid)
}

initialize_place_cells = function(n_units, n_inputs) {
  # create place cell synapses
  synapses = replicate(n_units, sample(c(rep(1, 1200), rep(0, n_inputs-1200))))
  # create weight matrix
  weights = 
    sample(synaptic_strength_pool, size=n_inputs*n_units, replace=T) %>% 
    matrix(n_inputs, n_units) %>% 
    { synapses * . }
  # create place cells object
  place_cells = list(
    synapses=synapses,
    weights=weights,
    firing_rates=NULL
  )
  place_cells
}

turnover = function(place_cells) {
  synapses = place_cells$synapses
  weights = place_cells$weights
  synapses_turned_over = matrix(0, nrow(synapses), ncol(synapses))
  weights_turned_over = matrix(0, nrow(weights), ncol(weights))
  for (column in 1:ncol(synapses)) {
    # turnover synapses
    synapses_cell = synapses[,column]
    n_turnover = 114 # number of synapses to replace; set appropriately
    lost_synapses = which(synapses_cell == T) %>% sample(n_turnover)
    synapses_cell[lost_synapses] = F
    new_synapses = which(synapses_cell == F) %>% sample(n_turnover)
    synapses_cell[new_synapses] = T
    synapses_turned_over[,column] = synapses_cell
    # turnover weights
    weights_cell = weights[,column]
    weights_cell[lost_synapses] = 0
    weights_cell[new_synapses] = sample(synaptic_strength_pool, size=length(new_synapses), replace=T)
    weights_turned_over[,column] = weights_cell
  }
  place_cells$synapses = synapses_turned_over
  place_cells$weights = weights_turned_over
  place_cells
}

calculate_firing_rates = function(place_cells, X, winner_quantile=.90, dropout_fun="E%-max") {
  # sum inputs
  y = np$dot(X, place_cells$weights)
  # apply competitive inhibition
  y = apply(y, 1, function(y) {
    if (dropout_fun == "E%-max") y = ifelse(y >= max(y)*winner_quantile, y, 0)
    if (dropout_fun == "winner-takes-all") y = ifelse(y >= quantile(y, winner_quantile), y, 0) 
    y
  }) %>% 
    t()
  place_cells$firing_rates = y
  return(place_cells)
}

update_place_cell_weights = function(place_cells, X, learning_rate=3e-8) {
  # update weights by Hebb's rule
  y = place_cells$firing_rates
  dw = np$dot(t(X), y)
  dw = place_cells$synapses * dw
  place_cells$weights = place_cells$weights + learning_rate*dw
  # synaptic scaling
  place_cells$weights = 
    place_cells$weights %>% 
    apply(2, function(x) 149.1*x/sum(x))
  place_cells
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

synaptic_strength_pool = 
  {
    # setting seed here enforces uniformity across all possible simulations
    set.seed(432143)
    
    # empirical probability density function
    P = function(s) {
      A = 100.7
      B = .02
      sigma1 = .022
      sigma2 = .018
      sigma3 = .150
      A*(1-exp(-s/sigma1))*(exp(-s/sigma2)+B*exp(-s/sigma3))
    }
    
    # synaptic size to strength conversion
    W = function(s) (s/.2)*(s/(s+.0314))
    
    # generate a pool of synaptic strengths by rejection sampling
    s = runif(1000000, 0, .2)
    p = runif(1000000, 0, 23)
    synaptic_size_pool = s[p<=P(s)]
    synaptic_strength_pool = W(synaptic_size_pool)
    synaptic_strength_pool
  }
