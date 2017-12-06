library(dplyr); library(imager); library(matlab); library(reshape2); 
np = reticulate::import("numpy")

load_memory_image = function(fname, size=100) {
  im = png::readPNG(fname)
  im_arr = array(NA, dim=c(dim(im)[1], dim(im)[2], 1, 1))
  im_arr[,,1,1] = im[,,1]
  im = resize(im_arr, size_x=sqrt(size), size_y=sqrt(size), size_z=1, size_c=1)
  im = im[,,1,1] %>% rot90() %>% rot90() %>% rot90()
  im[im<=0] = -1
  im[im>0] = 1
  return(im)
}

turnover_synapses = function(synapses, turnover=100) {
  lost_synapses = which(synapses == T) %>% sample(turnover)
  synapses[lost_synapses] = F
  new_synapses = which(synapses == F) %>% sample(turnover)
  synapses[new_synapses] = T
  return( list(synapses, lost_synapses, new_synapses) )
}

turnover_weights = function(weights, lost_synapses, new_synapses, weight_range=c(-1, 1)) {
  weights[lost_synapses] = 0
  weights[new_synapses] = runif(length(new_synapses), weight_range[1], weight_range[2])
  return(weights)
}


create_synapses = function(n_units, P) replicate(n_units, rbinom(n_units, 1, P)) == 1

create_weights = function(n_units) matrix(0, n_units, n_units)

train_net = function(I_list, n_iterations, synapses, weights, eta=1, tau=1) {
  n_units = nrow(synapses)
  Vm = runif(n_units, min=-1, max=1)
  for (i in 1:(n_iterations)) {
    for (I in I_list) {
      for (j in 1:12) {
        firing_rates = tanh(Vm)
        delta_Vm = c(-Vm + firing_rates %*% weights + I*80)
        Vm = Vm + delta_Vm
        delta_weights = ( -1 * weights + eta * np$dot(t(t(firing_rates)), t(firing_rates)) ) / tau
        delta_weights[synapses == F] = 0
        weights = weights + delta_weights
      }
    }
  }
  return(weights)
}

reenter = function(weights, I=NULL) {
  n_units = nrow(weights)
  Vm = runif(n_units, min=-0.004, max=0.004)
  if (!is.null(I)) Vm = c(I)
  for (i in 1:20) {
    firing_rates = tanh(Vm)
    delta_Vm = c(-Vm + np$dot(firing_rates, weights))
    Vm = Vm + delta_Vm
  }
  return(Vm)
}

reenter_2 = function(weights, I=NULL) {
  n_units = nrow(weights)
  
  rate_matrix = matrix(nrow=12, ncol=n_units)
  
  Vm = runif(n_units, min=-0.004, max=0.004)
  if (!is.null(I)) Vm = c(I)
  for (i in 1:12) {
    firing_rates = tanh(Vm)
    delta_Vm = c(-Vm + np$dot(firing_rates, weights))
    Vm = Vm + delta_Vm
    # save membrane potentials
    rate_matrix[i,] = Vm
  }
  return(rate_matrix)
}

update_weights = function(firing_rates, weights, synapses, eta=1, tau=1) {
  delta_weights = (-1 * weights + eta * np$dot(t(t(firing_rates)), t(firing_rates))) / tau
  delta_weights[synapses == F] = 0
  weights = weights + delta_weights
  return(weights)
}