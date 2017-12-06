library(dplyr); library(tidyr); library(igraph); 
library(ggplot2); library(ggthemes); library(ggalt); 
library(matlab); library(ggsci)

source("methods-SRR.R")

# simulation function
simulate = function(n_units, connection_probability, turnover_frac, I_list, n_reentries) {
  # initialization
  synapses = create_synapses(n_units, P=connection_probability)
  weights = create_weights(n_units)
  indegree = synapses %>%
    graph.adjacency(mode="directed") %>%
    degree(mode="in") %>% 
    median()
  # parameters for turnover
  n_synapses = sum(sum(synapses))
  n_turnover = floor(turnover_frac*n_synapses)
  # training
  weights = train_net(I_list=I_list, n_iterations=3, synapses=synapses, weights=weights)
  # reentries
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
    
    # activation
    Vm = reenter(weights)
    firing_rates = tanh(Vm)
    weights = update_weights(firing_rates, weights, synapses)
    # save
    results[i,] = c(Vm)
  }
  # collect output
  df = data.frame(
    unit=1:n_units,
    Vm=results[n_reentries,] %>% c(),
    indegree=indegree,
    turnover=turnover_frac,
    n_units=n_units)
  return(df)
}

# Run
set.seed(1234)

## constants
perfect_squares = (4:32)^2
image_path = "training-images/creeper.png"

## constant parameters
n_reentries = 100
n_samples = 100

## varying parameters
parameter_df = data.frame(
  turnover_fracs = runif(n_samples, 0, 1),
  connection_probabilities = runif(n_samples, min=0.1, max=0.8),
  n_units_vec = sample(perfect_squares, n_samples, replace=T))

## simulation execution
ID = 1
df_sim = apply(parameter_df, 1, function(params) {
  # load training image
  I = load_memory_image(image_path, size=params[3]) %>% c() %>% t()
  # run simulation
  df = simulate(
    n_units=params[3], 
    connection_probability=params[2], 
    turnover_frac=params[1], 
    list(I), n_reentries)
  # increment the ID variable
  df$ID = ID
  ID <<- ID + 1
  return(df)
}) %>%
  bind_rows()

## Find r-squared
df_cor = df_sim %>%
  ddply(c("ID", "indegree", "turnover"), function(df) {
    I = load_memory_image(image_path, size=df$n_units[1]) %>% 
      c() %>% 
      t()
    r_squared = cor(tanh(df$Vm), c(I))^2
    return(r_squared)
  }) %>%
  mutate(r_squared = V1 %>% {.[is.na(.)]=0;.})

# Plot
g_indegree = ggplot(df_cor, aes(indegree, turnover, color=r_squared)) +
  geom_point() +
  # geom_encircle(data=subset(df_cor, r_squared<0.81), 
  #               expand=0, s_shape=1, color="black") +
  scale_color_distiller(palette="Spectral") +
  scale_x_sqrt(breaks=c(10, 100, 500)) +
  xlab("In-degree") +
  ylab("Turnover fraction") +
  labs(color=expression("r"^"2"))
g_indegree
