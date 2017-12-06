library(dplyr); library(tidyr); library(igraph); 
library(ggplot2); library(ggthemes); library(ggalt); 
library(matlab); library(ggsci)

source("methods-SRR.R")

im_path = "training-images/creeper.png"
I_list = load_memory_image(im_path, size=n_units) %>% 
  c() %>% t() %>% list()

set.seed(1234)

# simulate an attractor network and save the membrane potentials at each timestep
sim = function(turnover_frac) {
  # parameters
  n_units = 100
  connection_probability = .2
  n_reentries = 100
  
  # initialization
  synapses = create_synapses(n_units, P=connection_probability)
  weights = create_weights(n_units)
  # parameters for turnover
  n_synapses = sum(sum(synapses))
  n_turnover = floor(turnover_frac*n_synapses)
  # training
  weights = train_net(I_list=I_list, n_iterations=3, synapses=synapses, weights=weights)
  # reactivations
  results = list()
  for (i in 1:n_reentries) {
    ## Replace synapses
    n_turnover = floor(turnover_frac*sum(sum(synapses)))
    turnover_list = turnover_synapses(synapses, turnover=n_turnover)
    synapses = turnover_list[[1]]
    lost_synapses = turnover_list[[2]]
    new_synapses = turnover_list[[3]]
    weights = turnover_weights(
      weights, lost_synapses, new_synapses, weight_range=c(-1, 1))
    # network simulation
    rate_matrix = reenter_2(weights)
    Vm = rate_matrix[12,]
    # update weights
    firing_rates = tanh(Vm)
    weights = update_weights(firing_rates, weights, synapses)
    # save
    results[[i]] = rate_matrix
  }
  
  # melt
  idx = 1
  df = lapply(results, function(item) {
    sapply(1:12, function(row) {
      df = item[row,] %>%
        matrix() %>%
        t() %>%
        melt() %>%
        mutate(time=idx)
      idx <<- idx + 1
      df
    }, simplify=F) %>%
      bind_rows()
  }) %>%
    bind_rows()
  
  # assign image indcies
  assign_idx = function(df, n_units) {
    df = ddply(df, c("time"), function(df) {
      idx = matrix(NA, nrow=sqrt(n_units), ncol=sqrt(n_units)) %>% 
      {which(is.na(.), arr.ind=T)}
      df = mutate(df, Row=idx[,1], Col=idx[,2])
      return(df)
    })
    return(df)
  }
  
  df = assign_idx(df, n_units)
  return(df)
}

df_0 = sim(0)
df_10 = sim(.1)
df_90 = sim(.8)

df_0$turnover = "None"
df_10$turnover = "Low"
df_90$turnover = "High"
df = bind_rows(df_0, df_10, df_90)

df_deriv = df %>%
  mutate(reactivation = floor((time-1)/12) ) %>%
  filter(time<=60) %>%
  ddply(c("Col", "Row", "turnover", "reactivation"), function(df) {
    df %>%
      arrange(time) %>%
      mutate(dv = c(NA, diff(value)))
  })

g_dVm = 
  subset(df_deriv, time<=60) %>%
  mutate(
    turnover=recode(
      turnover, 
      "High"="\nUnstable", 
      "Low"="\nStable", 
      "None"="Zero\nturnover")
    ) %>%
  ggplot(aes(time, dv, group=interaction(Var1, Var2, turnover))) +
  facet_grid(turnover~.) +
  geom_line(alpha=0.2) +
  scale_y_continuous(breaks=c(-15, 0, 15)) +
  annotate("point", x=c(1,13,25,37,49), y=rep(5, 5), shape=25, fill="black") + 
  theme(legend.position="none") +
  labs(color="Turnover rate") +
  xlab("Time step") +
  ylab(expression("dV"["m"]))
g_dVm
