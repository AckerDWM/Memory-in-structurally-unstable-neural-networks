library(dplyr); library(tidyr); library(ggplot2); 
library(ggthemes); library(ggsci); library(gridExtra)

source("methods-SRR.R")
source("simulation-function-panels-AtoF.R")

im_path = "training-images/creeper.png"

# Run and evaluate simulations
set.seed(1234)

## Parameters
n_replicates = 100                                  # Number of networks to simulate (constant)
n_reentries = 100                                   # Number of reactivations (constant)
n_units = 100                                       # Number of units per network (constant)
turnover_frac = 0.5                                 # Fraction of synapses replaced per reactivation (constant)
connection_probability = runif(                     # Dendritic filling fraction (varying)
  n_replicates, .1, .8)               

## Run simulations across parameter space
ID = 1
df = sapply(connection_probability, function(conn_prob) {  # Simulate while filling fraction is varying
  I_list = load_memory_image(im_path, size=n_units) %>% 
    c() %>% t() %>% list()
  df_sim = sim(                                     # Simulate one network
    n_units, conn_prob, 
    turnover_frac, I_list, n_reentries)
  df_sim$ID = ID                                    # Tag simulation data with a unique ID
  ID <<- ID + 1
  return(df_sim)
}, simplify=F) %>%
  bind_rows()

## Compute r-squared values comparing firing rate and the image
df_cor = df %>%
  group_by(ID, Reentry, conn_prob) %>%
  do({
    I = load_memory_image(                          # Load image of correct size
      im_path, size=.$n_units[1]) %>% c()
    r_squared = cor(tanh(.$Vm), I)^2                # Calculate r-squared
    if (is.na(r_squared)) r_squared = 0             # If r-squared is NA (invariant firing rate) -> set to 0
    data.frame(r_squared)
  }) %>%
  ungroup() %>%
  mutate(Reentry = factor(Reentry))

# Plot

## Plot r-squared values
g_r_squared_B = ggplot(df_cor, aes(conn_prob, r_squared, color=Reentry, shape=Reentry)) +
  geom_smooth(aes(fill=Reentry), span=0.25, color=NA, method="loess") +
  geom_point() +
  labs(x="Probability of connection", y=expression("r"^"2"),
       color="Reactivation #", fill="Reactivation #", shape="Reactivation #") +
  scale_color_igv() +
  scale_fill_igv() +
  coord_cartesian(ylim=c(0, 1))

## Plot representative firing rates

###
IDs_to_plot = sapply(                                # Choose low, medium, and high unit examples
  c(.1, .5, .8), function(x) df$ID[which.min((df$conn_prob-x)^2)] )
df_rep = df %>%
  filter(ID %in% IDs_to_plot) %>%
  group_by(ID, Reentry, conn_prob) %>%
  do({
    df = .
    n_units = df$n_units[1]
    matrix(                                          # Collect indecies from a blank matrix
      nrow=sqrt(n_units), ncol=sqrt(n_units)) %>% 
      {which(is.na(.), arr.ind=T)} %>%
      data.frame(Vm=df$Vm)
  }) %>%
  ungroup() %>%
  mutate(`Firing rate` = tanh(Vm)) %>%
  mutate(conn_prob = round(conn_prob, 2))

### Generate graphic
g_representative_E = ggplot(df_rep, aes(row, col, fill=`Firing rate`)) +
  facet_grid(Reentry~conn_prob, switch="y") +
  geom_raster() +
  scale_fill_gradient2(low="green3", mid="white", high="magenta3", lim=c(-1,1)) +
  theme(
    axis.text = element_blank(),
    strip.background = element_rect(fill=NA, color=NA),
    axis.line = element_blank(),
    axis.ticks = element_blank()
    ) +
  xlab("Probability of connection") +
  ylab("Reactivation #") +
  scale_x_discrete(position="top") +
  coord_equal()

## Arrange plots
grid.arrange(g_r_squared_B, g_representative_E, nrow=1)
