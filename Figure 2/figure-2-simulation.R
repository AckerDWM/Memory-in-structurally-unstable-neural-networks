library(plyr); library(dplyr);library(tidyr);
library(ggplot2);library(ggthemes);library(ggsci)

source("figure-2-methods.R")

set.seed(1234)

# Parameters
n_cosines = 100 # number of input units
n_outputs = 1 # number of output units
turnover = 50 # number of inputs replaced during turnover
eta = 2e-2 # learning rate
n_replicates = 500

# Run simulation
pre_post = replicate(
  n_replicates, 
  simulate(n_cosines, n_outputs, turnover, eta, pretraining=T, post_training=T))
pre = replicate(
  n_replicates, 
  simulate(n_cosines, n_outputs, turnover, eta, pretraining=T, post_training=F))
post = replicate(
  n_replicates, 
  simulate(n_cosines, n_outputs, turnover, eta, pretraining=F, post_training=T))
none = replicate(
  n_replicates, 
  simulate(n_cosines, n_outputs, turnover, eta, pretraining=F, post_training=F))

# Collect the simulation data
df = data.frame(pre_post, pre, post, none) %>% 
  gather(Condition, Correlation) %>%
  mutate(Condition=revalue(Condition, replace=c(
    "pre_post"="Pre & post-training",
    "pre"="Pre-training only",
    "post"="Post-training only",
    "none"="No training"
  ))) %>%
  mutate(Condition=factor(Condition, levels=c(
    "Pre & post-training", "Pre-training only",
    "Post-training only", "No training"
  )))

# Plot results
figure = ggplot(df, aes(Correlation, color=Condition)) +
  geom_step(stat="ecdf") +
  # Themes
  theme_classic() +
  theme(
    legend.position=c(.22, .8),
    legend.title = element_blank(),
    text=element_text(size=8)
  ) +
  guides(color=guide_legend(keyheight=.5, keywidth=.5)) +
  # Colors
  scale_color_brewer(palette="Dark2") +
  # Annotations
  geom_hline(yintercept=.5, linetype=2) +
  geom_vline(xintercept=0, linetype=2) +
  # Axis labels
  xlab(expression("r(PSPs"["new"]*",PSPs"["lost"]*")")) +
  ylab("Cumulative density")

figure

tiff("fig-2.tiff", width=3.09, height=2, units="in", res=300)
figure
dev.off()
