library(magrittr);library(plyr);library(dplyr);library(tidyr);library(reshape2)
library(ggplot2);library(ggsci);library(ggforce);library(cowplot);library(plot3D);library(plot3D)
library(scales);library(zoo); library(extrafont)
np = reticulate::import("numpy")

theme_set(
  theme_classic() +
    theme(
      text = element_text(size=8),
      strip.background = element_blank(),
      legend.margin = margin(0, 0, 0, 0)
      )
  )
