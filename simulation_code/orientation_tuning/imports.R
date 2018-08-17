library(magrittr);library(plyr);library(dplyr);library(tidyr);library(reshape2)
library(ggplot2);library(ggsci);library(ggforce);library(cowplot);library(plot3D);library(plot3D)
library(scales);library(zoo); library(extrafont)
np = reticulate::import("numpy")

theme_set(
  theme_classic() +
    theme(
      text=element_text(size=8, family="Times New Roman"),
      legend.background=element_rect(fill=NA),
      legend.margin = margin(0, 0, 0, 0),
      axis.line = element_line(size=.25),
      axis.ticks = element_blank(),
      axis.text.y = element_text(margin = margin(r = -1)),
      axis.text.x = element_text(margin = margin(t = -1)),
      strip.background = element_rect(fill=NA, color=NA)
    )
)
