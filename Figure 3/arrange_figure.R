library(cowplot); library(ggthemes)

fillcol = scale_fill_gradient2(
  low="wheat3", mid="white", high="grey20", lim=c(-1,1))

im_path = "training-images/creeper.png"
df_creeper = load_memory_image(im_path, size=n_units) %>% melt()
g_creeper = ggplot(df_creeper, aes(Var1, Var2, fill=value)) +
  geom_raster() +
  fillcol +
  theme_tufte() +
  theme(axis.text = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(text = element_text(family="sans", size=8)) +
  theme(legend.position = "none") +
  coord_equal() +
  ggtitle("Input") +
  theme(plot.title = element_text(hjust = .5, vjust=-1, size=8))

theme_set(
  theme_classic() +
    theme(
      text=element_text(family="sans", size=8),
      strip.background = element_rect(fill=NA, color=NA)
      )
  )

legend = get_legend(g_r_squared_A) # legend for A-C
legend_rep = get_legend(g_representative_D + fillcol) # legend for D-F
no_legend = theme(legend.position = "none")

row_height = .33

figure = ggdraw() +
  # top row
  draw_plot(g_r_squared_A+no_legend+axline, x=0, y=row_height*2, width=.3, height=row_height) +
  draw_plot(g_creeper, x=.07, y=.75, width=.125, height=.125) +
  draw_plot(g_r_squared_B+no_legend+axline, x=.3, y=row_height*2, width=.3, height=row_height) +
  draw_plot(g_r_squared_C+no_legend+axline, x=.6, y=row_height*2, width=.3, height=row_height) +
  draw_plot(legend, x=.85, y=row_height*2, width=0.15, height=row_height) +
  # middle row
  draw_plot(
    g_representative_D + no_legend + fillcol, 
    x=0, y=row_height, width=.3, height=row_height) +
  draw_plot(
    g_representative_E + no_legend + fillcol, 
    x=.3, y=row_height, width=.3, height=row_height) +
  draw_plot(g_representative_F + no_legend + fillcol, 
            x=.6, y=row_height, width=.3, height=row_height) +
  draw_plot(legend_rep, x=.88, y=row_height, width=.12, height=row_height) +
  # bottom row
  draw_plot(g_indegree, x=0, y=0, width=.5, height=row_height) +
  draw_plot(g_dVm, x=.5, y=0, width=.5, height=row_height) +
  # labels
  draw_plot_label(
    LETTERS[1:8], 
    x=c(0,.3,.6,0,.3,.6, 0, .5), 
    y=c(rep(1, 3), rep(.66, 3), rep(.33, 2)), 
    size=10)

# Save figure
tiff("figure-3.tiff", width = 6.18, height = 5, units = "in", res = 300)
figure
dev.off()
