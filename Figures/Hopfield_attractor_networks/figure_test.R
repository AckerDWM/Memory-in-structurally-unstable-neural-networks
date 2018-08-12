theme_set(
  theme_classic() +
    theme(
      text=element_text(size=8),
      strip.text=element_text(size=6),
      strip.background = element_blank()
    )
)
legend_top = get_legend(
  g_turnover + 
    labs(
      color="Reactivation #", 
      fill="Reactivation #",
      shape="Reactivation #"
    )
)
legend_middle = get_legend(g_rep_turnover+labs(fill="Firing rate")+guides(fill=guide_colorbar(barwidth=.5)))
no_legend = theme(legend.position = "none")
figure = ggdraw() +
  # top row
  draw_plot(g_turnover + no_legend, x=0, y=2/3, width=.3, height=1/3) +
  draw_plot(g_pconnection + no_legend, x=.3, y=2/3, width=.3, height=1/3) +
  draw_plot(g_nunits + no_legend, x=.6, y=2/3, width=.3, height=1/3) +
  draw_plot(legend_top, x=.88, y=2/3, width=.1, height=1/3) +
  draw_plot(g_input, x=1.2/24, y=2.25/3, width=.15, height=.15) +
  #draw_plot(g_u0, x=2.2/24, y=2.25/3, width=.15, height=.15) +
  # middle row
  draw_plot(g_rep_turnover + no_legend, x=0, y=1/3, width=.3, height=1/3) +
  draw_plot(g_rep_pconnection + no_legend, x=.3, y=1/3, width=.3, height=1/3) +
  draw_plot(g_rep_nunits + no_legend, x=.6, y=1/3, width=.3, height=1/3) +
  draw_plot(legend_middle, x=.9, y=1/3, width=.1, height=1/3) +
  # bottom row
  draw_plot(g_indegree, x=0, y=0, width=1/2, height=1/3) +
  draw_plot(g_epochs, x=1/2, y=0, width=1/2, height=1/3) +
  # labels
  draw_plot_label(
    LETTERS[1:8], 
    x=c(0,.3,.6,0,.3,.6,0,1/2), 
    y=c(1,1,1,2/3,2/3,2/3,1/3,1/3),
    size=10
  )

tiff("figure-3-test.tiff", width=6.18, height=5, units="in", res=300)
figure
dev.off()