p_main_city <- gridExtra::grid.arrange(
  ggpubr::annotate_figure(p_comp_1taxa2city,
    fig.lab = "a",
    fig.lab.face = "bold"
  ),
  ggpubr::annotate_figure(p_rmse_taxa,
    fig.lab = "b",
    fig.lab.face = "bold"
  ),
  layout_matrix = rbind(
    c(1),
    c(2)
  ),
  # widths = c(3, 2),
  heights = c(3, 2)
)

p_main_city <-
  p_comp_1taxa2city +
  p_rmse_taxa +
  plot_layout(
    design = "
  A
  B
",
    heights = c(3, 2)
  ) +
  plot_annotation(tag_levels = "A")

# save main figure
if (.fig_save) {
  ggsave(
    plot = p_main_city,
    filename = str_c(.path$out_fig, "main_city.png"),
    width = 8,
    height = 8,
    device = png, type = "cairo"
  )
}
