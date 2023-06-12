p_main_data <- gridExtra::grid.arrange(
  ggpubr::annotate_figure(p_nab_plant_map,
    fig.lab = "a",
    fig.lab.face = "bold"
  ),
  ggpubr::annotate_figure(p_nab_calen,
    fig.lab = "b",
    fig.lab.face = "bold"
  ),
  ggpubr::annotate_figure(p_ps_snap,
    fig.lab = "c",
    fig.lab.face = "bold"
  ),
  layout_matrix = rbind(
    c(1, 3),
    c(2, 2)
  ),
  widths = c(1, 1),
  heights = c(1, 1.4)
)

# save main figure
if (.fig_save) {
  ggsave(
    plot = p_main_data,
    filename = str_c(.path$out_fig, "main_data.png"),
    width = 8,
    height = 6,
    device = png, type = "cairo"
  )
}
