p_main_indi <- gridExtra::grid.arrange(
  ggpubr::annotate_figure(p_dt_doy_mult,
    fig.lab = "a",
    fig.lab.face = "bold"
  ),
  ggpubr::annotate_figure(p_dt_corr_2017,
    fig.lab = "b",
    fig.lab.face = "bold"
  ),
  ggpubr::annotate_figure(p_dt_corr_years,
    fig.lab = "c",
    fig.lab.face = "bold"
  ),
  layout_matrix = rbind(
    c(1, 2),
    c(1, 3)
  ),
  widths = c(3, 2),
  heights = c(1, 1)
)

# save main figure
if (.fig_save) {
  ggsave(
    plot = p_main_indi,
    filename = str_c(.path$out_fig, "main_indi.png"),
    width = 10,
    height = 10,
    device = png, type = "cairo"
  )
}
