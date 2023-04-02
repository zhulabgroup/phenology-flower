p_main_clim <-
  p_lag_clim +
  p_slope +
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
    plot = p_main_clim,
    filename = str_c(.path$out_fig, "main_clim.png"),
    width = 10,
    height = 10
  )
}
