p_main_loocv <-
  p_rmse_all +
  p_rmse_taxa +
  p_rmse_city +
  plot_layout(
    design = "
  AB
  AC
",
    widths = c(1, 5)
  ) +
  plot_annotation(tag_levels = "A")

# save main figure
if (.fig_save) {
  ggsave(
    plot = p_main_loocv,
    filename = str_c(.path$out_fig, "main_loocv.png"),
    width = 10,
    height = 10
  )
}
