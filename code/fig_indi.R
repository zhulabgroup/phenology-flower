p_main_indi <-
  p_dt_doy_mult +
  p_dt_corr_2017 +
  p_dt_corr_years +
  plot_layout(
    design = "
  AB
  AC
",
    widths = c(3, 2)
  ) +
  plot_annotation(tag_levels = "A")

# save main figure
if (.fig_save) {
  ggsave(
    plot = p_main_indi,
    filename = str_c(.path$out_fig, "main_indi.png"),
    width = 10,
    height = 10
  )
}
