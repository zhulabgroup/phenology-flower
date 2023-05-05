p_main_data <-
  p_nab_plant_map +
  p_nab_calen +
  p_ps_snap +
  plot_layout(
    design = "
  AC
  BB
",
    widths = c(3, 2),
    heights = c(2, 3)
  ) +
  plot_annotation(tag_levels = "A")

# save main figure
if (.fig_save) {
  ggsave(
    plot = p_main_data,
    filename = str_c(.path$out_fig, "main_data.png"),
    width = 10,
    height = 10
  )
}
