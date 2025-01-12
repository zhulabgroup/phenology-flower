p_main_data <-
  p_nab_plant_map +
  p_nab_calen +
  p_ps_snap +
  plot_layout(
    design = "
  AC
  BB
",
    widths = c(1, 1),
    heights = c(1, 2)
  ) +
  plot_annotation(tag_levels = "A")
