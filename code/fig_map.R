p_main_map <-
  p_doy_variation +
  p_plant_map_doy +
  plot_layout(
    design = "
  A
  B
",
    heights = c(1, 3)
  ) +
  plot_annotation(tag_levels = "A")
