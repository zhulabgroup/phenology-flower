p_main_map <-
  p_doy_variation +
  p_plant_map_doy +
  plot_layout(
    design = "
  A
  B
",
    heights = c(1, 2)
  ) +
  plot_annotation(tag_levels = "A")

# save main figure
if (.fig_save) {
  ggsave(
    plot = p_main_map,
    filename = str_c(.path$out_fig, "main_map.png"),
    width = 8,
    height = 9,
    device = png, type = "cairo"
  )
}
