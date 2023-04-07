p_main_data <-
  p_nab_plant_map +
  p_ps_snap +
  p_nab_calen +
  plot_layout(
    design = "
  AB
  CC
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
