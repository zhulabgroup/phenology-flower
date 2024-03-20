p_main_neon <-
  p_neon_data +
  p_neon_ps_corr_flower +
  p_neon_leaf_flower +
  plot_layout(
    design = "
  A
  B
",
    heights = c(2, 1)
  ) +
  plot_annotation(tag_levels = "A")

# save main figure
if (.fig_save) {
  ggsave(
    plot = p_main_neon,
    filename = str_c(.path$out_fig, "main_neon.png"),
    width = 10,
    height = 9,
    device = png, type = "cairo"
  )
}
