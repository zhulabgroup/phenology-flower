p_main_neon <-
  p_neon_data +
  p_neon_ps_corr_flower +
  p_neon_leaf_flower +
  plot_layout(
    design = "
  A
  B
",
    heights = c(1, 1)
  ) +
  plot_annotation(tag_levels = "A")
