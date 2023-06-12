p_main_neon <-
  p_neon_ps_corr_flower
#   p_neon_leaf_flower +
#   p_neon_ps_corr_flower +
#   p_rank_ps_neon +
#   plot_layout(
#     design = "
#   A
#   B
#   C
# ",
#     heights = c(1, 1, 1)
#   ) +
#   plot_annotation(tag_levels = "A")

# save main figure
if (.fig_save) {
  ggsave(
    plot = p_main_neon,
    filename = str_c(.path$out_fig, "main_neon.png"),
    width = 8,
    height = 4,
    device = png, type = "cairo"
  )
}
