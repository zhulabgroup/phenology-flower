p_main_city <-
  p_comp_1taxa2city +
  p_taxa_spearman +
  plot_layout(
    design = "
  A
  B
",
    heights = c(3, 2)
  ) +
  plot_annotation(tag_levels = "A")

# save main figure
if (.fig_save) {
  ggsave(
    plot = p_main_city,
    filename = str_c(.path$out_fig, "main_city.png"),
    width = 8,
    height = 8,
    device = png, type = "cairo"
  )
}
