p_main_city <-
  p_comp_1taxa2city +
  plot_layout(design = "
  A
") #+
# plot_annotation(tag_levels = "A")

# save main figure
if (.fig_save) {
  ggsave(
    plot = p_main_city,
    filename = str_c(.path$out_fig, "main_city.png"),
    width = 10,
    height = 6
  )
}
