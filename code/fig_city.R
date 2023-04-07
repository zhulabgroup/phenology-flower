p_main_city <-
  p_comp_1city +
  p_comp_1taxa +
  plot_layout(design = "
  A
  B
") +
  plot_annotation(tag_levels = "A")

# save main figure
if (.fig_save) {
  ggsave(
    plot = p_main_city,
    filename = str_c(.path$out_fig, "main_city.png"),
    width = 10,
    height = 10
  )
}
