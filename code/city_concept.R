p_conceptual <- cowplot::ggdraw() +
  cowplot::draw_image("figures/methods.png")

# save figure file
if (.fig_save) {
  ggsave(
    plot = p_conceptual,
    filename = str_c(.path$out_fig, "main-methods.png"),
    width = 8,
    height = 8 * 720 / 1280,
    device = png, type = "cairo"
  )
}
