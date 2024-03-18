# possible green up and green down thresholds
thres_up <- seq(from = 0, to = 1, by = 0.1) %>% round(1)
thres_down <- seq(from = 1, to = 0.0, by = -0.1) %>% round(1)
df_thres <- bind_rows(
  data.frame(direction = "up", threshold = thres_up),
  data.frame(direction = "down", threshold = thres_down)
)

get_thres_taxa <- function(df_thres, taxaoi) {
  if (taxaoi %in% c("Ambrosia", "Ulmus late", "neon_down")) {
    df_thres_taxa <- df_thres %>% filter(direction == "down")
  } else if (taxaoi == "Poaceae early") {
    df_thres_taxa <- df_thres %>% filter(threshold >= 0.5 | direction == "up")
  } else if (taxaoi == "Poaceae late") {
    df_thres_taxa <- df_thres %>% filter(threshold >= 0.5 | direction == "down")
  } else {
    df_thres_taxa <- df_thres %>% filter(direction == "up")
  }
  return(df_thres_taxa)
}