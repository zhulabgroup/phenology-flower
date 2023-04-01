# set color palette
cols <- c("enhanced vegetation index (PS)" = "dark green", "G2R (PS)" = "yellow green", "EBI (PS)" = "orange", "pollen concentration (NAB)" = "dark red", "flower observation (USA-NPN)" = "dark orchid", "flowering frequency" = "dark blue", "flower observation (Katz's team)" = "coral")

# possible green up and green down thresholds
thres_list_up <- seq(from = 0, to = 1, by = 0.1) %>% round(1)
thres_list_down <- seq(from = 1, to = 0.0, by = -0.1) %>% round(1)
thres_df <- bind_rows(
  data.frame(direction = "up", threshold = thres_list_up),
  data.frame(direction = "down", threshold = thres_list_down)
)

# read in manually determined window of flowering
# need to determine based on nab data
flower_window_df <- read_csv("./data/processed/flower_window.csv")
