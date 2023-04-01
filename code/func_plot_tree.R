plot_tree <- function(ts_df, flower_df, idoi) {
  p_1tree <- ggplot() +
    geom_point(
      data = ts_df_subset %>% filter(id == idoi | id == "npn" | id == "pollen") %>%
        filter(var %in% c("enhanced vegetation index (PS)", "pollen concentration (NAB)", "flower observation (USA-NPN)", "flower observation (Katz's team)")),
      aes(x = date, y = value, group = as.factor(id), col = var)
    ) +
    theme_classic() +
    guides(
      col = "none",
      alpha = "none"
    ) +
    scale_color_manual(values = cols) +
    facet_wrap(. ~ var, ncol = 1, scales = "free_y") +
    xlab("Day of year") +
    ylab("Standardized value") +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line = element_blank(),
      strip.background = element_rect(
        color = NA, fill = "grey"
      )
    ) +
    scale_x_date(date_labels = "%b", date_breaks = "3 month")
  # ggtitle(paste0("Site: ", siteoi,"  Year: ", yearoi,"  ID: ", idoi, "  Species: ", plant_sp))
  if (nrow(flower_df) == 0) {
    p_1tree
  } else {
    yearoi <- ts_df %>%
      pull(year) %>%
      head(1)
    p_1tree <- p_1tree +
      geom_vline(data = flower_df %>%
        mutate(doy = as.Date(doy, origin = str_c(yearoi, "-01-01"))), aes(xintercept = doy), col = "dark green", alpha = 0.2) +
      geom_vline(data = flower_df %>%
        filter(thres == 0.5) %>%
        mutate(doy = as.Date(doy, origin = str_c(yearoi, "-01-01"))), aes(xintercept = doy), col = "dark green", alpha = 0.8)
  }

  return(p_1tree)
}
