plot_tree_one <- function(df_ts, df_flower, yearoi, idoi) {
  df_ts_subset <- df_ts %>%
    filter(year == yearoi) %>%
    filter(id == idoi | id == "npn" | id == "pollen")
  df_flower_subset <- df_flower %>%
    filter(year == yearoi) %>%
    filter(id == idoi)

  p_tree <- ggplot() +
    geom_point(
      data = df_ts_subset %>%
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
  if (nrow(df_flower_subset) == 0) {
    p_tree
  } else {
    p_tree <- p_tree +
      geom_vline(data = df_flower_subset %>%
        mutate(doy = as.Date(doy, origin = str_c(yearoi, "-01-01"))), aes(xintercept = doy), col = "dark green", alpha = 0.2) +
      geom_vline(data = df_flower_subset %>%
        filter(thres == 0.5) %>%
        mutate(doy = as.Date(doy, origin = str_c(yearoi, "-01-01"))), aes(xintercept = doy), col = "dark green", alpha = 0.8)
  }

  return(p_tree)
}


plot_tree_mult <- function(df_ts, df_flower, yearoi, idoi) {
  df_ts_subset <- df_ts %>%
    filter(year == yearoi) %>%
    filter(id %in% idoi | id == "npn" | id == "pollen")
  df_flower_subset <- df_flower %>%
    filter(year == yearoi) %>%
    filter(id %in% idoi)

  p_tree <- ggplot() +
    geom_point(
      data = df_ts_subset %>%
        filter(var == "enhanced vegetation index (PS)" | var == "flower observation (Katz's team)") %>%
        mutate(var = factor(var,
          levels = c(
            "enhanced vegetation index (PS)",
            "flower observation (Katz's team)"
          ),
          labels = c(
            "Enhanced Vegetation Index (PlanetScope)",
            "Percentage of open flowers (Katz et al., 2019)"
          )
        )) %>%
        group_by(id, var) %>%
        mutate(value_st = (value - min(value, na.rm = T)) / (max(value, na.rm = T) - min(value, na.rm = T))) %>% # standardize
        ungroup() %>%
        filter(id %in% idoi) %>%
        mutate(id = factor(id, levels = idoi)),
      aes(x = date, y = value_st, col = var)
    ) +
    geom_vline(
      data = df_flower_subset %>%
        mutate(doy = as.Date(doy, origin = str_c(yearoi, "-01-01"))) %>%
        left_join(
          df_dt_meta %>% select(id, species) %>%
            mutate(id = as.character(id)),
          by = "id"
        ) %>%
        mutate(id = factor(id, levels = idoi)),
      aes(xintercept = doy), col = "dark green", alpha = 0.2
    ) +
    geom_vline(
      data = df_flower_subset %>%
        filter(thres == 0.5) %>%
        mutate(doy = as.Date(doy, origin = str_c(yearoi, "-01-01"))) %>%
        left_join(
          df_dt_meta %>% select(id, species) %>%
            mutate(id = as.character(id)),
          by = "id"
        ) %>%
        mutate(id = factor(id, levels = idoi)),
      aes(xintercept = doy), col = "dark green", alpha = 0.8
    ) +
    facet_wrap(. ~ id, ncol = 1, scales = "free_y") +
    scale_color_manual(values = cols) +
    xlab("Time of year") +
    ylab("Standardized value") +
    theme_classic() +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank(),
      # axis.line = element_blank(),
    ) +
    scale_x_date(date_labels = "%b", date_breaks = "3 month") +
    scale_y_continuous(n.breaks = 3) +
    theme(legend.position = "bottom") +
    guides(col = guide_legend(title = ""))

  return(p_tree)
}
