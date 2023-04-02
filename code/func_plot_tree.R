plot_tree_one <- function(ts_df, flower_df, yearoi, idoi) {
  ts_df_subset <- ts_df %>%
    filter(year == yearoi) %>%
    filter(id == idoi | id == "npn" | id == "pollen")
  flower_df_subset <- flower_df %>%
    filter(year == yearoi) %>%
    filter(id == idoi)

  p_tree <- ggplot() +
    geom_point(
      data = ts_df_subset %>%
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
  if (nrow(flower_df_subset) == 0) {
    p_tree
  } else {
    p_tree <- p_tree +
      geom_vline(data = flower_df_subset %>%
        mutate(doy = as.Date(doy, origin = str_c(yearoi, "-01-01"))), aes(xintercept = doy), col = "dark green", alpha = 0.2) +
      geom_vline(data = flower_df_subset %>%
        filter(thres == 0.5) %>%
        mutate(doy = as.Date(doy, origin = str_c(yearoi, "-01-01"))), aes(xintercept = doy), col = "dark green", alpha = 0.8)
  }

  return(p_tree)
}


plot_tree_mult <- function(ts_df, flower_df, yearoi, idoi) {
  ts_df_subset <- ts_df %>%
    filter(year == yearoi) %>%
    filter(id %in% idoi | id == "npn" | id == "pollen")
  flower_df_subset <- flower_df %>%
    filter(year == yearoi) %>%
    filter(id %in% idoi)

  p_tree <- ggplot() +
    geom_point(
      data = ts_df_subset %>%
        filter(var == "enhanced vegetation index (PS)" | var == "flower observation (Katz's team)") %>%
        group_by(id, var) %>%
        mutate(value_st = (value - min(value, na.rm = T)) / (max(value, na.rm = T) - min(value, na.rm = T))) %>% # standardize
        ungroup() %>%
        filter(id %in% idoi_list) %>%
        mutate(id = factor(id, levels = idoi_list)),
      aes(x = date, y = value_st, col = var)
    ) +
    geom_vline(
      data = flower_df_subset %>%
        mutate(doy = as.Date(doy, origin = str_c(yearoi, "-01-01"))) %>%
        left_join(
          df_dt_meta %>% select(id, species) %>%
            mutate(id = as.character(id)),
          by = "id"
        ) %>%
        mutate(id = factor(id, levels = idoi_list)),
      aes(xintercept = doy), col = "dark green", alpha = 0.2
    ) +
    geom_vline(
      data = flower_df_subset %>%
        filter(thres == 0.5) %>%
        mutate(doy = as.Date(doy, origin = str_c(yearoi, "-01-01"))) %>%
        left_join(
          df_dt_meta %>% select(id, species) %>%
            mutate(id = as.character(id)),
          by = "id"
        ) %>%
        mutate(id = factor(id, levels = idoi_list)),
      aes(xintercept = doy), col = "dark green", alpha = 0.8
    ) +
    facet_wrap(. ~ id, ncol = 1, scales = "free_y") +
    scale_color_manual(values = cols) +
    xlab("Day of year") +
    ylab("Standardized value") +
    theme_classic() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line = element_blank(),
      strip.background = element_rect(
        color = NA, fill = "grey"
      )
    ) +
    scale_x_date(date_labels = "%b", date_breaks = "3 month") +
    theme(legend.position = "bottom") +
    guides(col = guide_legend(title = ""))

  return(p_tree)
}
