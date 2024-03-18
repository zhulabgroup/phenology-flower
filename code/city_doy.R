v_f_evi <- list.files(str_c(.path$ps, "urban/evi"), "evi", full.names = T)
ls_df_evi <- vector(mode = "list")
for (f in v_f_evi) {
  site <- f %>%
    str_split("/", simplify = T) %>%
    rev() %>%
    `[`(1) %>%
    str_remove(".rds") %>%
    str_split("_", simplify = T) %>%
    `[`(2)
  taxa <- f %>%
    str_split("/", simplify = T) %>%
    rev() %>%
    `[`(1) %>%
    str_remove(".rds") %>%
    str_split("_", simplify = T) %>%
    `[`(3)
  ls_df_evi[[f]] <- read_rds(f) %>%
    group_by(year, doy) %>%
    summarise(evi = median(evi), .groups = "drop") %>%
    mutate(
      taxa = taxa,
      site = site
    )
}
df_ps_evi <- bind_rows(ls_df_evi)

if (FALSE) {
  (p_evi_compare_with_vacant <- df_ps_evi %>%
    filter(site == "AT", year == 2019) %>%
    ggplot() +
    geom_line(aes(x = doy, y = evi, group = taxa, col = ifelse(taxa == "Vacant", 1, 2)), alpha = 0.5))
}


v_f_doy <- list.files(str_c(.path$ps, "urban/doy"), "doy", full.names = T)
ls_df_doy <- vector(mode = "list")
for (f in v_f_doy) {
  site <- f %>%
    str_split("/", simplify = T) %>%
    rev() %>%
    `[`(1) %>%
    str_split("_", simplify = T) %>%
    `[`(2)
  ls_df_doy[[f]] <- read_rds(f) %>%
    mutate(site = site)
}
df_ps_doy <- bind_rows(ls_df_doy) %>%
  inner_join(df_tree, by = c("id", "site"))
if (FALSE) {
  p_doy_compare_with_vacant <- df_ps_doy %>%
    filter(direction == "up", thres == 0.5) %>%
    filter(site == "AT") %>%
    ggplot() +
    geom_density(aes(doy, group = genus, col = genus), alpha = 0.5)
}
