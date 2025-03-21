if (.full_data) {
  v_f_evi <- list.files(str_c(.path$input, "ps/urban/evi"), "evi", full.names = T)
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

  v_f_doy <- list.files(str_c(.path$input, "ps/urban/doy"), "doy", full.names = T)
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
}
