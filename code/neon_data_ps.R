if (.full_data) {
  v_f_evi <- list.files(str_c(.path$ps, "NEON/evi"), "evi", full.names = T)

  ls_df_evi <- vector(mode = "list")

  for (f in v_f_evi) {
    ls_df_evi[[f]] <- read_rds(f)
  }

  df_neon_evi <- bind_rows(ls_df_evi)

  v_f_doy <- list.files(str_c(.path$ps, "NEON/doy"), "doy", full.names = T)

  ls_df_doy <- vector(mode = "list")

  for (f in v_f_doy) {
    ls_df_doy[[f]] <- read_rds(f)
  }

  df_neon_doy <- bind_rows(ls_df_doy)

  write_rds(df_neon_evi %>% filter(str_detect(id, "HARV|ORNL")), str_c(.path$input, "ps/neon/df_neon_evi_sample.rds"))
  write_rds(df_neon_doy, str_c(.path$input, "ps/neon/df_neon_doy.rds"))
} else {
  df_neon_evi_sample <- read_rds(str_c(.path$input, "ps/neon/df_neon_evi_sample.rds"))
  df_neon_doy <- read_rds(str_c(.path$input, "ps/neon/df_neon_doy.rds"))
}
