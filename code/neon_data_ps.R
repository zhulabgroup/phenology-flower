v_f_evi <- list.files(str_c(.path$input, "ps/neon/evi"), "evi", full.names = T)

ls_df_evi <- vector(mode = "list")

for (f in v_f_evi) {
  ls_df_evi[[f]] <- read_rds(f)
}

df_neon_evi <- bind_rows(ls_df_evi)

v_f_doy <- list.files(str_c(.path$input, "ps/neon/doy"), "doy", full.names = T)

ls_df_doy <- vector(mode = "list")

for (f in v_f_doy) {
  ls_df_doy[[f]] <- read_rds(f)
}

df_neon_doy <- bind_rows(ls_df_doy)
