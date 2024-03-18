v_f_doy <- list.files(str_c(.path$ps, "NEON/doy"), "doy", full.names = T)

ls_df_doy <- vector(mode = "list")

for (f in v_f_doy) {
  ls_df_doy[[f]] <- read_rds(f)
}

df_neon_doy <- bind_rows(ls_df_doy)
