df_nab_raw <- tidynab::load_data(request = "2023") %>% mutate(Date = as.Date(Date))
df_nab_long <- tidynab::parse_data(df_nab_raw)
df_nab_taxa <- tidynab::harmonize_taxa(preload = T)
df_nab <- df_nab_long %>%
  left_join(df_nab_taxa, by = c("taxa" = "taxa_raw")) %>%
  filter(kingdom == "Viridiplantae") %>%
  select(date, taxa = taxa_clean, family, genus, count, stationid)

if (FALSE) {
  write_rds(df_nab, str_c(.path$nab, "clean/dat_", Sys.Date(), ".rds"))
}
