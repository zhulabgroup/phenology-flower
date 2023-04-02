cl <- makeCluster(20, outfile = "")
registerDoSNOW(cl)

for (taxaoi in v_taxa) {
  taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
  df_thres_taxa <- get_thres_taxa(df_thres, taxaoi)
  df_flower_doy <- read_rds(str_c("data/results/", taxaoi, "/flower_doy.rds"))

  ls_df_flower_freq_site <- vector(mode = "list", length = length(v_site))
  for (s in 1:length(v_site)) {
    siteoi <- v_site[s]
    df_flower_doy_site <- df_flower_doy %>%
      filter(site == siteoi)
    
    if (nrow(df_flower_doy_site)) {
      ls_df_flower_freq_year <- vector(mode = "list", length = length(v_year))
      for (y in 1:length(v_year)) {
        yearoi <- v_year[y]
        df_flower_doy_year <- df_flower_doy_site %>%
          filter(year == yearoi)
        ls_df_flower_freq_thres <-
          foreach(
            t = 1:nrow(df_thres_taxa),
            .packages = c("tidyverse")
          ) %dopar% {
            df_flower_doy_year %>%
              drop_na(start, end) %>%
              filter(
                direction == df_thres_taxa$direction[t],
                thres == df_thres_taxa$threshold[t]
              ) %>%
              group_by(doy, thres, direction) %>%
              summarise(count = n()) %>%
              mutate(freq = count / n()) %>%
              ungroup() %>%
              group_by(thres, direction) %>%
              complete(doy = c((274 - 365):(365 + 151)), fill = list(count = 0, freq = 0)) %>%
              ungroup() %>%
              mutate(freq_sm = freq %>% whitfun(lambda = 30))
          }
        ls_df_flower_freq_year[[y]] <- bind_rows(ls_df_flower_freq_thres) %>%
          mutate(year = yearoi)
        print(str_c(siteoi, ", ", yearoi))
      }
      ls_df_flower_freq_site[[s]] <- bind_rows(ls_df_flower_freq_year) %>%
        mutate(site = siteoi)
    }
    
  }
  df_flower_freq <- bind_rows(ls_df_flower_freq_site)

  path_output <- str_c("data/results/", taxaoi, "/")
  write_rds(df_flower_freq, str_c(path_output, "flower_freq.rds"))
}

stopCluster(cl)
