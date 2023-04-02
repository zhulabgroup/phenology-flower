cl <- makeCluster(20, outfile = "")
registerDoSNOW(cl)

for (taxaoi in taxa_list) {
  taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
  thres_df_taxa <- get_thres_taxa(thres_df, taxaoi)
  flower_doy_df <- read_rds(str_c("data/results/", taxaoi, "/flower_doy.rds"))

  flower_freq_df_site_list <- vector(mode = "list", length = length(site_list))
  for (s in 1:length(site_list)) {
    siteoi <- site_list[s]
    flower_doy_df_site <- flower_doy_df %>%
      filter(site == siteoi)
    flower_freq_df_year_list <- vector(mode = "list", length = length(year_list))
    for (y in 1:length(year_list)) {
      yearoi <- year_list[y]
      flower_doy_df_year <- flower_doy_df_site %>%
        filter(year == yearoi)
      flower_freq_df_thres_list <-
        foreach(
          t = 1:nrow(thres_df_taxa),
          .packages = c("tidyverse")
        ) %dopar% {
          flower_freq_df_thres <- flower_doy_df_year %>%
            drop_na(start, end) %>%
            filter(
              direction == thres_df_taxa$direction[t],
              thres == thres_df_taxa$threshold[t]
            ) %>%
            group_by(doy, thres, direction) %>%
            summarise(count = n()) %>%
            mutate(freq = count / n()) %>%
            ungroup() %>%
            group_by(thres, direction) %>%
            complete(doy = seq(1, 365, by = 1), fill = list(count = 0, freq = 0)) %>%
            ungroup() %>%
            mutate(freq_sm = freq %>% whitfun(lambda = 30))

          flower_freq_df_thres
        }
      flower_freq_df_year_list[[y]] <- bind_rows(flower_freq_df_thres_list) %>%
        mutate(year = yearoi)
      print(str_c(siteoi, ", ", yearoi))
    }
    flower_freq_df_site_list[[s]] <- bind_rows(flower_freq_df_year_list) %>%
      mutate(site = siteoi)
  }
  flower_freq_df <- bind_rows(flower_freq_df_site_list)

  output_path <- paste0("data/results", taxaoi, "/")
  write_rds(flower_freq_df, str_c(output_path, "flower_freq.rds"))
}

stopCluster(cl)
