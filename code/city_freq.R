if (list.files(str_c(.path$intermediate, "urban/"), pattern = "ps_freq.rds", recursive = T) %>% length() == 0) {
  cl <- makeCluster(20, outfile = "")
  registerDoSNOW(cl)

  ls_df_ps_freq_taxa <- vector(mode = "list")
  for (taxaoi in v_taxa) {
    taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
    df_thres_taxa <- get_thres_taxa(df_thres, taxaoi)
    df_ps_doy_taxa <- df_ps_doy %>%
      filter(str_detect(taxa, taxaoi_short))

    ls_df_ps_freq_site <- vector(mode = "list", length = length(v_site_tune))
    for (siteoi in v_site_tune) {
      df_ps_doy_site <- df_ps_doy_taxa %>%
        filter(site == siteoi)

      if (nrow(df_ps_doy_site) > 0) {
        ls_df_ps_freq_year <- vector(mode = "list", length = length(v_year))
        for (yearoi in v_year) {
          df_ps_doy_year <- df_ps_doy_site %>%
            filter(year == yearoi)
          ls_df_ps_freq_thres <-
            foreach(
              t = 1:nrow(df_thres_taxa),
              .packages = c("tidyverse")
            ) %dopar% {
              df <- df_ps_doy_year %>%
                drop_na(start, end) %>%
                filter(
                  direction == df_thres_taxa$direction[t],
                  thres == df_thres_taxa$threshold[t]
                ) %>%
                group_by(doy, thres, direction) %>%
                summarise(count = n()) %>%
                ungroup() %>%
                group_by(thres, direction) %>%
                complete(doy = c(-90:(365 + 90)), fill = list(count = 0)) %>%
                mutate(count_sm = util_fill_whit(x = count, maxgap = Inf, lambda = 30, minseg = 1)) %>%
                mutate(freq = count_sm / sum(count_sm)) %>% # convert to frequency
                ungroup()
            }

          print(str_c(taxaoi, siteoi, yearoi, sep = ", "))

          ls_df_ps_freq_year[[yearoi %>% as.character()]] <- bind_rows(ls_df_ps_freq_thres) %>%
            mutate(year = yearoi)
        }
        ls_df_ps_freq_site[[siteoi]] <- bind_rows(ls_df_ps_freq_year) %>%
          mutate(site = siteoi)
      }
    }
    ls_df_ps_freq_taxa <- bind_rows(ls_df_ps_freq_site)
    path_output <- str_c(.path$intermediate, "urban/", taxaoi, "/")
    dir.create(path_output, recursive = T, showWarnings = F)
    write_rds(ls_df_ps_freq_taxa, str_c(path_output, "ps_freq.rds"))
  }

  stopCluster(cl)
}
