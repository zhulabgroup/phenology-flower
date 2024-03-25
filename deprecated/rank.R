library(tidyverse)
v_site <- c("NY", "DT", "AT", "DV", "TP", "ST", "HT") # , "SJ"
taxaoi <- "Acer"
directionoi <- "up"

ls_gg_ps <- ls_df_model <- ls_df_ps <- vector(mode = "list")
for (siteoi in v_site) {
  df_doy <- read_rds(str_c("PSdata/doy/doy_", siteoi, "_", taxaoi, ".rds")) %>%
    filter(year > 2017) %>%
    drop_na() %>%
    filter(direction == directionoi, thres == 0.5) %>%
    filter(
      doy < quantile(doy, 0.95),
      doy > quantile(doy, 0.05)
    )
  if (nrow(df_doy) >= 500) {
    df_mat <- read_rds(str_c("LSTdata/ts/mat_", siteoi, "_", taxaoi, ".rds")) %>% filter(year > 2017)

    df_ps_site <- df_doy %>%
      inner_join(
        group_by(., id) %>%
          summarise(
            doy_mean = mean(doy),
            n = n()
          ) %>%
          filter(n >= 3),
        by = "id"
      ) %>%
      left_join(df_mat, by = c("year", "id")) %>%
      mutate(site = siteoi)
    # mutate(doy_mean_sd = (doy_mean-mean(doy_mean))/sd(doy_mean),
    #        mat_sd = (mat-mean(mat))/sd(mat))

    ls_df_ps[[siteoi]] <- df_ps_site

    model_rank <- lm(doy ~ doy_mean, data = df_ps_site) %>% summary()
    model_clim <- lm(doy ~ mat, data = df_ps_site) %>% summary()
    # model_full <- lm(doy ~ doy_mean + mat , data = ls_df_ps[[siteoi]])
    # rela_importance <- relaimpo::calc.relimp(model_full, type = c("lmg", "last", "first", "pratt"))

    ls_df_model[[siteoi]] <- bind_rows(
      cbind(model_rank$coefficients %>% data.frame() %>% rownames_to_column(var = "predictor") %>% filter(predictor != "(Intercept)"),
        adj.r.squared = model_rank$adj.r.squared
      ),
      cbind(model_clim$coefficients %>% data.frame() %>% rownames_to_column(var = "predictor") %>% filter(predictor != "(Intercept)"),
        adj.r.squared = model_clim$adj.r.squared
      )
    ) %>%
      mutate(site = siteoi)

    ls_gg_ps[[siteoi]] <- df_ps_site %>%
      ggplot(aes(x = doy_mean, y = doy)) +
      geom_hex(bins = 20) +
      scale_fill_gradient(low = "white", high = "dark green") +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", lty = "dashed") +
      ggpubr::stat_cor(
        aes(label = str_c(after_stat(rr.label),
          # after_stat(p.label),
          sep = "*`,`~"
        )),
        p.accuracy = 0.05,
        color = "red"
      ) +
      ggthemes::theme_few() +
      guides(fill = "none") +
      ggtitle(siteoi) +
      labs(
        x = str_c("Multi-year mean\n50% green-", directionoi, " time (DOY)"),
        y = str_c("Current year\n50% green-", directionoi, " time (DOY)")
      )
  }

  print(siteoi)
}

df_ps <- bind_rows(ls_df_ps) %>%
  select(site, everything()) %>%
  arrange(site)

df_model <- bind_rows(ls_df_model) %>%
  select(site, predictor, everything()) %>%
  arrange(site)

model_rank <- lm(doy ~ doy_mean, data = df_ps, weights = df_ps %>%
  left_join(group_by(., site) %>% summarise(weight = 1 / n()), by = "site")
  %>% pull(weight)) %>% summary()
model_rank$coefficients
model_rank$adj.r.squared

model_clim <- lm(doy ~ mat, data = df_ps, weights = df_ps %>%
  left_join(group_by(., site) %>% summarise(weight = 1 / n()), by = "site")
  %>% pull(weight)) %>% summary()
model_clim$coefficients
model_clim$adj.r.squared

gg_ps <- ls_gg_ps %>% patchwork::wrap_plots()
gg_ps

### NEON
v_site <- read_rds("data/processed/dat_neon_npn.rds")$metric %>%
  pull(site) %>%
  unique() %>%
  sort()

taxaoi <- "Acer"

ls_df_neon <- vector(mode = "list")
for (siteoi in v_site) {
  df_site <- read_rds("data/processed/dat_neon_npn.rds")$metric %>%
    filter(site == siteoi) %>%
    filter(str_detect(genus, taxaoi)) %>%
    filter(event == "leaf") %>%
    filter(doy < 300)

  if (nrow(df_site) > 0) {
    ls_df_neon[[siteoi]] <- df_site %>%
      inner_join(
        group_by(., id) %>%
          summarise(
            doy_mean = mean(doy),
            n = n()
          ) %>%
          filter(n >= 3),
        by = "id"
      ) %>%
      mutate(doy_mean_sd = (doy_mean - mean(doy_mean)) / sd(doy_mean)) %>%
      mutate(site = siteoi)
  }


  print(siteoi)
}

df_neon <- bind_rows(ls_df_neon) %>%
  select(site, everything()) %>%
  arrange(site)

df_neon %>%
  ggplot(aes(x = doy_mean, y = doy)) +
  geom_jitter(alpha = 0.5, col = "dark green") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", lty = "dashed") +
  ggpubr::stat_cor(
    aes(label = str_c(after_stat(rr.label), after_stat(p.label), sep = "*`,`~")),
    p.accuracy = 0.05,
    color = "red"
  ) +
  ggthemes::theme_few() +
  facet_wrap(. ~ site, scales = "free")
