ls_df_tune <- vector(mode = "list")
for (taxaoi in v_taxa) {
  path_output <- paste0("./data/results/", taxaoi, "/")
  ls_df_tune[[taxaoi]] <- read_rds(str_c(path_output, "tune.rds")) %>%
    mutate(taxa = taxaoi)
}
df_tune <- bind_rows(ls_df_tune)
df_best_thres <- df_tune %>%
  group_by(taxa, direction, thres) %>%
  summarise(mse = weighted.mean(mse, n)
            ) %>%
  ungroup() %>% 
  arrange(mse) %>%
  group_by(taxa) %>% 
  slice(1) %>%
  select(taxa, direction, thres)

df_fit <- df_tune %>%
  right_join(df_best_thres, by = c("taxa", "direction", "thres")) %>%
  left_join(df_meta %>% select(site, sitename), by =  "site") 

p_fit<-df_fit %>% 
  group_by(site,sitename, taxa) %>% 
  summarise(rmse_ps = mse_ps %>% weighted.mean( n) %>% sqrt(),
            rmse_clim = mse_clim %>% weighted.mean( n) %>% sqrt(),
            n = sum(n)
  ) %>% 
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = site, y = rmse_ps), col = "dark blue", cex = 2) +
  geom_point(aes(x = site, y = rmse_clim), col = "dark red") +
  facet_wrap(. ~ taxa) +
  ylab("RMSE") +
  theme_classic()

tb_fit_taxa<-df_fit %>%
  filter(!taxa %in% c("Poaceae early", "Poaceae late", "Ambrosia")) %>%
  # filter(site!="SJ") %>% 
  group_by( taxa) %>% 
  summarise(
    rmse_ps = mse_ps %>% weighted.mean( n) %>% sqrt(),
            rmse_clim = mse_clim %>% weighted.mean( n) %>% sqrt(),
            n = sum(n)
  ) %>% 
  ungroup() %>%
  mutate(taxa = factor(taxa, levels = v_taxa)) %>%
  arrange(taxa) %>%
  mutate(diff = (rmse_ps - rmse_clim) / rmse_clim)

tb_fit_site<-df_fit %>%
  filter(!taxa %in% c("Poaceae early", "Poaceae late", "Ambrosia")) %>%
  group_by(site, sitename) %>%
  summarize(
    rmse_ps = mse_ps %>% weighted.mean( n) %>% sqrt(),
    rmse_clim = mse_clim %>% weighted.mean( n) %>% sqrt(),
    n = sum(n)
  ) %>%
  ungroup() %>%
  rename(city = sitename) %>%
  mutate(city = factor(city, levels = v_site_lat)) %>%
  mutate(diff = (rmse_ps - rmse_clim) / rmse_clim)
