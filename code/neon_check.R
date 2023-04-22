# check evi of different species
siteoi <- v_site[2]
df_ps <- read_rds(str_c(path_ps, "ts/ps_", siteoi, ".rds"))
df_ps_proc <- process_ps(df_ps) %>%
  left_join(
    df_plant %>%
      filter(site == siteoi) %>%
      drop_na(lon, lat) %>%
      select(id, growthform, species = sciname),
    by = "id"
  )

# df_ps_proc %>%
#   left_join(
#     df_plant %>%
#       filter(site == siteoi) %>%
#       drop_na(lon, lat) %>%
#       select(id, growthform, species = sciname),
#     by = "id"
#   ) %>%
#   filter(id =="NEON.PLA.D01.HARV.06001") %>%
#   ggplot()+
#   geom_point(aes(x = date, y = evi))+
#   geom_vline(xintercept = lubridate::as_date(str_c("2018", "-01-01"))+281-1)
set.seed(42)
v_id <- df_ps_proc %>% pull(id) %>% unique() %>% sample(3)
df_ps_proc %>% 
  filter(id %in% v_id) %>% 
  ggplot()+
  geom_line(aes(x = date, y=evi, group = id, col = species), alpha = 0.5)+
  # ggtitle(str_c(df_idoi$species, " ",df_idoi$growthform))+
  theme_classic()+
  theme(legend.position="bottom")

p_ps_growthform <- df_ps_proc %>%
  # filter(year == 2019) %>%
  group_by(growthform, date) %>%
  summarise(
    median = median(evi, na.rm = T),
    upper = quantile(evi, 0.95, na.rm = T),
    lower = quantile(evi, 0.05, na.rm = T)
  ) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = date, y = median, col = growthform), alpha = 1) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = growthform), alpha = 0.1) +
  # geom_vline(xintercept = 125)+
  theme_classic() +
  ylab("evi")+
  theme(legend.position="bottom")
p_ps_growthform

p_ps_species <- df_ps_proc %>%
  filter(str_detect(species, "Quercus")) %>% 
  filter( species %in% (group_by(.,species) %>% summarise(n=id %>% unique() %>% length()) %>% arrange(desc(n)) %>% head(3) %>% pull(species))) %>% 
  group_by(growthform, date, species) %>%
  summarise(
    median = median(evi, na.rm = T),
    upper = quantile(evi, 0.95, na.rm = T),
    lower = quantile(evi, 0.05, na.rm = T)
  ) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = date, y = median, col = species), alpha = 1) +
  # geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = species), alpha = 0.1) +
  # geom_vline(xintercept = 125)+
  theme_classic() +
  ylab("evi")+
  theme(legend.position="bottom")
p_ps_species


p_ps_indi <- df_ps_proc %>%
  filter(str_detect(species, "Quercus douglasii")) %>% 
  filter( id %in% (pull(.,id) %>% unique() %>% sample(3))) %>% 
  group_by(growthform, date, species, id) %>%
  summarise(
    median = median(evi, na.rm = T),
    upper = quantile(evi, 0.95, na.rm = T),
    lower = quantile(evi, 0.05, na.rm = T)
  ) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = date, y = median, col = id), alpha = 1) +
  # geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = species), alpha = 0.1) +
  # geom_vline(xintercept = 125)+
  theme_classic() +
  ylab("evi")+
  theme(legend.position="bottom")
p_ps_indi
