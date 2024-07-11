pollen_df<- nab_with_taxa_df %>% 
  left_join(meta_df %>% dplyr::select(id, site), by="id") %>% 
  filter(site==siteoi) %>% 
  filter(genus==taxaoi_short|family==taxaoi_short) 
  

