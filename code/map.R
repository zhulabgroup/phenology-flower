library(rgdal)
library(tidyverse)
library(ggrepel)
library(foreach)
library(doSNOW)

fia_df<-read_csv("./RS4flower/data/species_back0.csv")
perc_df<-fia_df %>% 
  drop_na() %>% 
  filter(lat>=25 & lat<=50 & lon < -60 & lon> -130) %>% # focus on CONUS
  filter(dia>5) %>% # adult trees
  group_by(plt,lon,lat) %>% 
  # summarise(percent=sum(genus=="Betula")/n())
  summarise(percent=sum(genus=="Quercus")/n())

p_map<-
  ggplot() +
  geom_polygon( data=map_data("state"), aes(x=long, y=lat, group = group), fill="white" ) +
  geom_point(data=perc_df[perc_df$percent==0,],aes(x=lon,y=lat),cex=0.5,color="white", alpha=0.5)+
  geom_point(data=perc_df[perc_df$percent>0,],aes(x=lon,y=lat,color=percent),cex=0.5, alpha=0.5)+
  scale_colour_gradient(low = "white", high = "#2D6EB0",
                        space = "Lab", na.value = "grey50", guide = "colourbar",
                        aesthetics = "colour",limits = c(0,1), breaks = c(0,0.5, 1))+
  geom_path( data=map_data("state"), aes(x=long, y=lat, group = group),color="grey50" ,alpha=0.5,lwd=0.2)+
  theme_void()+
  # theme(legend.position = c(0.05,0.05),
  #       legend.justification = c("left", "bottom"),
  #       legend.direction = "horizontal",
  #       legend.title  = element_blank())+
  # theme(legend.text=element_text(size=12))+
  # legend.title  = element_text(size = 10))
  # guides(color = guide_colourbar(barwidth = 5, barheight = 0.5))+
  guides(color = F)+
  coord_equal()
p_map


taxa_df<-read_csv("./nab/data/nab_and_inat_taxa.csv")
df_nab<- read_rds("/data/ZHULAB/phenology/nab/nab_dat.rds")

taxa_list<-c( "Quercus", "Betula", "Populus", "Morus","Ulmus early", "Ulmus late" , "Fraxinus", "Acer", "Cupressaceae","Pinaceae","Ambrosia", "Poaceae early", "Poaceae late")

meta_df<-df_nab %>% 
  filter (taxa%in% str_split(taxa_list, pattern=" ", simplify = T) [,1]) %>% 
  drop_na(count) %>% 
  filter(count>0) %>% 
  group_by( station, location, lat, lon, id) %>% 
  summarise(mindate=min(Date),
            maxdate=max(Date),
            n=n()) %>% 
  mutate(range=maxdate-mindate) %>%
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(site=NA)

site_list<-c("NY", "SJ", "AT", "ST",   "HT",  "TP", "DT", "DV", "KC", "SL")
sitename_list<-c("New York", "San Jose", "Austin", "Seattle",  "Houston", "Tampa", "Detroit", "Denver", "Kansas City", "St. Louis")
region_list<-c("NE", "CA", "SE", "PNW","SE", "SE", "NE" , "MT", "NE", "NE")
regions<-c("NE", "SE", "CA", "PNW", "MT")

meta_df$site[meta_df$id==26]<-"NY"
meta_df$site[meta_df$id==7]<-"SJ"
meta_df$site[meta_df$id==5]<-"AT"
meta_df$site[meta_df$id==33]<-"ST"
# meta_df$site[meta_df$id==43]<-"BL" # wrong city in the inventory
# meta_df$site[meta_df$id==19]<-"HT"
meta_df$site[meta_df$id==2]<-"HT"
meta_df$site[meta_df$id==48]<-"TP"
meta_df$site[meta_df$id==12]<-"DV"
meta_df$site[meta_df$id==47]<-"DT"
meta_df$site[meta_df$id==17]<-"KC"
meta_df$site[meta_df$id==37]<-"SL"

meta_df<-meta_df %>% 
  left_join(data.frame(site=site_list, sitename=sitename_list, region=region_list), by="site")

p_pollen_map<-p_map+
  geom_point(data=meta_df, aes(x=lon, y=lat), pch=10, color="black", cex=3)+ 
  geom_label_repel(data=meta_df %>% filter(site %in% site_list), aes(x=lon, y=lat, label=sitename))+
  geom_point(data=meta_df %>% filter(site %in% site_list), aes(x=lon, y=lat), pch=10, color="red", cex=3)+
  coord_equal()
p_pollen_map
cairo_pdf("./RS4flower/output/figures/map.pdf", height = 6, width = 12)
print(p_pollen_map)
dev.off()

cl <- makeCluster(length(site_list), outfile = "")
registerDoSNOW(cl)

trees_df_list<-foreach (site = site_list,
                        .packages = c("tidyverse", "rgdal")) %dopar% {
                          if (site=="KC"|site=="SL") {
                            # Download https://experience.arcgis.com/experience/3641cea45d614ab88791aef54f3a1849/page/Urban-Datamart/
                            # Manual: https://www.fia.fs.fed.us/library/database-documentation/urban/dbDescription/Urban_FIADB_User_Guides_Database_Description_ver3-0_2021_03_17.pdf
                            id_tree_df<-read_csv("/data/ZHULAB/phenology/StreetTrees/UrbanFIA/ID_TREE.csv") %>% 
                              filter(statecd==29) %>%  #Missouri
                              filter(countycd %in% case_when(site=="KC"~95, site=="SL"~c(189,510))) %>% 
                              dplyr::select(plotid,id=cn, spcd, statuscd) %>% 
                              group_by(id) %>% 
                              filter(sum(statuscd!=1)==0) %>% 
                              ungroup() %>% 
                              distinct(id, .keep_all = T) %>% 
                              dplyr::select(-statuscd)
                            id_plot_df<-read_csv("/data/ZHULAB/phenology/StreetTrees/UrbanFIA/ID_PLOT.csv") %>% 
                              dplyr::select(plotid, lat, lon )
                            ref_sp_df<-read_csv("/data/ZHULAB/phenology/StreetTrees/UrbanFIA/REF_SPECIES.csv") %>% 
                              dplyr::select(spcd, genus, species) %>% 
                              mutate(species=paste(genus, species))
                            trees_df<-id_tree_df %>% 
                              left_join(id_plot_df, by="plotid") %>% 
                              left_join(ref_sp_df, by="spcd") %>% 
                              dplyr::select(-plotid, -spcd) %>%
                              mutate(site=site)
                          }
                          if (site=="DT") {
                            trees_df<-read_csv("/raid/users/ysong67/GitHub/phenology/RS4flower/data/Detroit_oak_pheno_obs_spring_2017.csv") %>% 
                              distinct(id=tree, species=Species,x, y)
                            
                            pts<-SpatialPoints(trees_df[, c("x","y")],
                                               proj4string = CRS("+init=EPSG:3857"))
                            # +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs
                            pts_reproj<-spTransform(pts,
                                                    CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
                            
                            trees_df<-cbind(trees_df %>% dplyr::select(-x, -y), coordinates(pts_reproj)) %>% 
                              rename(lat=y, lon=x) %>% 
                              as_tibble() %>%
                              mutate(site=site)
                          }
                          if (site=="DV") {
                            trees_df<-read_csv("/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_Denver.csv") %>% 
                              dplyr::select( id=SITE_ID, species=SPECIES_BO, time=INVENTORY_DATE, lat=Y_LAT, lon=X_LONG) %>%
                              arrange(desc(time)) %>%
                              distinct(id,species, .keep_all = T)  %>%
                              dplyr::select(-time) %>% 
                              filter(lon!=0) %>% 
                              mutate(site=site)
                          }
                          # if (site=="SL") {
                          #   trees_df<-read_csv("/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_Salem/Tree_Inventory_Salem.csv") %>%
                          #     mutate(address=paste0(Street_Number, " ", Street_Name, " Salem, MA")) %>%
                          #     dplyr::select(genus=Genus, species=Species, address) %>%
                          #     mutate(species=paste0(genus, " ",species)) %>%
                          #     dplyr::select(-genus) %>%
                          #     mutate(id=row_number())
                          #   
                          #   # cl <- makeCluster(20, outfile = "")
                          #   # registerDoSNOW(cl)
                          #   # address_df<-foreach(address = trees_df$address,
                          #   #         .combine = "rbind",
                          #   #         .packages = c("tidyverse", "ggmap")) %dopar% {
                          #   #   register_google(key = "REMOVED")
                          #   #   df<-cbind(address=address,
                          #   #             geocode(address))
                          #   #   df
                          #   #         }
                          #   # stopCluster(cl)
                          #   # write_csv(address_df, "/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_Salem/address reference.csv")
                          #   address_df<-read_csv("/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_Salem/address reference.csv")
                          #   trees_df<-trees_df %>%
                          #     left_join(address_df, by="address") %>%
                          #     dplyr::select(-address) %>%
                          #     mutate(site=site)
                          #   
                          # }
                          if (site=="TP") { #tree present field
                            trees_df<-read_csv("/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_Tampa.csv") %>%
                              dplyr::select(id=`Tree Id`, genus=Genus, species=Species, lat=`Point Y`, lon=`Point X`) %>%
                              mutate(species=paste0(genus, " ",species)) %>%
                              dplyr::select(-genus) %>%
                              mutate(site=site)
                          }
                          if (site=="HT") {
                            trees_df<-read_csv("/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_Houston/houston-texas-street-tree-inventory.csv") %>%
                              dplyr::select( species_common=Species, X=Shape_X, Y=Shape_Y) %>%
                              rowwise() %>%
                              mutate(species_common_clean=case_when(str_detect(species_common, ", spp.")~str_replace(species_common, ",", ""),
                                                                    (str_detect(species_common, ", ")&!str_detect(species_common, ", spp.") )~paste0(str_split(species_common, ", ")[[1]][2], " ",str_split(species_common, ", ")[[1]][1]),
                                                                    TRUE~species_common)) %>%
                              ungroup() %>%
                              mutate(id=row_number())
                            
                            pts<-SpatialPoints(trees_df[, c("X","Y")],
                                               proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"))
                            pts_reproj<-spTransform(pts,
                                                    CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
                            
                            trees_df<-cbind(trees_df %>% dplyr::select(-X, -Y), coordinates(pts_reproj) ) %>%
                              rename(lat=Y, lon=X)
                            
                            # library(taxize)
                            # species_df<-trees_df %>%
                            #   distinct(species_common_clean) %>%
                            #   rowwise() %>%
                            #   mutate(species=comm2sci(species_common_clean, simplify=T)) %>%
                            #   mutate(species=species[1]) %>%
                            #   ungroup()
                            # write_csv(species_df, "/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_Houston/species_reference.csv")
                            species_df<-read_csv("/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_Houston/species_reference.csv")
                            
                            trees_df<-trees_df %>%
                              left_join(species_df, by="species_common_clean") %>%
                              dplyr::select(-species_common, -species_common_clean) %>%
                              mutate(site=site)
                            
                          }
                          # if (site=="BL") {
                          #   shape <- readOGR(dsn = "/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_Bellevue/CITYTREE.shp")
                          #   shape_reproj<-spTransform(shape,
                          #                             CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
                          #   
                          #   trees_df<-as.data.frame(shape_reproj) %>%
                          #     dplyr::select(id=CityTreeID, species_id=SpeciesNbr, lat=coords.x2, lon=coords.x1, time=LastInspec ) %>%
                          #     mutate(id=as.numeric(id)) %>%
                          #     arrange(desc(time)) %>%
                          #     distinct(id, .keep_all = T)  %>%
                          #     dplyr::select(-time)
                          #   
                          #   # species_df<-trees_df %>%
                          #   #   distinct(species_id) %>%
                          #   #   mutate(species=NA)
                          #   # write_csv(species_df, "/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_Bellevue/species_reference.csv")
                          #   # manually filled in species name
                          #   species_df<-read_csv( "/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_Bellevue/species_reference.csv")
                          #   
                          #   trees_df<-trees_df %>%
                          #     mutate(species_id=as.numeric(species_id)) %>%
                          #     left_join(species_df, by="species_id") %>%
                          #     dplyr::select(-species_id) %>%
                          #     mutate(site=site)
                          # }
                          
                          if (site=="NY") {
                            trees_df<-read_csv("/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_NewYork.csv") %>%
                              dplyr::select(id=tree_id, species=spc_latin, lat=latitude, lon=longitude ) %>%
                              mutate(site=site)
                          }
                          if (site=="AT") {
                            trees_df<-read_csv("/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_Austin/Tree_Inventory_Austin.csv") %>%
                              dplyr::select(id=OBJECTID, species=SPECIES, coordinates= the_geom) %>%
                              mutate(coordinates=str_replace(coordinates, "POINT \\(", "")) %>%
                              mutate(coordinates=str_replace(coordinates, "\\)", "")) %>%
                              rowwise() %>%
                              mutate(lon=str_split(coordinates,pattern = " ", simplify = T)[1],
                                     lat=str_split(coordinates,pattern = " ", simplify = T)[2]) %>%
                              ungroup() %>%
                              mutate(lon=as.numeric(lon),
                                     lat=as.numeric(lat)) %>%
                              dplyr::select(-coordinates) %>%
                              mutate(site=site)
                          }
                          if (site=="SJ") {
                            trees_df<-read_csv("/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_SanJose/Tree_Inventory_SanJose.csv") %>%
                              dplyr::select(id=OBJECTID, species=NAMESCIENTIFIC, Y=Y, X=X )
                            
                            # shape <- readOGR(dsn = "/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_SanJose/Street_Tree.shp")
                            # proj4string(shape)
                            projection_sj<-"+proj=lcc +lat_0=36.5 +lon_0=-120.5 +lat_1=38.4333333333333 +lat_2=37.0666666666667 +x_0=2000000.0001016 +y_0=500000.0001016 +datum=NAD83 +units=us-ft +no_defs"
                            pts<-SpatialPoints(trees_df[, c("X","Y")],
                                               proj4string = CRS(projection_sj))
                            pts_reproj<-spTransform(pts,
                                                    CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
                            
                            trees_df<-cbind(trees_df %>% dplyr::select(-X, -Y), coordinates(pts_reproj) ) %>%
                              rename(lat=Y, lon=X)%>%
                              mutate(site=site)
                          }
                          
                          if (site=="ST") {
                            trees_df<-read_csv("/data/ZHULAB/phenology/StreetTrees/Tree_Inventory_Seattle.csv") %>%
                              dplyr::select(id=OBJECTID, species=SCIENTIFIC_NAME, lat=Y, lon=X ) %>%
                              mutate(site=site)
                          }
                          trees_df
                        }

stopCluster(cl)

trees_df<-bind_rows(trees_df_list) %>%
  rowwise() %>%
  mutate(genus=str_split(species,pattern = " ", simplify = T)[1]) %>%
  ungroup() %>%
  distinct(id, site, .keep_all = T) %>%
  mutate(genus_id=as.integer(as.factor(genus)))

library(taxize)
genus_to_family<-trees_df %>%
  distinct(genus) %>% 
  drop_na() %>% 
  mutate(family=tax_name(query = genus, get = "family", messages = F, db="ncbi")$family)
write_rds(genus_to_family, "./RS4flower/data/genus_to_family.rds")
genus_to_family<-read_rds("./RS4flower/data/genus_to_family.rds")

trees_df<-trees_df %>% 
  left_join(genus_to_family, by="genus")
write_rds(trees_df, "./RS4flower/data/street_trees.rds")

trees_df<-read_rds("./RS4flower/data/street_trees.rds")

p_tree_genus_ny<-ggplot()+
  geom_point(data=trees_df %>% 
               filter(site=="NY") %>% 
               filter(genus==taxaoi) ,aes(x=lon, y=lat), col="#2D6EB0",alpha=0.01)+
  geom_point(data=meta_df %>% filter(site=="NY"),aes(x=lon, y=lat),cex=10,pch=10)+
  ggtitle("New York")+
  theme_void()+
  coord_equal()
p_tree_genus_ny

p_tree_genus_sj<-ggplot()+
  geom_point(data=trees_df %>% 
               filter(site=="SJ") %>% 
               filter(genus==taxaoi) ,aes(x=lon, y=lat), col="#2D6EB0",alpha=0.25)+
  geom_point(data=meta_df %>% filter(site=="SJ"),aes(x=lon, y=lat),cex=10,pch=10)+
  ggtitle("San Jose")+
  theme_void()+
  coord_equal()
p_tree_genus_sj

library(magick)
conceptual<-image_read("./RS4flower/output/figures/conceptual.jpg")
p_conceptual<-image_ggplot(conceptual)
p_conceptual
