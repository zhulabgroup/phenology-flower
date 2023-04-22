if (FALSE) {
  library(tidyverse)
  library(ggrepel)
  library(cowplot)
  library(gridExtra)
  library(spocc)
  library(sf)
  library(rnpn)
  # library(devtools)
  # devtools::install_github("bevingtona/planetR")
  unloadNamespace("RJSONIO") # conflict with jsonlite
  library(planetR)
  library(httr)
  library(jsonlite)
  library(raster)
  library(stringr)
  library(pracma)
  library(foreach)
  library(doSNOW)
  library(ptw)
  library(EnvCpt)
  library(rgdal)
  library(mclust)
  library(npreg)
  library(nlme)
  library(readxl)
  library(fuzzyjoin)
  library(taxize)
  library(ptw)
  library(segmented)
  library(RhpcBLASctl)
  library(geosphere)
  library(nlme)
  library(shiny)
  library(rsconnect)
  library(ggpubr)
  library(scales)
  library(neonUtilities)
  # devtools::install_github("NEONScience/NEON-geolocation/geoNEON")
  library(geoNEON)
}

# pacman::p_unload("all")
pacman::p_load("tidyverse")
pacman::p_load("sf")
pacman::p_load("parallel")
pacman::p_load("doSNOW")
pacman::p_load("patchwork")

source("code/func_whit.R")

.deploy_shiny <- T
.fig_save <- T

.path <- list(
  out_fig = "figures/",
  neon = "data/NEON/",
  dat_other = "data/processed/",
  npn = "data/NPN/",
  ps = "data/PS/",
  res = "data/results/"
)

## PS related
# Set API
api_key <- "00f3d86639c84d908d7c97ec6917d655" # nhcarter
# api_key <- "e297230ef5914b65876bd5d1f1feac5b" #ysong67
# api_key <- "PLAK6f22afea2b6c4d7d8bd233e8556360c4" # xcui12

# Metadata filters
cloud_lim <- 1 # percent from 0-1
item_name <- "PSScene"
asset <- "ortho_analytic_4b_sr"
product_bundle <- "analytic_sr_udm2"
# (see https://developers.planet.com/docs/data/items-assets/)
# https://developers.planet.com/docs/apis/data/psscene3-4band-deprecation/


path_ps <- "./data/PS/"
