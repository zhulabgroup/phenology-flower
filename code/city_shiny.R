rsconnect::setAccountInfo(
  name = "yiluansong",
  token = "REMOVED",
  secret = "REMOVED"
)
if (.deploy_shiny) {
  for (taxaoi in v_taxa) {
    files <- list.files(paste0("./data/results/", taxaoi), pattern = "*.jpg", recursive = T, full.names = T)
    path_local <- paste0("./shinyapp/result_figs/", taxaoi)
    dir.create(path_local, recursive = T)
    file.copy(
      from = files, to = path_local,
      overwrite = TRUE, recursive = TRUE,
      copy.mode = TRUE
    )
  }
  rsconnect::deployApp(appDir = "./shinyapp", lint = F, account = "yiluansong", server = "shinyapps.io", appName = "RS4Flower_result", upload = T, forceUpdate = T)
}
