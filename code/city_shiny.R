rsconnect::setAccountInfo(
  name = "yiluansong",
  token = "095CAA728048A2F23867FA44B16C92F8",
  secret = "xZ99vo7in7UIKIcAG0kBoFi8adeeBu0xTNef0dLd"
)
if (deplpy_shiny) {
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
