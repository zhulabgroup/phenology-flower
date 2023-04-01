flat_better <- function(ts, doy = (274 - 365):(365 + 151), k = 50) {
  fit_list <- vector(mode = "list")

  # fit simple linear regression model
  fit0 <- lm(ts ~ doy, data = data.frame(doy, ts))
  fit_list[[1]] <- data.frame(AIC = AIC(fit0, k = k), model = "fit0")

  # fit piecewise regression model to original model with different numbers of breakpoints
  try( # sometimes fitting may fail
    {
      fit1 <- segmented::segmented(fit0, seg.Z = ~doy, npsi = 1, it = 10, control = segmented::seg.control(seed = 42, fix.npsi = T))
      fit_list[[2]] <- data.frame(AIC = AIC(fit1, k = k), model = "fit1")
    },
    silent = T
  )

  try(
    {
      fit2 <- segmented::segmented(fit0, seg.Z = ~doy, npsi = 2, it = 10, control = segmented::seg.control(seed = 42, fix.npsi = T))
      fit_list[[3]] <- data.frame(AIC = AIC(fit2, k = k), model = "fit2")
    },
    silent = T
  )

  try(
    {
      fit3 <- segmented::segmented(fit0, seg.Z = ~doy, npsi = 3, it = 10, control = segmented::seg.control(seed = 42, fix.npsi = T))
      fit_list[[4]] <- data.frame(AIC = AIC(fit3, k = k), model = "fit3")
    },
    silent = T
  )

  # rank by AIC. k=50 sets a large penalty for more complex models
  aic_df <- bind_rows(fit_list) %>%
    arrange(AIC, model)

  # return 1 when straight line is the best fit
  better <- aic_df %>%
    head(1) %>%
    pull(model) == "fit0"
  return(better)
}
