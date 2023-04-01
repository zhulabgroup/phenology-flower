get_doy <- function(thres_df, ts_df, idoi) {
  px_doy <- ts_df %>%
    filter(id == idoi) %>%
    filter(var == "enhanced vegetation index (PS)") %>%
    pull(doy)

  if (length(px_doy[px_doy >= 1 & px_doy <= 365]) < 50) {
    flower_df_up <- flower_df_down <- NULL
    print("too few data points")
  } else {
    px_evi <- ts_df %>%
      filter(id == idoi) %>%
      filter(var == "enhanced vegetation index (PS)") %>%
      pull(value)
    px_evi_in <- approx(px_doy, px_evi, (274 - 365):(365 + 151), rule = 1)$y

    px_evi_in_sm <- whitfun(px_evi_in, 30)

    flatbetter <- flat_better(px_evi_in_sm, k = 50)

    ### green down
    thres_list_down <- thres_df %>%
      filter(direction == "down") %>%
      pull(threshold)
    if (length(thres_list_down) == 0) {
      flower_df_down <- NULL
    } else {
      max_evi <- quantile(px_evi_in_sm[(-274 + 365 + 1):(-274 + 365 + 365 + 1)], 1, na.rm = T)
      start_doy <- which(!is.na(px_evi_in_sm[(-274 + 365 + 1):(-274 + 365 + 365 + 1)]) & px_evi_in_sm[(-274 + 365 + 1):(-274 + 365 + 365 + 1)] >= max_evi) %>% max() - 274 + 365
      min_evi <- quantile(px_evi_in_sm[start_doy:length(px_evi_in_sm)], 0, na.rm = T)
      min_doy <- which(!is.na(px_evi_in_sm[start_doy:length(px_evi_in_sm)]) & px_evi_in_sm[start_doy:length(px_evi_in_sm)] <= min_evi) %>% min() + start_doy
      end_doy <- min_doy

      param_ok2 <- (end_doy > start_doy) & (!flatbetter)

      if (!param_ok2) {
        greendown_doy <- rep(NA, length(thres_list_down))
        start_doy <- NA
        end_doy <- NA
        print("not typical growth curve")
      } else {
        greendown_thres <- (max_evi - min_evi) * thres_list_down + min_evi
        greendown_doy <- rep(NA, length(greendown_thres))
        for (t in 1:length(greendown_thres)) {
          greendown_doy[t] <- which(px_evi_in_sm[start_doy:end_doy] <= greendown_thres[t]) %>% min() + start_doy
        }
        start_doy <- start_doy + 274 - 365
        end_doy <- end_doy + 274 - 365
        greendown_doy <- greendown_doy + 274 - 365
      }
      flower_df_down <- data.frame(id = idoi, start = start_doy, end = end_doy, direction = "down", thres = thres_list_down, doy = greendown_doy)
    }

    ### green up
    thres_list_up <- thres_df %>%
      filter(direction == "up") %>%
      pull(threshold)
    if (length(thres_list_up) == 0) {
      flower_df_up <- NULL
    } else {
      max_evi <- quantile(px_evi_in_sm[(-274 + 365 + 1):(-274 + 365 + 365 + 1)], 1, na.rm = T)
      end_doy <- which(!is.na(px_evi_in_sm[(-274 + 365 + 1):(-274 + 365 + 365 + 1)]) & px_evi_in_sm[(-274 + 365 + 1):(-274 + 365 + 365 + 1)] >= max_evi) %>% min() - 274 + 365
      min_evi <- quantile(px_evi_in_sm[1:end_doy], 0.00, na.rm = T)
      min_doy <- which(!is.na(px_evi_in_sm[1:end_doy]) & px_evi_in_sm[1:end_doy] <= min_evi) %>% max()
      start_doy <- min_doy

      param_ok2 <- (end_doy > start_doy) & (!flatbetter)

      if (!param_ok2) {
        greenup_doy <- rep(NA, length(thres_list_up))
        start_doy <- NA
        end_doy <- NA
        print("not typical growth curve")
      } else {
        greenup_thres <- (max_evi - min_evi) * thres_list_up + min_evi
        greenup_doy <- rep(NA, length(greenup_thres))
        for (t in 1:length(greenup_thres)) {
          greenup_doy[t] <- which(px_evi_in_sm[start_doy:end_doy] >= greenup_thres[t]) %>% min() + start_doy
        }
        start_doy <- start_doy + 274 - 365
        end_doy <- end_doy + 274 - 365
        greenup_doy <- greenup_doy + 274 - 365
      }
      flower_df_up <- data.frame(id = idoi, start = start_doy, end = end_doy, direction = "up", thres = thres_list_up, doy = greenup_doy)
    }
  }

  flower_df <- bind_rows(flower_df_up, flower_df_down)

  return(flower_df)
}
