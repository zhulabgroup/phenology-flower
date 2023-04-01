# Function for smoothing, because ptw::whit1 does not take NA in the time series
whitfun <- function(x, lambda) {
  max_id <- 0
  done <- F
  while (!done) {
    min_id <- min(which(!is.na(x[(max_id + 1):length(x)]))) + (max_id) # first number that is not NA
    if (min_id == Inf) { # all numbers are NA
      done <- T # consider this ts done
    } else {
      max_id <- min(which(is.na(x[min_id:length(x)]))) - 1 + (min_id - 1) # last number in the first consecutive non-NA segment
      if (max_id == Inf) {
        max_id <- length(x) # last non-NA segment is at the end of the whole ts
        done <- T # consider this ts done
      }
      x[min_id:max_id] <- ptw::whit1(x[min_id:max_id], lambda) # whitman smoothing for this non-NA segment
    }
  }
  return(x)
}
