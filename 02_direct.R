calc_direct_manual <- function(samp) {
  out <- aggregate(y ~ area, samp, mean)
  names(out)[2] <- "direct_mean"
  out
}
