compare_methods <- function(Dat) {
  
  pop  <- Dat$pop
  samp <- Dat$samp
  
  true_means <- aggregate(y ~ area, pop, mean)$y
  direct     <- calc_direct_manual(samp)$direct_mean
  
  HB1 <- HB_unit(pop, samp, c("x1"), A = 2000)
  HB2 <- HB_unit(pop, samp, c("x1", "x2"), A = 2000)
  HB3 <- HB_unit(pop, samp, c("x1", "x2", "x12"), A = 2000)
  
  restricted_4_model_space <- list(
    c("x1"),             
    c("x2"),             
    c("x1", "x2"),       
    c("x1", "x2", "x12") 
  )
  HBMA <- HBMA_unit(pop, samp, covars_list = restricted_4_model_space, A = 2000)
  
  methods_list <- list(Direct = direct, HB1 = HB1$mean, HB2 = HB2$mean, HB3 = HB3$mean, HBMA = HBMA$mean)
  CI_list      = list(Direct = NULL, HB1 = HB1$CI, HB2 = HB2$CI, HB3 = HB3$CI, HBMA = HBMA$CI)
  
  out_df <- data.frame(method = names(methods_list), Bias = NA, RMSE = NA, Coverage = NA, Avg_CI_Length = NA, stringsAsFactors = FALSE)
  
  for (i in seq_along(methods_list)) {
    m_name <- out_df$method[i]
    est    <- methods_list[[m_name]]
    ci     <- CI_list[[m_name]]
    
    out_df$Bias[i] <- mean(est - true_means)
    out_df$RMSE[i] <- sqrt(mean((est - true_means)^2))
    
    if (!is.null(ci)) {
      out_df$Coverage[i]      <- mean(true_means >= ci[1, ] & true_means <= ci[2, ])
      out_df$Avg_CI_Length[i] <- mean(ci[2, ] - ci[1, ])
    }
  }
  
  ## ---- Robustness Fix: Track and Append Model PMPs ----
  out_df$PMP_M1 <- HBMA$pmp[1]
  out_df$PMP_M2 <- HBMA$pmp[2]
  out_df$PMP_M3 <- HBMA$pmp[3]
  out_df$PMP_M4 <- HBMA$pmp[4]
  
  return(out_df)
}
