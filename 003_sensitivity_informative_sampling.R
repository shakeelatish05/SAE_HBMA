# ==============================================================================
# INFORMATIVE SAMPLING SENSITIVITY: DUAL-TIER LEVEL TRACKER
# ==============================================================================
library(MASS)
library(nlme)
library(mvtnorm)
library(dplyr)
library(openxlsx)

# --- Runtime Controls ---
R_inf <- 500 # Final run
A_draws <- 2000
target_dir <- "G:/My Drive/BMA_2025/Simulation/Output"
##D=10, rho=0.1 and re_dist=Normal
scen_indices <- c(1, 9, 49, 57)
scen_labels  <- c("Baseline Small", "Baseline Large", "Dense Small", "Dense Large")

# Map both informative intensity levels to test structural breakdown points
gamma_tiers <- c(Moderate = 0.20, Severe = 0.50)

# Ensure the embedded generator matches your workspace scoping
gen_data_informative <- function(scenario, Nd = 50, sigma_e2_true = 1, gamma_inf) {
  D        <- scenario$D
  rho_true <- scenario$rho_true
  re_dist  <- scenario$re_dist
  dgp_type <- scenario$dgp_type
  n_type   <- scenario$n_type
  
  n_vec <- get_n_vec(D, n_type)
  
  beta_true <- switch(
    dgp_type,
    baseline    = c(1, 2, 0, 0),
    interaction = c(1, 0, 0, 3),
    sparse      = c(1, 0, 3, 0),
    dense       = c(1, 3, 2, 2)
  )
  
  sigma_u2 <- rho_true / (1 - rho_true) * sigma_e2_true
  
  u_d <- switch(
    re_dist,
    normal = rnorm(D, 0, sqrt(sigma_u2)),
    skewed = rgamma(D, shape = 2, scale = sqrt(sigma_u2 / 2)) - sqrt(2 * sigma_u2)
  )
  
  pop <- do.call(rbind, lapply(seq_len(D), function(d) {
    x1  <- rnorm(Nd)
    x2  <- rnorm(Nd)
    x12 <- x1 * x2
    X   <- cbind(1, x1, x2, x12)
    y   <- as.vector(X %*% beta_true + u_d[d] + rnorm(Nd, 0, sqrt(sigma_e2_true)))
    data.frame(area = d, y, x1, x2, x12)
  }))
  pop$uid <- seq_len(nrow(pop))
  
  samp <- do.call(rbind, lapply(seq_len(D), function(d) {
    pop_d <- pop[pop$area == d, ]
    prob_selection <- exp(gamma_inf * pop_d$y)
    prob_selection <- prob_selection / sum(prob_selection)
    sampled_rows <- sample(nrow(pop_d), n_vec[d], prob = prob_selection)
    pop_d[sampled_rows, ]
  }))
  
  list(pop = pop, samp = samp, n_vec = n_vec, beta_true = beta_true, sigma_u2 = sigma_u2)
}

# --- Nested Dual-Tier Iteration Loop ---
dual_informative_collection <- list()

cat("======================================================================\n")
cat("LAUNCHING DUAL-TIER INFORMATIVE SENSITIVITY NESTED LOOPS\n")
cat("======================================================================\n\n")

for (g_name in names(gamma_tiers)) {
  g_val <- gamma_tiers[[g_name]]
  cat(sprintf(">>> STARTING EXTRACTION FOR COHORT: %s (gamma = %.2f) <<<\n\n", g_name, g_val))
  
  for (idx in seq_along(scen_indices)) {
    s_id  <- scen_indices[idx]
    s_lbl <- scen_labels[idx]
    scen  <- scenario_grid[s_id, ]
    
    cat(sprintf("   Processing Scenario %d / 4: %s ...\n", idx, s_lbl))
    
    rep_metrics <- lapply(1:R_inf, function(r) {
      set.seed(r + 6000)
      
      Dat <- gen_data_informative(scen, gamma_inf = g_val)
      
      HBMA_out <- HBMA_unit(
        pop = Dat$pop, 
        samp = Dat$samp, 
        covars_list = list(c("x1"), c("x2"), c("x1", "x2"), c("x1", "x2", "x12")),
        rho_grid = seq(0.05, 0.95, length = 40),
        A = A_draws
      )
      
      true_means  <- aggregate(y ~ area, Dat$pop, mean)$y
      direct_mean <- Dat$samp %>%
        dplyr::group_by(area) %>%
        dplyr::summarise(direct_mean = mean(y, na.rm = TRUE), .groups = "drop") %>%
        dplyr::pull(direct_mean)
      
      direct_rmse <- sqrt(mean((direct_mean - true_means)^2))
      hbma_rmse   <- sqrt(mean((HBMA_out$mean - true_means)^2))
      
      data.frame(
        Bias     = mean(HBMA_out$mean - true_means),
        RMSE     = hbma_rmse,
        Coverage = mean(true_means >= HBMA_out$CI[1, ] & true_means <= HBMA_out$CI[2, ]),
        Length   = mean(HBMA_out$CI[2, ] - HBMA_out$CI[1, ]),
        RE       = direct_rmse / hbma_rmse
      )
    })
    
    df_reps <- do.call(rbind, rep_metrics)
    
    dual_informative_collection[[length(dual_informative_collection) + 1]] <- data.frame(
      Informativeness = g_name,
      Gamma_Value     = g_val,
      Scenario        = s_lbl,
      Scenario_ID     = s_id,
      Mean_Bias       = mean(df_reps$Bias, na.rm = TRUE),
      Mean_RMSE       = mean(df_reps$RMSE, na.rm = TRUE),
      Coverage        = mean(df_reps$Coverage, na.rm = TRUE),
      Avg_CI_Length   = mean(df_reps$Length, na.rm = TRUE),
      RE              = mean(df_reps$RE, na.rm = TRUE)
    )
  }
  cat("\n")
}

Master_Dual_Informative_Table <- do.call(rbind, dual_informative_collection)

# ---- Save Standalone Excel Spreadsheet to Drive ----
file_path_dual_inf <- file.path(target_dir, "dual_informative_sampling_sensitivity.xlsx")
openxlsx::write.xlsx(Master_Dual_Informative_Table, file = file_path_dual_inf, overwrite = TRUE)

cat("======================================================================\n")
cat("✅ DUAL-TIER INFORMATIVE SAMPLING PROFILE CONCLUDED SUCCESSFULLY\n")
cat("   Spreadsheet exported to: ", file_path_dual_inf, "\n")
cat("======================================================================\n\n")

print(Master_Dual_Informative_Table)
