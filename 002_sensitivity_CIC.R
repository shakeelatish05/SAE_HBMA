# ==============================================================================
# CORRECTED EXPANDED INTERVAL SENSITIVITY RUNNER: MULTI-SCENARIO (R = 500)
# ==============================================================================
library(dplyr)
library(openxlsx)

target_dir <- "G:/My Drive/BMA_2025/Simulation/Output"
if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}
#Fixing other parameters e.g. D=10, rh0.1re_dis=Normal
# --- Isolate Four Selective Factored Scenarios ---
scen_sparse_small <- scenario_grid[33, ]  # Sparse DGP, Small Sample
scen_sparse_large <- scenario_grid[41, ]  # Sparse DGP, Large Sample
scen_dense_small  <- scenario_grid[49, ]  # Dense DGP, Small Sample
scen_dense_large  <- scenario_grid[57, ]  # Dense DGP, Large Sample

testing_batch <- list(
  "Sparse + Small Sample" = scen_sparse_small,
  "Sparse + Large Sample" = scen_sparse_large,
  "Dense + Small Sample"  = scen_dense_small,
  "Dense + Large Sample"  = scen_dense_large
)

R_sens <- 50  # Full replication benchmark to smooth away simulation noise
A_draws <- 2000

interval_sensitivity_results <- list()

cat("======================================================================\n")
cat("LAUNCHING EXPANDED INTERVAL ANALYSIS GRIDS (R = 500)\n")
cat("======================================================================\n\n")

for (scen_label in names(testing_batch)) {
  scen_profile <- testing_batch[[scen_label]]
  
  cat(sprintf("Processing Factored Cohort: %s ...\n", scen_label))
  
  rep_tracks <- lapply(1:R_sens, function(r) {
    set.seed(r + 8000)
    Dat <- gen_data(scen_profile)
    
    # Run core engine to extract point estimates, variance, and quantiles
    HBMA_out <- HBMA_unit(
      pop = Dat$pop, 
      samp = Dat$samp, 
      covars_list = list(c("x1"), c("x2"), c("x1", "x2"), c("x1", "x2", "x12")),
      rho_grid = seq(0.05, 0.95, length = 40),
      A = A_draws
    )
    
    true_means <- aggregate(y ~ area, Dat$pop, mean)$y
    
    # --------------------------------------------------------------------------
    # FIXED EXTRACTION: OPTION B (PROPOSED QUANTILE-BASED CIs)
    # --------------------------------------------------------------------------
    # Coerce to an explicit matrix and apply safe row-index dimension tracking
    CI_matrix <- as.matrix(HBMA_out$CI)
    if (nrow(CI_matrix) == 2) {
      optB_lower <- CI_matrix[1, ]
      optB_upper <- CI_matrix[2, ]
    } else {
      optB_lower <- CI_matrix[, 1]
      optB_upper <- CI_matrix[, 2]
    }
    
    optB_coverage <- mean(true_means >= optB_lower & true_means <= optB_upper)
    optB_length   <- mean(optB_upper - optB_lower)
    
    # --------------------------------------------------------------------------
    # EXTRACTION: OPTION A (SYMMETRIC VARIANCE-T CIs USING EQ 19)
    # --------------------------------------------------------------------------
    df_residual <- nrow(Dat$samp) - 4
    t_multiplier <- qt(0.975, df = df_residual)
    
    optA_lower <- HBMA_out$mean - t_multiplier * sqrt(HBMA_out$var)
    optA_upper <- HBMA_out$mean + t_multiplier * sqrt(HBMA_out$var)
    
    optA_coverage <- mean(true_means >= optA_lower & true_means <= optA_upper)
    optA_length   <- mean(optA_upper - optA_lower)
    
    data.frame(
      optB_cov = optB_coverage, optB_len = optB_length,
      optA_cov = optA_coverage, optA_len = optA_length
    )
  })
  
  df_reps <- do.call(rbind, rep_tracks)
  
  # Append Option B row summary
  interval_sensitivity_results[[length(interval_sensitivity_results) + 1]] <- data.frame(
    Scenario      = scen_label,
    Method        = "Option B: Proposed Quantile Mixture",
    Mean_Coverage = mean(df_reps$optB_cov, na.rm = TRUE),
    Avg_CI_Length = mean(df_reps$optB_len, na.rm = TRUE)
  )
  
  # Append Option A row summary
  interval_sensitivity_results[[length(interval_sensitivity_results) + 1]] <- data.frame(
    Scenario      = scen_label,
    Method        = "Option A: Symmetric Variance-t",
    Mean_Coverage = mean(df_reps$optA_cov, na.rm = TRUE),
    Avg_CI_Length = mean(df_reps$optA_len, na.rm = TRUE)
  )
}

Master_Interval_Table_Expanded <- do.call(rbind, interval_sensitivity_results)

# ---- Save Standalone Table S7 Excel Workbook to Drive ----
file_path_s7 <- file.path(target_dir, "Interval_Sensitivity_Table_S7.xlsx")
openxlsx::write.xlsx(Master_Interval_Table_Expanded, file = file_path_s7, overwrite = TRUE)

cat("======================================================================\n")
cat("✅ EXPANDED INTERVAL COMPARISON MATRIX EXPORTED SUCCESSFULLY\n")
cat("   Spreadsheet successfully saved to: ", file_path_s7, "\n")
cat("======================================================================\n\n")

print(Master_Interval_Table_Expanded)
