# ==============================================================================
# UPDATED SENSITIVITY RUNNER LOOP: GRID RESOLUTION (G) × POSTERIOR DRAWS (A)
# ==============================================================================
library(dplyr)
library(openxlsx)

target_dir <- "G:/My Drive/BMA_2025/Simulation/Output"
if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}
##D=10, rho=0.1 and re_dist Normal
# --- Extract Stated Scenarios from Existing Grid ---
scen_sparse_small <- scenario_grid[33, ]
scen_dense_large   <- scenario_grid[49, ]

testing_batch <- list(
  "Sparse" = scen_sparse_small,
  "Dense"  = scen_dense_large
)

R_sens <- 500 
grid_options  <- c(10, 40, 100)
draw_options  <- c(500, 1000, 2000)

sensitivity_matrix <- list()

cat("======================================================================\n")
cat("LAUNCHING FRAMEWORK SENSITIVITY GRID LOOPS (R = 500)\n")
cat("======================================================================\n\n")

for (scen_label in names(testing_batch)) {
  scen_profile <- testing_batch[[scen_label]]
  
  for (G in grid_options) {
    for (A in draw_options) {
      
      cat(sprintf("Processing: %s | G = %d | A = %d\n", scen_label, G, A))
      current_grid <- seq(0.05, 0.95, length = G)
      
      rep_metrics <- lapply(1:R_sens, function(r) {
        
        set.seed(r + 1000) 
        Dat <- gen_data(scen_profile) 
        
        # Run the engine, varying ONLY the inner grid and A parameters
        HBMA_out <- HBMA_unit(
          pop = Dat$pop, 
          samp = Dat$samp, 
          covars_list = list(c("x1"), c("x2"), c("x1", "x2"), c("x1", "x2", "x12")),
          rho_grid = current_grid,
          A = A
        )
        
        true_means  <- aggregate(y ~ area, Dat$pop, mean)$y
        
        # ---- RESOLVES THE FUNCTION LOOK-UP BUG: Inline Direct Calculation ----
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
      
      sensitivity_matrix[[length(sensitivity_matrix) + 1]] <- data.frame(
        Scenario      = scen_label,
        Grid_G        = G,
        Draws_A       = A,
        Mean_Bias     = mean(df_reps$Bias, na.rm = TRUE),
        Mean_RMSE     = mean(df_reps$RMSE, na.rm = TRUE),
        Coverage      = mean(df_reps$Coverage, na.rm = TRUE),
        Avg_CI_Length = mean(df_reps$Length, na.rm = TRUE),
        RE            = mean(df_reps$RE, na.rm = TRUE)
      )
    }
  }
}

Master_Sensitivity_Table <- do.call(rbind, sensitivity_matrix)

# ---- Export to Standalone Spreadsheet ----
file_path_sens <- file.path(target_dir, "framework_sensitivity_G_A.xlsx")
openxlsx::write.xlsx(Master_Sensitivity_Table, file = file_path_sens, overwrite = TRUE)

cat("\n======================================================================\n")
cat("✅ REPLICATION COMPLETE: Pure grid sensitivity table saved safely.\n")
cat("   Destination file: ", file_path_sens, "\n")
cat("======================================================================\n\n")

print(Master_Sensitivity_Table)
