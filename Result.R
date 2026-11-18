# ==============================================================================
# STUDY REPLICATION SCRIPT: RUNNING & EXTRACTING SCENARIOS 21 - 30 (R = 500)
# ==============================================================================
library(MASS)
library(nlme)
library(mvtnorm)
library(dplyr)
library(tidyr)
library(openxlsx)

# --- Global Parameter Initialization ---
R <- 500
A_mcmc <- 2000

# Verify that the scenario grid and simulation components exist in the workspace
if (!exists("scenario_grid")) {
  stop("Master object 'scenario_grid' not detected. Please ensure '06_scenarios.R' is loaded.")
}
if (!exists("run_simulation")) {
  stop("Function 'run_simulation' not detected. Please ensure '07_run_simulation.R' is loaded.")
}

# ---- Updated: Isolate Rows 21 through 30 from the Factorial Grid ----
scenario_subset <- scenario_grid[21:30, ]

cat("======================================================================\n")
cat("LAUNCHING BATCH PROCESSING FOR SCENARIOS 21 THROUGH 30 (R = 500)\n")
cat("======================================================================\n\n")

res_list <- lapply(seq_len(nrow(scenario_subset)), function(i) {
  scen <- scenario_subset[i, ]
  
  cat(sprintf("Starting scenario %d / %d ...\n", i, nrow(scenario_subset)))
  print(scen)
  
  # Execute the 500-replication nested calculation loop
  res <- run_simulation(scen, R = R)
  
  cat(sprintf("Completed scenario %d: %d replications finished\n\n", i, R))
  res
})

# ---- Assign Clean Scenario Index String Names ----
names(res_list) <- paste0("scen", 21:30)

# ==============================================================================
# DATA SUMMARY AGGREGATION LOOP
# ==============================================================================
scenario_summary <- lapply(names(res_list), function(scen_name) {
  
  # Stack the 500 independent replication data frames together using base R binding
  res_df <- do.call(rbind, res_list[[scen_name]])
  
  # Explicitly compute area-wide averages using dplyr namespace mapping
  res_df %>%
    dplyr::group_by(method) %>%
    dplyr::summarise(
      Mean_Bias        = mean(Bias, na.rm = TRUE),
      Mean_RMSE        = mean(RMSE, na.rm = TRUE),
      Coverage         = mean(Coverage, na.rm = TRUE),
      Avg_CI_Length    = mean(Avg_CI_Length, na.rm = TRUE),
      PMP_Model1       = mean(PMP_M1, na.rm = TRUE),
      PMP_Model2       = mean(PMP_M2, na.rm = TRUE),
      PMP_Model3       = mean(PMP_M3, na.rm = TRUE),
      PMP_Model4       = mean(PMP_M4, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(Scenario = scen_name)
})

# Combine the individual scenario blocks into a single master summary data frame
Scenarios21_30 <- do.call(rbind, scenario_summary)

# ==============================================================================
# ISOLATED FILE EXPORT PROTOCOL
# ==============================================================================
# Ensure Output target directory exists safely
if (!dir.exists("Output")) {
  dir.create("Output")
}

# Define a completely unique, standalone file destination path to prevent overwrites
isolated_file_path <- "Output/scenario_summary_21_30.xlsx"

# Create a fresh workbook container object
wb_isolated <- createWorkbook()
sheet_name_isolated <- "Scenarios21_30"
addWorksheet(wb_isolated, sheet_name_isolated)

# Write the data frame to the isolated workbook container
writeData(wb_isolated, sheet = sheet_name_isolated, Scenarios21_30)

# Export the standalone file safely to your Output directory
saveWorkbook(wb_isolated, file = isolated_file_path, overwrite = TRUE)

# ==============================================================================
# CONSOLE VERIFICATION SUMMARY PRINT
# ==============================================================================
cat("\n======================================================================\n")
cat("SUCCESS: Standalone spreadsheet successfully saved to:\n", isolated_file_path, "\n")
cat("======================================================================\n\n")

print(head(Scenarios21_30, 20))
print(tail(Scenarios21_30, 20))
