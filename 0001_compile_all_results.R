# ==============================================================================
# STUDY REPLICATION SCRIPT: CONSOLIDATING MULTI-BATCH SCENARIOS INTO MASTER CSV
# ==============================================================================
library(openxlsx)
library(dplyr)

# Define the absolute directory paths
output_dir <- "Output"
master_csv_path <- "Output/complete_simulation_results.csv"

# List all standalone batch Excel files generated across your execution slots
# This covers your 1-10, 11-20, 21-30, 31-40, 41-50, and 51-60 file splits
target_files <- c(
  "scenario_summary_1_10.xlsx",
  "scenario_summary_11_20.xlsx",
  "scenario_summary_21_30.xlsx",
  "scenario_summary_31_40.xlsx",
  "scenario_summary_41_50.xlsx",
  "scenario_summary_51_60.xlsx"
)

all_batches_list <- list()

cat("======================================================================\n")
cat("LAUNCHING DATA CONSOLIDATION SYSTEM FOR ALL SIMULATION SCENARIOS\n")
cat("======================================================================\n\n")

for (f_name in target_files) {
  full_file_path <- file.path(output_dir, f_name)
  
  if (file.exists(full_file_path)) {
    cat("Reading batch spreadsheet container:", f_name, "...\n")
    
    # Extract available sheets within the active workbook container
    wb_sheets <- openxlsx::getSheetNames(full_file_path)
    
    for (sheet in wb_sheets) {
      # Read the sheet contents directly into a data frame
      sheet_df <- openxlsx::read.xlsx(full_file_path, sheet = sheet)
      
      # Coerce data types explicitly to match columns perfectly before binding
      sheet_df <- sheet_df %>%
        dplyr::mutate(
          method        = as.character(method),
          Mean_Bias     = as.numeric(Mean_Bias),
          Mean_RMSE     = as.numeric(Mean_RMSE),
          Coverage      = as.numeric(Coverage),
          Avg_CI_Length = as.numeric(Avg_CI_Length),
          PMP_Model1    = as.numeric(PMP_Model1),
          PMP_Model2    = as.numeric(PMP_Model2),
          PMP_Model3    = as.numeric(PMP_Model3),
          PMP_Model4    = as.numeric(PMP_Model4),
          Scenario      = as.character(Scenario)
        )
      
      # Append to the master tracking collection list object
      all_batches_list[[length(all_batches_list) + 1]] <- sheet_df
    }
  } else {
    cat("⚠️ NOTICE: Spreadsheet batch file omitted or not yet compiled:", f_name, "\n")
  }
}

if (length(all_batches_list) > 0) {
  # Stack all scenario blocks together seamlessly
  complete_results_df <- do.call(rbind, all_batches_list)
  
  # Ensure rows are sorted logically by scenario number and method hierarchy
  complete_results_df <- complete_results_df %>%
    dplyr::mutate(scen_num = as.numeric(gsub("scen", "", Scenario))) %>%
    dplyr::arrange(scen_num, match(method, c("Direct", "HB1", "HB2", "HB3", "HBMA"))) %>%
    dplyr::select(-scen_num)
  
  # ---- Export to Standalone Master CSV File ----
  write.csv(complete_results_df, file = master_csv_path, row.names = FALSE)
  
  cat("\n======================================================================\n")
  cat("✅ SUCCESS: Master data consolidation complete!\n")
  cat("   Total compiled scenario rows stacked:", nrow(complete_results_df), "\n")
  cat("   Master CSV database saved to:", master_csv_path, "\n")
  cat("======================================================================\n\n")
  
  # Print a structural verification preview
  print(head(complete_results_df, 10))
} else {
  stop("CRITICAL error: No source excel sheets found to compile. Verify your Output folder.")
}