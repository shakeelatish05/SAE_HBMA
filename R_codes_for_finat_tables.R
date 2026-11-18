

# ==============================================================================
# STUDY REPLICATION SCRIPT: GENERATING MAIN & SUPPLEMENTARY TABLES
# ==============================================================================
library(dplyr)
library(openxlsx)
library(readr)

# ---- Set Target Production Output Directory ----
target_dir <- "G:/My Drive/BMA_2025/Simulation/Output"
if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}
setwd(target_dir)

# ---- Load Complete Simulation Matrix File ----
# This reads directly from your single consolidated CSV database to ensure 
# perfect structural alignment with your 64 experimental factorial conditions.
csv_source_path <- file.path(target_dir, "complete_simulation_results.csv")

if (!file.exists(csv_source_path)) {
  stop("Master database 'complete_simulation_results.csv' not found in your Drive output folder.\n",
       "Please compile the CSV database first using 'compile_all_results.R'.")
}

master_results <- readr::read_csv(csv_source_path)

# ---- Reconstruct the Explicit Factor Grid Meta-Keys ----
# Since your finalized results script tracks your factors dynamically, we reconstruct 
# your meta-parameters from the original 'scenario_grid' properties using explicit indices.
# This eliminates your old 'expanded_data' matrix-slice replication layer completely.
if (!exists("scenario_grid")) {
  stop("Object 'scenario_grid' must be loaded in the active environment to map structural scenarios.")
}

grid_metadata <- scenario_grid %>%
  dplyr::mutate(
    scenario_id = dplyr::row_number(),
    Scenario    = paste0("scen", scenario_id)
  )

# Merge keys together via a 1-to-1 matching protocol
final_dataset <- master_results %>%
  dplyr::left_join(grid_metadata, by = "Scenario")

# ---- Compute Relative Efficiency (RE) Baseline Scores Safely ----
final_dataset_RE <- final_dataset %>%
  dplyr::group_by(scenario_id) %>%
  dplyr::mutate(
    # Pull the exact Direct baseline RMSE for this unique scenario ID
    RMSE_Direct = Mean_RMSE[method == "Direct"],
    RE          = dplyr::if_else(method != "Direct", RMSE_Direct / Mean_RMSE, NA_real_)
  ) %>%
  dplyr::ungroup()

# ==============================================================================
# PART 1: MAIN MANUSCRIPT TABLES RECONSTRUCTION (TABLES 1 TO 4)
# ==============================================================================

### TABLE 2 — Baseline Data-Generating Setup (Correct Specification)
table2_ids <- c(1, 3, 9, 11)
Table2 <- final_dataset_RE %>%
  dplyr::filter(scenario_id %in% table1_ids, dgp_type == "baseline") %>%
  dplyr::select(scenario_id, method, Mean_Bias, Mean_RMSE, Coverage, Avg_CI_Length, RE) %>%
  dplyr::arrange(scenario_id, match(method, c("Direct", "HB1", "HB2", "HB3", "HBMA")))


### TABLE 3 — Interaction Functional Misspecification
table3_ids <- c(17, 19, 21, 23)
Table3 <- final_dataset_RE %>%
  dplyr::filter(scenario_id %in% table3_ids) %>%
  dplyr::select(scenario_id, method, Mean_Bias, Mean_RMSE, Coverage, Avg_CI_Length, RE) %>%
  dplyr::arrange(scenario_id, match(method, c("Direct", "HB1", "HB2", "HB3", "HBMA")))

### TABLE 4 — Sparse vs. Dense Architectural Departures (KEY MANUSCRIPT RESULT)
table4_ids <- c(33, 39, 49, 55)
Table4 <- final_dataset_RE %>%
  dplyr::filter(scenario_id %in% table4_ids) %>%
  dplyr::select(scenario_id, method, Mean_Bias, Mean_RMSE, Coverage, Avg_CI_Length, RE) %>%
  dplyr::arrange(scenario_id, match(method, c("Direct", "HB1", "HB2", "HB3", "HBMA")))


### TABLE 5 — Baseline + Random Effect Misspecification (Distribution Stress)
table5_ids <- c(5, 7, 13, 15)
Table5 <- final_dataset_RE %>%
  dplyr::filter(scenario_id %in% table5_ids) %>%
  dplyr::select(scenario_id, method, Mean_Bias, Mean_RMSE, Coverage, Avg_CI_Length, RE) %>%
  dplyr::arrange(scenario_id, match(method, c("Direct", "HB1", "HB2", "HB3", "HBMA")))

## ---- Save Main Tables to Drive ----
file_path_main <- file.path(target_dir, "main_Tables.xlsx")
wb_main <- createWorkbook()

write_table_sheet <- function(wb, sheet_name, data) {
  if (sheet_name %in% names(wb)) {
    removeWorksheet(wb, sheet_name)
  }
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data)
}

write_table_sheet(wb_main, "Table2_Baseline", Table2)
write_table_sheet(wb_main, "Table5_MisspecRE", Table5)
write_table_sheet(wb_main, "Table3_Interaction", Table3)
write_table_sheet(wb_main, "Table4_SparseDense", Table4)
saveWorkbook(wb_main, file_path_main, overwrite = TRUE)

# ==============================================================================
# PART 2: SUPPLEMENTARY APPENDIX TABLES RECONSTRUCTION (TABLES S1a TO S1c)
# ==============================================================================

#### Table S1(b): Aggregation by Method × rho × n_type
Table_S1b_rho_n <- final_dataset_RE %>%
  dplyr::filter(method != "Direct") %>% 
  dplyr::group_by(method, rho_true, n_type) %>%
  dplyr::summarise(
    Mean_Bias      = mean(Mean_Bias, na.rm = TRUE),
    Mean_RMSE      = mean(Mean_RMSE, na.rm = TRUE),
    Mean_RE        = mean(RE, na.rm = TRUE),
    Coverage       = mean(Coverage, na.rm = TRUE),
    Mean_CI_Length = mean(Avg_CI_Length, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(match(method, c("HB1", "HB2", "HB3", "HBMA")), rho_true, n_type)

#### Table S1(c): Aggregation by Method × Domain Count m
# RESOLVES THE ERROR: Group by D, then rename to m for perfect table presentation
Table_S1c_m <- final_dataset_RE %>%
  dplyr::filter(method != "Direct") %>%
  dplyr::group_by(method, D) %>%  # <--- Uses the variable found in .data
  dplyr::summarise(
    Mean_Bias      = mean(Mean_Bias, na.rm = TRUE),
    Mean_RMSE      = mean(Mean_RMSE, na.rm = TRUE),
    Mean_RE        = mean(RE, na.rm = TRUE),
    Coverage       = mean(Coverage, na.rm = TRUE),
    Mean_CI_Length = mean(Avg_CI_Length, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::rename(m = D) %>%        # <--- Renames to m to match your text table exactly
  dplyr::arrange(match(method, c("HB1", "HB2", "HB3", "HBMA")), m)

#### Table S1(a): Aggregation by Method × Random Effect Distribution Structure
Table_S1a_REdist <- final_dataset_RE %>%
  dplyr::filter(method != "Direct") %>%
  dplyr::group_by(method, re_dist) %>%
  dplyr::summarise(
    Mean_Bias      = mean(Mean_Bias, na.rm = TRUE),
    Mean_RMSE      = mean(Mean_RMSE, na.rm = TRUE),
    Coverage       = mean(Coverage, na.rm = TRUE),
    Mean_CI_Length = mean(Avg_CI_Length, na.rm = TRUE),
    Mean_RE        = mean(RE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(match(method, c("HB1", "HB2", "HB3", "HBMA")), re_dist)

## ---- Save Supplementary Tables to Drive ----
file_path_supp <- file.path(target_dir, "Supplementary_Tables.xlsx")
wb_supp <- createWorkbook()

# Helper function to prevent sheet crashes or duplication warnings
write_table_sheet <- function(wb, sheet_name, data) {
  if (sheet_name %in% names(wb)) {
    removeWorksheet(wb, sheet_name)
  }
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data)
}

# FIXED: Aligned worksheet sheet targets precisely with your manuscript structure
write_table_sheet(wb_supp, "S1a_REdist", Table_S1a_REdist)
write_table_sheet(wb_supp, "S1b_Rho_nType", Table_S1b_rho_n)
write_table_sheet(wb_supp, "S1c_m", Table_S1c_m)
write_table_sheet(wb_supp, "S2_Full_Results", final_dataset_RE)

saveWorkbook(wb_supp, file = file_path_supp, overwrite = TRUE)

cat("\n======================================================================\n")
cat("✅ RECONSTRUCTION COMPLETION REPORT: SUPPLEMENTARY MATRICES UNIFIED\n")
cat("   Supplementary Workbook Saved to: ", file_path_supp, "\n")
cat("   Sheets included: S1a_REdist, S1b_Rho_nType, S1c_m, S2_Full_Results\n")
cat("======================================================================\n\n")







# ==============================================================================
# RENDER VISUALIZATIONS PANELS
# ==============================================================================
library(ggplot2)
library(dplyr)

# ---- Establish Explicit Output Directory Path Strings ----
target_dir  <- "G:/My Drive/BMA_2025/Simulation/Output"
figures_dir <- file.path(target_dir, "Figures")

if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

# ---- Prepare Consolidated Plotting Data Framework ----
# LOCKED: Returns the rho conditional check exactly to your stated 0.1 baseline value
final_dataset_plot <- final_dataset_RE %>%
  dplyr::mutate(
    n_group   = factor(n_type, levels = c("small", "large"), 
                       labels = c("Small Sample (DU(3,5))", "Large Sample (nd = 25)")),
    # RESTORED: Matches your exact 0.1 low-correlation manuscript baseline
    cor_group = factor(dplyr::if_else(rho_true == 0.1, "low", "high"), 
                       levels = c("low", "high"), 
                       labels = c("Low Correlation (rho = 0.1)", "High Correlation (rho = 0.6)")),
    method    = factor(method, levels = c("Direct", "HB1", "HB2", "HB3", "HBMA"))
  )




# ------------------------------------------------------------------------------
# FIGURE 1: Overall RMSE Distribution Panel
# ------------------------------------------------------------------------------
plot_rmse <- final_dataset_plot %>%
  ggplot2::ggplot(aes(x = method, y = Mean_RMSE, fill = method)) +
  ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = 1) +
  ggplot2::labs(x = "Estimation Method", y = "Mean RMSE", 
                title = "Distribution of Mean RMSE Across Simulation Scenarios") +
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.position = "none", title = element_text(size = 10))

ggplot2::ggsave(filename = file.path(figures_dir, "Overall_RMSE.png"), 
                plot = plot_rmse, width = 7, height = 5, dpi = 300)




# ------------------------------------------------------------------------------
# FIGURE 2: Overall Point Estimation Bias Panel
# ------------------------------------------------------------------------------
plot_bias <- final_dataset_plot %>%
  ggplot2::ggplot(aes(x = method, y = Mean_Bias, fill = method)) +
  ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = 1) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.6) +
  ggplot2::labs(x = "Estimation Method", y = "Mean Bias", 
                title = "Distribution of Mean Bias Across Simulation Scenarios") +
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.position = "none", title = element_text(size = 10))

ggplot2::ggsave(filename = file.path(figures_dir, "Overall_Bias.png"), 
                plot = plot_bias, width = 7, height = 5, dpi = 300)




# ------------------------------------------------------------------------------
# FIGURE 3: RMSE Comparison Separated by Sample Size Density Tiers
# ------------------------------------------------------------------------------
plot_rmse_sample <- ggplot2::ggplot(final_dataset_plot, aes(x = method, y = Mean_RMSE, fill = n_group)) +
  ggplot2::geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.75, outlier.shape = 1) +
  ggplot2::labs(x = "Estimation Method", y = "Mean RMSE", fill = "Sample Size Structure", 
                title = "RMSE Comparison Under Small and Large Sample Size Configurations") +
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.position = "top", panel.grid.minor = element_blank(), title = element_text(size = 10))

ggplot2::ggsave(filename = file.path(figures_dir, "RMSE_SampleSize.png"), 
                plot = plot_rmse_sample, width = 8, height = 5, dpi = 300)




# ------------------------------------------------------------------------------
# FIGURE 4: Point Bias Comparison Separated by Sample Size Density Tiers
# ------------------------------------------------------------------------------
plot_bias_sample <- ggplot2::ggplot(final_dataset_plot, aes(x = method, y = Mean_Bias, fill = n_group)) +
  ggplot2::geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.75, outlier.shape = 1) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.6) +
  ggplot2::labs(x = "Estimation Method", y = "Mean Bias", fill = "Sample Size Structure", 
                title = "Bias Comparison Under Small and Large Sample Size Configurations") +
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.position = "top", panel.grid.minor = element_blank(), title = element_text(size = 10))

ggplot2::ggsave(filename = file.path(figures_dir, "Bias_SampleSize.png"), 
                plot = plot_bias_sample, width = 8, height = 5, dpi = 300)




# ------------------------------------------------------------------------------
# FIGURE 5: RMSE Comparison Separated by Intraclass Clustering Strengths
# ------------------------------------------------------------------------------
plot_rmse_cor <- ggplot2::ggplot(final_dataset_plot, aes(x = method, y = Mean_RMSE, fill = cor_group)) +
  ggplot2::geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.75, outlier.shape = 1) +
  ggplot2::labs(x = "Estimation Method", y = "Mean RMSE", fill = "Clustering Intensity", 
                title = "RMSE Comparison Under Low and High Intraclass Correlation (rho)") +
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.position = "top", panel.grid.minor = element_blank(), title = element_text(size = 10))

ggplot2::ggsave(filename = file.path(figures_dir, "RMSE_Correlation.png"), 
                plot = plot_rmse_cor, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# FIGURE 6: Point Bias Comparison Separated by Intraclass Clustering Strengths
# ------------------------------------------------------------------------------
plot_bias_cor <- ggplot2::ggplot(final_dataset_plot, aes(x = method, y = Mean_Bias, fill = cor_group)) +
  ggplot2::geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.75, outlier.shape = 1) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.6) +
  ggplot2::labs(x = "Estimation Method", y = "Mean Bias", fill = "Clustering Intensity", 
                title = "Bias Comparison Under Low and High Intraclass Correlation (rho)") +
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.position = "top", panel.grid.minor = element_blank(), title = element_text(size = 10))

ggplot2::ggsave(filename = file.path(figures_dir, "Bias_Correlation.png"), 
                plot = plot_bias_cor, width = 8, height = 5, dpi = 300)

cat("\n======================================================================\n")
cat("✅ GRAPHICS SYSTEM RE-ALIGNED: Panels successfully configured for rho = 0.1\n")
cat("   Destination folder: ", figures_dir, "\n")
cat("======================================================================\n\n")











