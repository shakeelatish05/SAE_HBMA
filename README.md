# SAE_HBMA
Hierarchical Model Averaging Reproducible Codes
## Setup Note
Before running the scripts locally, update the `setwd(...)` path at the top of the driver scripts (e.g., `run_all.R`) to point to your local working directory, and ensure an `Output/` subfolder exists in that directory.
# Simulation Study: Unit-Level Hierarchical Bayesian Model Averaging (HBMA)
## Execution Order
1. Run `run_all.R` (which sources `00_setup.R` through `07_run_simulation.R`) to execute the 64-scenario factorial simulation study.
2. Run `0001_compile_all_results.R`, `R_codes_for_finat_tables.R`, `Result.R`, and `Plots.R` to compile summary tables and figures.
3. Run `001_sensitivity_rho_grid.R`, `002_sensitivity_CIC.R`, and `003_sensitivity_informative_sampling.R` to reproduce Supplementary Tables S3–S5.

