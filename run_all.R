setwd("G:/My Drive/BMA_2025/Simulation")
# 1. Setup
source("00_setup.R")
# 2. Core functions
source("01_data_gen.R")
source("02_direct.R")
source("03_HBMA.R")
source("04_HB_unit.R")
source("05_compare_methods.R")
# 3. Scenarios
source("06_scenarios.R")

# 4. Simulation engine
source("07_run_simulation.R")
