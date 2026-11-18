# -------------------------------
# Required packages
# -------------------------------
pkgs <- c("MASS", "nlme", "mvtnorm", "dplyr", "tidyr")

to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

lapply(pkgs, library, character.only = TRUE)

set.seed(123)