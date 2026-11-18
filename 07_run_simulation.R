run_simulation <- function(scen, R = 500) {
  
  cat("Scenario parameters:\n")
  print(scen)
  
  replicate(R, {
    Dat <- gen_data(scen)
    compare_methods(Dat)
  }, simplify = FALSE)
}

## Corrected allocation: returns strictly small and large size options matching your grid
get_n_vec <- function(D, type) {
  switch(type,
         small = sample(3:5, D, TRUE),
         large = rep(25, D))
}
