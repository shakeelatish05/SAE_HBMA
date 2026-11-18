scenario_grid <- expand.grid(
  D = c(10, 50),
  rho_true = c(0.1, 0.6),
  re_dist = c("normal", "skewed"),
  n_type = c("small", "large"),
  dgp_type = c("baseline", "interaction", "sparse", "dense"),
  stringsAsFactors = FALSE
)

get_n_vec <- function(D, type) {
  switch(type,
         small = sample(3:5, D, TRUE),
         medium = rep(10, D),
         large = rep(25, D))
}


