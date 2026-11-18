gen_data <- function(scenario, Nd = 50, sigma_e2_true = 1) {
  
  D        <- scenario$D
  rho_true <- scenario$rho_true
  re_dist  <- scenario$re_dist
  dgp_type <- scenario$dgp_type
  n_type   <- scenario$n_type
  
  n_vec <- get_n_vec(D, n_type)
  
  beta_true <- switch(
    dgp_type,
    baseline    = c(1, 2, 0, 0),
    interaction = c(1, 0, 0, 3),
    sparse      = c(1, 0, 3, 0),
    dense       = c(1, 3, 2, 2)
  )
  
  sigma_u2 <- rho_true / (1 - rho_true) * sigma_e2_true
  
  u_d <- switch(
    re_dist,
    normal = rnorm(D, 0, sqrt(sigma_u2)),
    skewed = rgamma(D, shape = 2, scale = sqrt(sigma_u2 / 2)) - sqrt(2 * sigma_u2)
  )
  
  pop <- do.call(rbind, lapply(seq_len(D), function(d) {
    x1  <- rnorm(Nd)
    x2  <- rnorm(Nd)
    x12 <- x1 * x2
    X   <- cbind(1, x1, x2, x12)
    
    y <- as.vector(X %*% beta_true + u_d[d] + rnorm(Nd, 0, sqrt(sigma_e2_true)))
    data.frame(area = d, y, x1, x2, x12)
  }))
  
  ## ---- Robustness Fix: Create UID Unconditionally ----
  pop$uid <- seq_len(nrow(pop))
  
  samp <- do.call(rbind, lapply(seq_len(D), function(d) {
    pop_d <- pop[pop$area == d, ]
    pop_d[sample(nrow(pop_d), n_vec[d]), ]
  }))
  
  list(
    pop = pop,
    samp = samp,
    n_vec = n_vec,
    beta_true = beta_true,
    sigma_u2 = sigma_u2
  )
}
