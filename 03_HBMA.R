HBMA_unit <- function(pop, samp, covars_list,
                      rho_grid = seq(0.05, 0.95, length = 40),
                      A = 2000,
                      alpha_prior = 1,
                      beta_prior = 1) {
  
  if (!("uid" %in% names(pop)) || !("uid" %in% names(samp))) {
    stop("Both pop and samp must contain uid.")
  }
  if (!all(samp$uid %in% pop$uid)) {
    stop("Some sampled UIDs are not present in the population.")
  }
  
  ## ---- Dynamically Define Variable Spaces ----
  full_vars <- c("x1", "x2", "x12")
  p_total_vars <- length(full_vars)
  
  full_form <- as.formula(paste("~", paste(full_vars, collapse = "+")))
  X_full_samp <- model.matrix(full_form, samp)
  X_full_pop  <- model.matrix(full_form, pop)
  P <- ncol(X_full_samp)
  n_total <- nrow(samp)
  
  D <- length(unique(pop$area))
  J <- length(covars_list)
  log_marginal_lik <- numeric(J)
  model_p_j <- numeric(J)
  
  ## ---- 1. Profile Log-Likelihood Engine (Equation 24) ----
  evaluate_marginal_density <- function(rho, data, X) {
    p_j <- ncol(X)
    sigma_u2 <- rho / (1 - rho)
    
    sum_log_V <- 0
    X_V_X_inv_sum <- matrix(0, p_j, p_j)
    X_V_y_sum <- matrix(0, p_j, 1)
    
    for (d in unique(data$area)) {
      idx <- data$area == d
      y_d <- matrix(data$y[idx], ncol = 1)
      X_d <- X[idx, , drop = FALSE]
      n_d <- length(y_d)
      
      V_d_det <- 1 + n_d * sigma_u2
      sum_log_V <- sum_log_V + log(V_d_det)
      
      V_d_inv <- diag(n_d) - (sigma_u2 / (1 + n_d * sigma_u2)) * matrix(1, n_d, n_d)
      X_V_X_inv_sum <- X_V_X_inv_sum + (t(X_d) %*% V_d_inv %*% X_d)
      X_V_y_sum <- X_V_y_sum + (t(X_d) %*% V_d_inv %*% y_d)
    }
    
    if (qr(X_V_X_inv_sum)$rank < ncol(X_V_X_inv_sum)) {
      stop("Singular GLS information matrix in marginal likelihood")
    }
    
    beta_gls <- solve(X_V_X_inv_sum) %*% X_V_y_sum
    SSE <- 0
    
    for (d in unique(data$area)) {
      idx <- data$area == d
      y_d <- matrix(data$y[idx], ncol = 1)
      X_d <- X[idx, , drop = FALSE]
      V_d_inv <- diag(length(y_d)) - (sigma_u2 / (1 + length(y_d) * sigma_u2)) * matrix(1, length(y_d), length(y_d))
      
      resid <- y_d - (X_d %*% beta_gls)
      SSE <- SSE + as.numeric(t(resid) %*% V_d_inv %*% resid)
    }
    
    if (SSE <= 0) return(-Inf)
    
    ll_rho <- lgamma(0.5 * (n_total - p_j)) - 
      0.5 * (n_total - p_j) * log(pi) - 
      0.5 * sum_log_V - 
      0.5 * log(det(X_V_X_inv_sum)) - 
      0.5 * (n_total - p_j) * log(SSE)
    return(ll_rho)
  }
  
  ## ---- 2. Posterior Weights Grid Processing ----
  posterior_rho_list <- vector("list", J)
  
  for (j in 1:J) {
    vars <- covars_list[[j]]
    sub_form <- as.formula(paste("~", paste(vars, collapse = "+")))
    Xj <- model.matrix(sub_form, samp)
    model_p_j[j] <- ncol(Xj) - 1 
    
    loglik_grid <- vapply(rho_grid, function(rho) evaluate_marginal_density(rho, samp, Xj), numeric(1))
    
    max_ll <- max(loglik_grid)
    post_rho_unnorm <- exp(loglik_grid - max_ll)
    
    if (!is.finite(sum(post_rho_unnorm)) || sum(post_rho_unnorm) == 0) {
      stop("Invalid rho posterior weights")
    }
    posterior_rho_list[[j]] <- post_rho_unnorm / sum(post_rho_unnorm)
    
    grid_diffs <- diff(rho_grid)
    delta_rho <- if(length(grid_diffs) > 0) mean(grid_diffs) else 1/40
    integrated_lik <- sum(post_rho_unnorm) * delta_rho
    log_marginal_lik[j] <- max_ll + log(integrated_lik)
  }
  
  log_model_prior <- lgamma(alpha_prior + model_p_j) + 
    lgamma(beta_prior + p_total_vars - model_p_j) - 
    lgamma(alpha_prior + beta_prior + p_total_vars) -
    lchoose(p_total_vars, model_p_j)
  
  log_unnorm_pmp <- log_marginal_lik + log_model_prior
  max_log_pmp <- max(log_unnorm_pmp)
  model_pmp <- exp(log_unnorm_pmp - max_log_pmp) / sum(exp(log_unnorm_pmp - max_log_pmp))
  
  stopifnot(all(is.finite(model_pmp)), abs(sum(model_pmp) - 1) < 1e-10)
  
  ## ---- 3. Model-Specific Sampling Loops ----
  model_means <- matrix(0, D, J)
  model_vars  <- matrix(0, D, J)
  all_model_draws <- vector("list", J)
  
  for (j in 1:J) {
    vars <- covars_list[[j]]
    sub_form <- as.formula(paste("y ~", paste(vars, collapse = "+")))
    X_sub_samp <- model.matrix(sub_form, samp)
    p_j <- ncol(X_sub_samp)
    
    df_residual <- n_total - p_j
    if (df_residual <= 0) stop("Non-positive residual degrees of freedom")
    
    delta_j_replications <- matrix(0, D, A)
    
    for (a in 1:A) {
      rho_a <- sample(rho_grid, 1, prob = posterior_rho_list[[j]])
      sigma_u2_a <- rho_a / (1 - rho_a)
      
      X_V_X_inv_sum <- matrix(0, p_j, p_j)
      X_V_y_sum <- matrix(0, p_j, 1)
      
      for (d in unique(samp$area)) {
        idx <- samp$area == d
        y_d <- matrix(samp$y[idx], ncol = 1)
        X_d <- X_sub_samp[idx, , drop = FALSE]
        n_d <- length(y_d)
        
        V_d_inv <- diag(n_d) - (sigma_u2_a / (1 + n_d * sigma_u2_a)) * matrix(1, n_d, n_d)
        X_V_X_inv_sum <- X_V_X_inv_sum + (t(X_d) %*% V_d_inv %*% X_d)
        X_V_y_sum <- X_V_y_sum + (t(X_d) %*% V_d_inv %*% y_d)
      }
      
      if (qr(X_V_X_inv_sum)$rank < ncol(X_V_X_inv_sum)) {
        stop("Singular GLS information matrix in coefficient draw loop")
      }
      
      beta_gls_a <- solve(X_V_X_inv_sum) %*% X_V_y_sum
      
      SSE_a <- 0
      for (d in unique(samp$area)) {
        idx <- samp$area == d
        y_d <- matrix(samp$y[idx], ncol = 1)
        X_d <- X_sub_samp[idx, , drop = FALSE]
        V_d_inv <- diag(length(y_d)) - (sigma_u2_a / (1 + length(y_d) * sigma_u2_a)) * matrix(1, length(y_d), length(y_d))
        
        resid <- y_d - (X_d %*% beta_gls_a)
        SSE_a <- SSE_a + as.numeric(t(resid) %*% V_d_inv %*% resid)
      }
      
      sigma_e2_a <- SSE_a / rchisq(1, df = df_residual)
      V_beta_a <- sigma_e2_a * solve(X_V_X_inv_sum)
      beta_draw_sub <- as.numeric(beta_gls_a) + as.numeric(mvtnorm::rmvnorm(1, rep(0, p_j), V_beta_a))
      
      beta_full <- numeric(P)
      col_idx <- match(colnames(X_sub_samp), colnames(X_full_samp))
      beta_full[col_idx] <- beta_draw_sub
      
      sigma_u2_scaled <- sigma_u2_a * sigma_e2_a
      u_a <- numeric(D)
      
      for (d in 1:D) {
        idx_d <- samp$area == d
        n_d <- sum(idx_d)
        if (n_d > 0) {
          resid_mean <- mean(samp$y[idx_d] - as.vector(X_sub_samp[idx_d, , drop = FALSE] %*% beta_draw_sub))
          gamma_d <- (n_d * sigma_u2_scaled) / (n_d * sigma_u2_scaled + sigma_e2_a)
          post_var_u <- sigma_e2_a * (sigma_u2_scaled / (n_d * sigma_u2_scaled + sigma_e2_a))
          u_a[d] <- rnorm(1, gamma_d * resid_mean, sqrt(post_var_u))
        } else {
          u_a[d] <- rnorm(1, 0, sqrt(sigma_u2_scaled))
        }
      }
      
      for (d in 1:D) {
        pop_area_idx  <- pop$area == d
        samp_area_idx <- samp$area == d
        
        y_sampled <- samp$y[samp_area_idx]
        sampled_uids <- samp$uid[samp_area_idx]
        pop_uids     <- pop$uid[pop_area_idx]
        r_d_uids     <- setdiff(pop_uids, sampled_uids)
        
        if (length(r_d_uids) > 0) {
          non_sampled_idx <- pop$uid %in% r_d_uids
          X_r_d <- X_full_pop[non_sampled_idx, , drop = FALSE]
          
          y_predicted <- as.vector(X_r_d %*% beta_full) + u_a[d] + rnorm(nrow(X_r_d), 0, sqrt(sigma_e2_a))
          total_y_sum <- sum(y_sampled) + sum(y_predicted)
        } else {
          total_y_sum <- sum(y_sampled)
        }
        
        delta_j_replications[d, a] <- total_y_sum / sum(pop_area_idx)
      }
    }
    
    model_means[, j] <- rowMeans(delta_j_replications)
    model_vars[, j]  <- apply(delta_j_replications, 1, var)
    all_model_draws[[j]] <- delta_j_replications
  }
  
  ## ---- 4. Master Model-Averaging Point & Variance Summaries ----
  final_means <- as.vector(model_means %*% model_pmp)
  final_vars  <- numeric(D)
  
  for (d in 1:D) {
    intra_model_var <- sum(model_pmp * model_vars[d, ])
    inter_model_var <- sum(model_pmp * (model_means[d, ] - final_means[d])^2)
    final_vars[d] <- intra_model_var + inter_model_var
  }
  
  ## ---- 5. Option B: Clean PMP-Weighted Resampling Quantiles ----
  CI_lower <- numeric(D)
  CI_upper <- numeric(D)
  
  for (d in 1:D) {
    pooled_draws <- numeric(A)
    allocations <- rmultinom(1, size = A, prob = model_pmp)[, 1]
    
    current_idx <- 1
    for (j in 1:J) {
      n_draws_j <- allocations[j]
      if (n_draws_j > 0) {
        selected_draws <- sample(all_model_draws[[j]][d, ], size = n_draws_j, replace = TRUE)
        end_idx <- current_idx + n_draws_j - 1
        pooled_draws[current_idx:end_idx] <- selected_draws
        current_idx <- end_idx + 1
      }
    }
    
    if (current_idx == 1) stop("No posterior mixture draws available")
    
    quantiles <- quantile(pooled_draws[1:(current_idx - 1)], probs = c(0.025, 0.975), na.rm = TRUE)
    
    CI_lower[d] <- quantiles[1]
    CI_upper[d] <- quantiles[2]
  }
  
  list(
    mean = final_means,
    var  = final_vars,
    CI   = rbind(CI_lower, CI_upper),
    pmp  = model_pmp
  )
}
