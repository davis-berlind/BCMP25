#' Multivariate MICH Gibbs Sampler
#'
#' A Gibbs sampler implementation of the multivariate MICH model in Section 3.1
#' of Berlind, Cappello, and Madrid Padilla (2025).
#'
#' @param y A numeric matrix. A \eqn{T \time d} matrix of observations.
#' @param L An integer. Number of components in MICH model.
#' @param log_omega A scalar. Prior precision parameter.
#' @param log_pi A numeric vector. Log change-point location probabilities.
#' @param prior A character. One of `"var.weighted"`, `"weighted"`, or
#'  `"independent"` specifying model for prior precision matrix.
#' @param B An integer. Total number of posterior samples.
#' @param burnin An integer. Number of burn-in iterations.
#' @param thin An integer. Thinning parameter for posterior samples.
#' @param standardize A logical. Will center and scale `y` if
#'  `standardize == TRUE`.
#' @param verbose A logical. Progress displayed if `verbose == TRUE`.
#'
#' @return A list. A list of `B` posterior samples of `L` change-points with
#'  objects: `tau`: sampled change-point locations, `b` sampled jump-sizes,
#'  `mu`: sampled posterior means, and `active`: logical vector where
#'  `active[i, t] == TRUE` if `any(tau[i,] == t) == TRUE`.
#'
mich_gibbs <- function(
    y,
    L,
    log_omega,
    log_pi,
    prior = "var.weighted",
    B,
    burnin = B,
    thin = 10,
    standardize = TRUE,
    verbose = FALSE
) {
  T <- nrow(y)
  T1 <- T:1
  d <- ncol(y)
  # standardize data for numerical stability
  if (standardize) {
    scale_eigen <- eigen(var(y))
    Q_scale <- scale_eigen$vectors
    lambda_scale <- scale_eigen$values
    if (any(lambda_scale <= 0)) {
      warning("Var(y) is singular. Consider removing collinear columns.")
      lambda_scale <- lambda_scale + 1e-5
    }
    scale <- Q_scale %*% diag(sqrt(lambda_scale)) %*% t(Q_scale)
    inv_scale <- Q_scale %*% diag(1 / sqrt(lambda_scale)) %*% t(Q_scale)
    y <- y %*% inv_scale
  }

  # estimate precision matrix
  y_diff <- diff(y) # difference out mean changes

  # remove outliers due to big mean changes
  y_diff_norm <- sqrt(rowSums(y_diff^2))
  y_diff <- y_diff[y_diff_norm <= stats::quantile(y_diff_norm, p = 0.75) +  1.5 * stats::IQR(y_diff_norm), ]

  # estimate variance
  Sigma <- (t(y_diff) %*% y_diff) / (2 * nrow(y_diff))
  Sigma_eigen <- eigen(Sigma)
  if (any(Sigma_eigen$values <= 0)) {
    warning("Var(y) is singular. Consider removing collinear columns.")
    Sigma_eigen$values <- Sigma_eigen$values + 1e-5
  }
  Q <- Sigma_eigen$vectors
  lambda <- 1 / Sigma_eigen$values

  omega <- exp(log_omega)
  if (prior == "var.weighted") {
    Omega <- outer(T1, omega * lambda)
    eigen_vals <- outer(T1, lambda) + Omega
    log_det_omega_omega_bar <- rep(d * (log_omega - log(1 + omega)), T)
  } else if (prior == 'weighted') {
    Omega <- omega * matrix(T1, nrow = T, ncol = d, byrow = FALSE)
    eigen_vals <- outer(T1, lambda) + Omega
    log_det_omega_omega_bar <- rep(d * log_omega - sum(log(lambda + omega)), T)
  } else if (prior == 'independent') {
    Omega <- matrix(omega, nrow = T, ncol = d)
    eigen_vals <- outer(T1, lambda) + Omega
    log_det_omega_omega_bar <- d * log_omega - rowSums(log(eigen_vals))
  }
  sd_weights <- 1 / eigen_vals
  mean_weights <- matrix(lambda, nrow = T, ncol = d, byrow = TRUE) / eigen_vals
  sandwich_weights <- 0.5 * mean_weights * matrix(lambda, nrow = T, ncol = d, byrow = TRUE)
  log_prob_weight <- log_pi + 0.5 * c(log_det_omega_omega_bar, 0)

  result <- mich_gibbs_cpp(
    y, L, lambda, Q, B, thin, burnin, verbose,
    log_prob_weight, mean_weights, sandwich_weights, sd_weights
  )

  if (standardize) {
    for (l in 1:L) {
      result[["b"]][[l]] <- result[["b"]][[l]] %*% scale
    }
  }
  return(result)
}
