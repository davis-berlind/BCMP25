#' Posterior Mean Sampler
#'
#' @description
#' Takes \eqn{B} posterior samples of change-points and jump-sizes and returns
#' \eqn{B} posterior samples of the mean.
#'
#' @param tau An integer matrix. Posterior samples of change-point locations.
#' @param b_post A list. List of \eqn{L} \eqn{B \times d} matrices of sampled
#'  posterior jump sizes.
#' @param Q A numeric matrix. Eigenvectors of variance of \eqn{y_t}.
#' @param lambda A numeric vector Eigenvalues of variance of \eqn{y_t}.
#' @param omega A scalar. Prior precision parameter used to sample `tau` and
#'  `b_post`.
#' @param prior A character. Prior precision model used to sample `tau` and
#'  `b_post`.
#'
#' @return A matrix. A \eqn{B \times (d\times T)} matrix of stacked posterior
#'   mean samples.
#'
mu_sample <- function(tau, b_post, Q, lambda, omega, prior = "var.weighted") {
  T <- nrow(b_post[[1]]) - 1
  T1 <- T:1
  if (prior == "var.weighted") {
    Omega <- outer(T1, omega * lambda)
    eigen_vals <- outer(T1, lambda) + Omega
  } else if (prior == 'weighted') {
    Omega <- omega * matrix(T1, nrow = T, ncol = d, byrow = FALSE)
    eigen_vals <- outer(T1, lambda) + Omega
  } else if (prior == 'independent') {
    Omega <- matrix(omega, nrow = T, ncol = d)
    eigen_vals <- outer(T1, lambda) + Omega
  }
  sd_weights <- 1 / eigen_vals

  mu_sample_cpp(tau-1, b_post, Q, sd_weights)
}
