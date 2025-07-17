#' Spatial Correlation Matrix
#'
#' @description
#' Takes a length \eqn{d} vector \eqn{s} and a correlation strength parameter
#' \eqn{\rho} and returns a \eqn{d \times d} spatial correlation matrix with
#' elements in the \eqn{i^\text{th}} row and \eqn{j^\text{th}} column equal to
#' \eqn{s_is_j \rho^{|i-j|}}.
#'
#' @param d An integer. Dimension of correlation matrix.
#' @param rho A scalar. Correlation strength parameter.
#' @param s A numeric vector. Diagonal variance parameters.
#'
#' @return A numeric matrix. A \eqn{d \times d} spatial correlation matrix with
#' elements in the \eqn{i^\text{th}} row and \eqn{j^\text{th}} column equal to
#' \eqn{s_is_j \rho^{|i-j|}}.
#'
serial_cov <- function(d, rho, s) {
  cov_mat <- rho^abs(outer(1:d,1:d,"-"))
  for (i in 1:d) {
    for (j in i:d) {
      cov_mat[i,j] <- cov_mat[i,j] * s[i] * s[j]
      cov_mat[j,i] <- cov_mat[i,j]
    }
  }
  return(cov_mat)
}

#' Random Laplace Variable
#'
#' @description
#' Draws `n` random Laplace variables with location and scale parameters `mu`
#' and `b`.
#'
#' @param n An integer. Number of random Laplace variables to draw.
#' @param mu A scalar. Mean parameter of distribution.
#' @param b A scalar. Scale parameter such that \eqn{V(y) = 2b^2}.
#'
#' @return A numeric vector. `n` random draws from the Laplace distribution.
#'
rlaplace <- function(n, mu = 0, b = 1) {
  u <- runif(n, min = -0.5, max = 0.5)
  x <- mu - b * sign(u) * log(1 - 2 * abs(u))
  return(x)
}

#' Random Change-Point Generator
#'
#' Randomly draws K change-points uniformly distributed between 1:T subject
#' to a minimum spacing criterion
#'
#' @param T         An integer. Number of observations.
#' @param K         An integer. Number of change-points.
#' @param min_space An integer. The minimum spacing criterion between
#'                  change-points
#'
#' @return A numeric vector. K change-points in 1:T.
#'
point_picker <- function(T, K, min_space) {
  if (min_space * (K-1) > T) {
    stop("T must be > (K-1) * min_space")
  }
  valid <- 1:T # current valid change-point locations
  picked <- c() # initialized sampled locations
  for (k in 1:K) {
    # check enough remaining valid points
    if (length(valid) < K - k + 1) return(point_picker(T, K, min_space))
    # sample change point from valid locations
    if (length(valid) == 1) picked <- c(picked, valid)
    else picked <- c(picked, sample(valid, size = 1))
    # update set of valid locations
    valid <- valid[abs(valid - picked[k]) >= min_space]
  }
  return(picked[order(picked)])
}

#' MeanVar Change-Point Simulation
#'
#' @description
#' Adapts the Simulation study of Pein et al. (2017) to implement the data
#' generating process described in Simulations 1 and 2 of Berlind, Cappello, and
#' Madrid Padilla (2025).
#'
#' @param T An integer. Number of observations
#' @param K An integer. Number of change-points.
#' @param C A scalar. Jump size parameter.
#' @param min_space A scalar. Minimum spacing between change-points.
#' @param family A character. Data generating process of replicates. Can be
#'   equal to:
#'   * `"gaussian"`: Independent draws from a Normal distribution.
#'   * `"laplace"`: Independent draws from a Laplace distribution.
#'   * `"t"`: Independent draws from a Student's T distribution.
#'   * `"MA"`: Draws from an MA(2) process with Gaussian innovations.
#' @param df An integer. Degrees of freedom if `family=="t`.
#' @param theta A scalar. Decay parameter if `family=="MA`.
#'
#' @return A list.
#'   * `y`: A numeric vector. The simulated replicate.
#'   * `changepoints`: A numeric vector. The locations of the change-points.
#'   * `mu`: A numeric vector. The size of the jumps in the mean of `y` at each
#'     change-point.
#'   * `s`: A numeric vector. The size of the jumps in the variance of `y` at
#'     each change-point.
#'   * `mean_signal`: A numeric vector. The mean of `y`.
#'   * `var_signal`: A numeric vector. The variance of `y`.
#'
hsmuce_simulation <- function(T, K, C, min_space, family = "gaussian", df = NULL, theta = NULL) {
  # sample change-points
  chp <- point_picker(T - 2 * min_space, K, min_space) + min_space
  # sample variances
  s <- c(1, 2^runif(K, -2, 2))
  # generate means and signal
  mu <- numeric(K + 1)
  blks <- diff(c(1, chp, T+1))
  jumps <- sapply(2:(K+1), function(k) sqrt(C / min(blks[k] / s[k]^2, blks[k-1] / s[k-1]^2)))
  mu[2:(K+1)] <- cumsum(jumps * sample(c(-1,1), replace = TRUE, K))
  mean_signal = rep(mu, blks)
  var_signal = rep(s^2, blks)
  if (family == "gaussian") error <- sqrt(var_signal) * rnorm(T)
  else if (family == "laplace") error <- sqrt(var_signal / 2) * rlaplace(T)
  else if (family == "t") error <- sqrt(var_signal * (df - 2) / df) * rt(T, df)
  else if (family == "MA") {
    error <- rnorm(T + 2)
    error <- sqrt(var_signal / (1 + theta^2 + theta^4)) * sapply(1:T, function(t) error[t+2] + theta * error[t+1] + theta^2 * error[t])
  }

  y <- mean_signal + error

  return(list(y = y,
              mu = mu,
              s = s,
              changepoints = chp,
              mean_signal = mean_signal,
              var_signal = var_signal)
  )
}

#' Multivariate Change-Point Simulation
#'
#' @description
#' Adapts the Simulation study of Frick et al. (2014) to implement the data
#' generating process described in Simulations 3 of Berlind, Cappello, and
#' Madrid Padilla (2025).
#'
#' @param T An integer. Number of observations
#' @param d An integer. Dimension of the observations.
#' @param K An integer. Number of change-points.
#' @param p A scalar. A number between (0,1) indicating the proportion of the
#'   `d` coordinates that will actually exhibit a mean change.
#' @param rho A scalar. Strenght of spatial correlation between `d` coordinates
#'   (see `serial_cov()`)
#' @param C A scalar. Jump size parameter.
#' @param min_space A scalar. Minimum spacing between change-points.
#' @param adapt A logical. If `adapt==TRUE`, then the size of the jump is scaled
#'   by the number of active coordinates `d * p` (see (D.8) in Berlind,
#'   Cappello, and Madrid Padilla (2025)).
#'
#' @return A list.
#'   * `Y`: A numeric matrix. The simulated replicate.
#'   * `changepoints`: A numeric vector. The locations of the change-points.
#'   * `mu`: A numeric matrix. The size of the jumps in the mean of `Y` at each
#'     change-point.
#'   * `Sigma`: A numeric matrix. The variance-covariance matrix for each
#'     observation in `Y`.
#'   * `mean_signal`: A numeric matrix. The mean of `Y`.
#'
multi_simulation <- function(T, K, d, p, rho, C, min_space, adapt) {
  # number of active series
  d_0 <- floor(p * d)
  # sample change-points
  chp <- point_picker(T - 2 * min_space, K, min_space) + min_space
  # sample active indices
  active <- sample(1:d, d_0, replace = FALSE)
  # generate means and signal
  s <- 2^runif(d, -2, 2)
  mu <- matrix(0, ncol = d, nrow = K + 1)
  blks <- diff(c(1, chp, T+1))
  mean_signal <- matrix(0, nrow = blks[1], ncol = d)
  for (k in 2:(K+1)) {
    jump <- rnorm(d_0, sqrt(C * s[active] / (ifelse(adapt, d_0, 1) * min(blks[k], blks[k-1]))), sd = 0.1)
    mu[k,active] <- mu[k-1,active] + jump * sample(c(-1,1), d_0, replace = TRUE)
    mean_signal <- rbind(mean_signal, matrix(mu[k,], nrow = blks[k], ncol = d, byrow = TRUE))
  }

  Sigma <- serial_cov(d, rho = 0.9, s)
  Sigma_eigen <- eigen(Sigma)

  Y <- mean_signal + sapply(1:d, function(x) rnorm(T)) %*% Sigma_eigen$vectors %*% diag(sqrt(Sigma_eigen$values)) %*% t(Sigma_eigen$vectors)

  return(list(Y = Y,
              mu = mu,
              Sigma = Sigma,
              changepoints = chp,
              mean_signal = mean_signal))
}

#' False Positive Sensitive Location Error
#'
#' @description
#' Calculates the FPSLE for a set of estimated and true change-points (see
#' equation D.2 in Berlind, Cappello, and Madrid Padilla (2025))
#'
#' @param true_cp An integer vector. Locations of the true change-points.
#' @param est_cp An integer vector. Locations of the estimated change-points.
#'
#' @return A scalar. The FPSLE between `true_cp` and `est_cp`.
#'
fpsle <- function(true_cp, est_cp) {
  true_cp <- unique(true_cp[order(true_cp)])
  est_cp <- unique(est_cp[order(est_cp)])
  K <- length(est_cp)
  mid <- est_cp[-K] + diff(est_cp) / 2
  nbs <- as.numeric(cut(mid, true_cp, include.lowest = TRUE))
  error <- sum(abs(est_cp[-1] - true_cp[nbs+1]) + abs(est_cp[-K] - true_cp[nbs])) / (2*(K-1))
  return(error)
}

#' False Negative Sensitive Location Error
#'
#' @description
#' Calculates the FNSLE for a set of estimated and true change-points (see
#' equation D.3 in Berlind, Cappello, and Madrid Padilla (2025))
#'
#' @param true_cp An integer vector. Locations of the true change-points.
#' @param est_cp An integer vector. Locations of the estimated change-points.
#'
#' @return A scalar. The FNSLE between `true_cp` and `est_cp`.
#'
fnsle <- function(true_cp, est_cp) {
  fpsle(est_cp, true_cp)
}

#' Hausdorff Statistic
#'
#' @description
#' Calculates the Hausdorff distance between a set of estimated and true
#' change-points (see equation D.1 in Berlind, Cappello, and Madrid
#' Padilla (2025))
#'
#' @param true_cp An integer vector. Locations of the true change-points.
#' @param est_cp An integer vector. Locations of the estimated change-points.
#'
#' @return A scalar. The Hausdorff distance between `true_cp` and `est_cp`.
#'
hausdorff <- function(true_cp, est_cp) {
  max(sapply(true_cp, function(x) min(abs(x - est_cp))))
}

