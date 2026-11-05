library(transport)
source("../simulation_functions.R")
Rcpp::sourceCpp("../mu_sample.cpp")
source("../mu_sample.R")
Rcpp::sourceCpp("../mich_gibbs.cpp")
source("../mich_gibbs.R")

# load MICH functions
library(mich)

# get job id and set seed
jobid = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
set.seed(jobid)

# simulation parameters
T <- 250
min_space <- 10
settings <- expand.grid(
  L = c(5, 10, 20),
  d = c(2, 10, 50),
  p = c(1)
)
C <- as.numeric(Sys.getenv("C"))
rho <- as.numeric(Sys.getenv("RHO"))
adapt <- as.logical(Sys.getenv("ADAPT"))

# significance level
level <- 0.9

# MICH parameters
tol <- 1e-8
prior = "var.weighted"
log_omega <- -5.0
null_prob <- 0.05
prior_prob <- c(rep((1 - null_prob) / T, T), null_prob)

# Gibbs parameters
B <- 10000
burnin <- 1e7
thin <- 100

# initialize results matrix
cols <- c(
  "C", "L", "d", "p", "rho", "adapt", "time_vb", "time_gibbs",
  "w1_tau", "w2_mu", "marginal_dist"
)
result <- matrix(ncol = length(cols), nrow = 0)
result <- data.frame(result)
names(result) <- cols

for (j in 1:nrow(settings)) {
  print(j)
  # extract settings
  d <- settings$d[j]; L <- settings$L[j]; p <- settings$p[j];

  # generate data
  cp_data <- multi_simulation(T, L, d, p, rho, C, min_space, adapt)

  time_vb <- system.time({
    fit_vb <- mich(
      cp_data$Y, L = L, log_omega = log_omega, pi_l = prior_prob, prior = prior,
      max_iter = Inf, tol = tol, verbose = FALSE
    )
  })[3]

  time_gibbs <- system.time({
    fit_gibbs <- mich_gibbs(
      cp_data$Y, L, log_omega, log(prior_prob), prior, B, burnin, thin,
      standardize = TRUE, verbose = FALSE
    )
  })[3]

  # tau-Wasserstein distance
  tau_vb <- sapply(1:L, function(l) sample(1:(T+1), B, replace = TRUE, prob = fit_vb$pi_bar[,l]))
  tau_vb_collapse <- collapse_samples(tau_vb)
  tau_gibbs_collapse <- collapse_samples(fit_gibbs$tau)

  costm <- outer(
    seq_len(length(tau_vb_collapse$weights)),
    seq_len(length(tau_gibbs_collapse$weights)),
    Vectorize(function(i, j) {
      hausdorff(tau_vb_collapse$x[i, ], tau_gibbs_collapse$x[j, ])
    })
  )

  w1_tau <- wasserstein(
    tau_vb_collapse$weights,
    tau_gibbs_collapse$weights,
    p = 1,
    costm = costm
  )

  # mu-Wasserstein distance
  mu_vb <- mu_sample(
    tau = tau_vb,
    b_post = lapply(fit_vb$post_params, function(x) x[["b_bar"]]),
    Q = fit_vb$Q,
    lambda = fit_vb$lambda,
    omega = exp(log_omega),
    prior = prior
  )

  p_mu_vb <- pp(mu_vb)
  p_mu_gibbs <- pp(fit_gibbs$mu)

  w2_mu <- wasserstein(p_mu_vb, p_mu_gibbs, p = 2) / sqrt(sum(cp_data$mean_signal^2))

  # marginal prob distance
  marginals_vb <- apply(fit_vb$pi_bar, 1, function(x) 1 - prod(1 - x))
  marginals_gibbs <- colMeans(fit_gibbs$active)
  marginal_dist <- sqrt(sum((marginals_gibbs - marginals_vb)^2))

  result <- rbind(
    result,
    c(C, L, d, p, rho, adapt, time_vb, time_gibbs, w1_tau, w2_mu, marginal_dist)
  )
}

write.csv(
  result,
  paste0("./results/result_",
         "C_", C, "rho_", rho, "adapt_", adapt, "j_",jobid,
         ".csv"),
  row.names = FALSE
)
