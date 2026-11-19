library(transport)
source("../simulation_functions.R")
Rcpp::sourceCpp("../simulation_functions.cpp")
source("../mu_sample.R")
Rcpp::sourceCpp("../mu_sample.cpp")
source("../mich_gibbs.R")
Rcpp::sourceCpp("../mich_gibbs.cpp")

# load MICH functions
library(mich)

# get job id and set seed
jobid = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
set.seed(jobid)

# simulation parameters
T <- 250
min_space <- 15
settings <- expand.grid(
  L = c(2, 5, 10),
  d = c(5, 10),
  p = c(1)
)
C <- as.numeric(Sys.getenv("C"))
rho <- as.numeric(Sys.getenv("RHO"))
adapt <- as.logical(Sys.getenv("ADAPT"))

# MICH parameters
tol <- 1e-10
prior = "var.weighted"
log_omega <- -7.0
null_prob <- 1e-10
prior_prob <- c(rep((1 - null_prob) / T, T), null_prob)

# Gibbs parameters
B <- 10000
burnin <- 1e7
thin <- 100

# initialize results matrix
cols <- c(
  "C", "L", "d", "p", "rho", "adapt",
  "w1_tau", "w2_mu", "mcpd", "mcpd_p", "mcpd_q",
  "time_vb", "hausdorff_vb", "fpsle_vb", "fnsle_vb",
  "time_gibbs", "hausdorff_gibbs", "fpsle_gibbs", "fnsle_gibbs"
)
result <- matrix(ncol = length(cols), nrow = 0)
result <- data.frame(result)
names(result) <- cols

# check if file exists
file_name <- paste0(
  "./results/result","_C_",C,"_rho_",rho,"_adapt_",adapt,"_j_",jobid,".csv"
)
if (file.exists(file_name)) result <- read.csv(file_name)
start <- nrow(result)

for (j in 1:nrow(settings)) {
  print(j)
  # extract settings
  d <- settings$d[j]; L <- settings$L[j]; p <- settings$p[j];

  # generate data
  cp_data <- multi_simulation(T, L, d, p, rho, C, min_space, adapt)
  cp <- c(1, cp_data$changepoints, T+1)
  if (j <= start) next

  time_vb <- system.time({
    fit_vb <- mich(
      cp_data$Y, L = L, log_omega = log_omega, pi_l = prior_prob, prior = prior,
      max_iter = Inf, tol = tol, verbose = FALSE, fit_intercept = FALSE
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
  tau_vb_collapse$x <- cbind(1, tau_vb_collapse$x, T+1)

  tau_gibbs_collapse <- collapse_samples(fit_gibbs$tau)
  tau_gibbs_collapse$x <- cbind(1, tau_gibbs_collapse$x, T+1)

  costm <- matrix_hausdorff(tau_vb_collapse$x, tau_gibbs_collapse$x, sorted = TRUE)

  w1_tau <- wasserstein(
    tau_vb_collapse$weights,
    tau_gibbs_collapse$weights,
    p = 1,
    costm = costm
  )

  # cp estimates
  post_sets_vb <- mich_sets(fit_vb$pi_bar, alpha=0.9, detect = TRUE)
  cp_vb <- c(1, post_sets_vb$cp, T+1)
  cp_gibbs <- tau_gibbs_collapse$x[which.max(tau_gibbs_collapse$weights),]

  # mu-Wasserstein distance
  mu_vb <- mu_sample(
    tau = tau_vb,
    b_post = lapply(fit_vb$post_params, function(x) x[["b_bar"]]),
    Q = fit_vb$Q,
    lambda = fit_vb$lambda,
    omega = exp(log_omega),
    prior = prior
  )

  p_mu_vb <- pp(mu_vb[,1:T])
  p_mu_gibbs <- pp(fit_gibbs$mu[,1:T])

  w2_mu <- wasserstein(p_mu_vb, p_mu_gibbs, p = 2) / sqrt(sum(cp_data$mean_signal[,1]^2))

  # marginal prob distance
  marginals_prior <- 1 - (1 - prior_prob)^L
  marginals_vb <- apply(fit_vb$pi_bar, 1, function(x) 1 - prod(1 - x))
  marginals_gibbs <- colMeans(fit_gibbs$active)
  mcpd <- sqrt(sum((marginals_gibbs - marginals_vb)^2))
  mcpd_p <- sqrt(sum((marginals_gibbs - marginals_prior)^2))
  mcpd_q <- sqrt(sum((marginals_vb - marginals_prior)^2))

  result[j,] <- c(
    C, L, d, p, rho, adapt, w1_tau, w2_mu, mcpd, mcpd_p, mcpd_q,
    time_vb, hausdorff(cp, cp_vb), fnsle(cp, cp_vb), fpsle(cp, cp_vb),
    time_gibbs, hausdorff(cp, cp_gibbs), fnsle(cp, cp_gibbs), fpsle(cp, cp_gibbs)
  )

  write.csv(result, file_name, row.names = FALSE)
}
