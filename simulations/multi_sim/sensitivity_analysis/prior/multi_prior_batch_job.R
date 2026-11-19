source("~/BCMP25/simulations/simulation_functions.R")
Rcpp::sourceCpp("~/BCMP25/simulations/simulation_functions.cpp")

# load MICH functions
library(mich)

# package for calculating coverage
library(igraph)

# get job id and set seed
jobid = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
set.seed(jobid)

# simulation parameters
T <- 250
min_space <- 15
settings <- expand.grid(
  log_omega = c(-12, -6, -3, 0),
  null_prob = c(1e-10, 1e-5, 0.05),
  L = c(2, 5, 10), 
  d = c(5, 10, 50), 
  p = c(1)
)
C <- 10
rho <- 0
adapt <- FALSE

# significance level
level <- 0.9

# MICH parameters
tol <- 1e-7
prior = Sys.getenv("PRIOR")

# conditional detection window
window <- floor(min(sqrt(T) / 2, 15))

# initialize results matrix
cols <- c(
  "method", "C", "L", "d", "p", "rho", "adapt", 
  "prior", "log_omega", "null_prob",
  "time", "L_fit", "L_est", "n_detected", "n_covered", "n_covering", 
  "n_covering_d", "MMC", "MMC_d", "set_len", "set_len_d", 
  "hausdorff", "fpsle", "fnsle"
)
result <- matrix(ncol = length(cols), nrow = 0)
result <- data.frame(result)
names(result) <- cols

# check if file exists
file_name <- paste0("./results/result_prior_", prior, "_j_", jobid, ".csv")
if (file.exists(file_name)) result <- read.csv(file_name)
m <- nrow(result)
if (m == 0) {
  start <- 0
} else {
  start <- which(
    settings$log_omega == result$log_omega[m] &
      settings$null_prob == result$null_prob[m] &
      settings$L == result$L[m] &
      settings$d == result$d[m] &
      settings$p == result$p[m]
  )
}

for (j in 1:nrow(settings)) {
  print(j)
  # extract settings
  d <- settings$d[j]; L <- settings$L[j]; p <- settings$p[j];
  log_omega = settings$log_omega[j]
  null_prob <- settings$null_prob[j]
  
  # generate data
  cp_data <- multi_simulation(T, L, d, p, rho, C, min_space, adapt)
  true_cp <- cp_data$changepoints
  if (j <= start) next
  
  #### MICH Auto ####
  # fit model and time process
  time <- system.time({
    fit <- mich(
      cp_data$Y, L_auto = TRUE, max_iter = Inf, restart = FALSE, 
      fit_intercept = TRUE, fit_scale = TRUE, standardize = TRUE,
      tol = tol, prior = prior, log_omega = log_omega, null_prob = null_prob
    )
  })[3]
  
  # determine MICH credible sets and change-points
  cs <- numeric(0); est_cp <- numeric(0)
  n_covered <- 0; n_covering <- 0; n_covering_d <- 0; MMC <- 0; MMC_d <- 0;
  if (fit$L > 0) {
    post_sets <- mich_sets(fit$pi_bar, level, detect = FALSE)
    # map true changes to credible sets
    A <- vapply(post_sets$sets, function(s) true_cp %in% s, logical(length(true_cp)))
    A <- matrix(A, nrow = length(true_cp)) 
    n_covered <- sum(apply(A, 1, max)) # number of changes covered
    n_covering <- sum(apply(A, 2, max)) # number of sets covering change
    g <- graph_from_biadjacency_matrix(A)
    MMC <- sum(!is.na(max_bipartite_match(g)$matching[seq_len(nrow(A))]))
    
    post_sets_detect <- mich_sets(fit$pi_bar, level, detect = TRUE)
    est_cp <- post_sets_detect$cp
  }
  
  # estimate number of change-points
  L_est <- length(est_cp)
  
  # detected true change-points
  detected <- true_cp[true_cp %in% unlist(lapply(est_cp, function(x) (x - window):(x + window)))]
  n_detected <- length(detected)
  
  if (L_est > 0) {
    # map true changes to credible sets
    A <- vapply(post_sets_detect$sets, function(s) detected %in% s, logical(length(detected)))
    A <- matrix(A, nrow = length(detected)) 
    n_covering_d <- sum(apply(A, 2, max)) # number of sets covering change
    g <- graph_from_biadjacency_matrix(A)
    MMC_d <- sum(!is.na(max_bipartite_match(g)$matching[seq_len(nrow(A))]))
  }
  
  # average credible set length
  set_len <- ifelse(fit$L == 0, NA, mean(sapply(post_sets$sets, function(x) length(x))))
  set_len_d <- ifelse(L_est == 0, NA, mean(sapply(post_sets_detect$sets, function(x) length(x))))
  
  # store results
  result[nrow(result) + 1,"method"] <- "MICH Auto"
  result[nrow(result),-1] <- c(
    C, L, d, p, rho, adapt, prior, log_omega, null_prob,
    time, fit$L, L_est, n_detected, n_covered, n_covering, 
    n_covering_d, MMC, MMC_d, set_len, set_len_d, 
    hausdorff(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fpsle(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fnsle(c(1, true_cp, T+1), c(1, est_cp, T+1))
  )
  
  #### MICH Auto Rev ####
  # fit model and time process
  time_fwd <- time
  elbo_fwd <- max(fit$elbo)
  time <- system.time({
    fit <- mich(
      cp_data$Y, L_auto = TRUE, max_iter = Inf, restart = FALSE, 
      fit_intercept = TRUE, fit_scale = TRUE, standardize = TRUE, reverse = TRUE,
      tol = tol, prior = prior, log_omega = log_omega, null_prob = null_prob
    )
  })[3]
  
  # determine MICH credible sets and change-points
  cs <- numeric(0); est_cp <- numeric(0)
  n_covered <- 0; n_covering <- 0; n_covering_d <- 0; MMC <- 0; MMC_d <- 0;
  if (fit$L > 0) {
    post_sets <- mich_sets(fit$pi_bar, level, detect = FALSE)
    # map true changes to credible sets
    A <- vapply(post_sets$sets, function(s) true_cp %in% s, logical(length(true_cp)))
    A <- matrix(A, nrow = length(true_cp)) 
    n_covered <- sum(apply(A, 1, max)) # number of changes covered
    n_covering <- sum(apply(A, 2, max)) # number of sets covering change
    g <- graph_from_biadjacency_matrix(A)
    MMC <- sum(!is.na(max_bipartite_match(g)$matching[seq_len(nrow(A))]))
    
    post_sets_detect <- mich_sets(fit$pi_bar, level, detect = TRUE)
    est_cp <- post_sets_detect$cp
  }
  
  # estimate number of change-points
  L_est <- length(est_cp)
  
  # detected true change-points
  detected <- true_cp[true_cp %in% unlist(lapply(est_cp, function(x) (x - window):(x + window)))]
  n_detected <- length(detected)
  
  if (L_est > 0) {
    # map true changes to credible sets
    A <- vapply(post_sets_detect$sets, function(s) detected %in% s, logical(length(detected)))
    A <- matrix(A, nrow = length(detected)) 
    n_covering_d <- sum(apply(A, 2, max)) # number of sets covering change
    g <- graph_from_biadjacency_matrix(A)
    MMC_d <- sum(!is.na(max_bipartite_match(g)$matching[seq_len(nrow(A))]))
  }
  
  # average credible set length
  set_len <- ifelse(fit$L == 0, NA, mean(sapply(post_sets$sets, function(x) length(x))))
  set_len_d <- ifelse(L_est == 0, NA, mean(sapply(post_sets_detect$sets, function(x) length(x))))
  
  # store results
  result[nrow(result) + 1,"method"] <- "MICH Auto rev"
  result[nrow(result),-1] <- c(
    C, L, d, p, rho, adapt, prior, log_omega, null_prob,
    time, fit$L, L_est, n_detected, n_covered, n_covering, 
    n_covering_d, MMC, MMC_d, set_len, set_len_d, 
    hausdorff(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fpsle(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fnsle(c(1, true_cp, T+1), c(1, est_cp, T+1))
  )
  
  #### MICH Auto best ####
  result[nrow(result) + 1,"method"] <- "MICH Auto Best"
  if (elbo_fwd > max(fit$elbo)) {
    result[nrow(result),-1] <- result[nrow(result)-2,-1]
  } else {
    result[nrow(result),-1] <- result[nrow(result)-1,-1]
  }
  result[nrow(result),"time"] <- max(time_fwd, time)
  
  #### MICH Oracle ####
  # fit model and time process
  time <- system.time({
    fit <- mich(
      cp_data$Y, L = L, max_iter = Inf, restart = FALSE, 
      fit_intercept = TRUE, fit_scale = TRUE, standardize = TRUE,
      tol = tol, prior = prior, log_omega = log_omega, null_prob = null_prob
    )
  })[3]
  
  # determine MICH credible sets and change-points
  cs <- numeric(0); est_cp <- numeric(0)
  n_covered <- 0; n_covering <- 0; n_covering_d <- 0; MMC <- 0; MMC_d <- 0;
  if (fit$L > 0) {
    post_sets <- mich_sets(fit$pi_bar, level, detect = FALSE)
    # map true changes to credible sets
    A <- vapply(post_sets$sets, function(s) true_cp %in% s, logical(length(true_cp)))
    A <- matrix(A, nrow = length(true_cp)) 
    n_covered <- sum(apply(A, 1, max)) # number of changes covered
    n_covering <- sum(apply(A, 2, max)) # number of sets covering change
    g <- graph_from_biadjacency_matrix(A)
    MMC <- sum(!is.na(max_bipartite_match(g)$matching[seq_len(nrow(A))]))
    
    post_sets_detect <- mich_sets(fit$pi_bar, level, detect = TRUE)
    est_cp <- post_sets_detect$cp
  }
  
  # estimate number of change-points
  L_est <- length(est_cp)
  
  # detected true change-points
  detected <- true_cp[true_cp %in% unlist(lapply(est_cp, function(x) (x - window):(x + window)))]
  n_detected <- length(detected)
  
  if (L_est > 0) {
    # map true changes to credible sets
    A <- vapply(post_sets_detect$sets, function(s) detected %in% s, logical(length(detected)))
    A <- matrix(A, nrow = length(detected)) 
    n_covering_d <- sum(apply(A, 2, max)) # number of sets covering change
    g <- graph_from_biadjacency_matrix(A)
    MMC_d <- sum(!is.na(max_bipartite_match(g)$matching[seq_len(nrow(A))]))
  }
  
  # average credible set length
  set_len <- ifelse(fit$L == 0, NA, mean(sapply(post_sets$sets, function(x) length(x))))
  set_len_d <- ifelse(L_est == 0, NA, mean(sapply(post_sets_detect$sets, function(x) length(x))))
  
  # store results
  result[nrow(result) + 1,"method"] <- "MICH Ora"
  result[nrow(result),-1] <- c(
    C, L, d, p, rho, adapt, prior, log_omega, null_prob,
    time, fit$L, L_est, n_detected, n_covered, n_covering, 
    n_covering_d, MMC, MMC_d, set_len, set_len_d, 
    hausdorff(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fpsle(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fnsle(c(1, true_cp, T+1), c(1, est_cp, T+1))
  )
  
  #### MICH Ora Rev ####
  # fit model and time process
  time_fwd <- time
  elbo_fwd <- max(fit$elbo)
  time <- system.time({
    fit <- mich(
      cp_data$Y, L = L, max_iter = Inf, restart = FALSE, 
      fit_intercept = TRUE, fit_scale = TRUE, standardize = TRUE, reverse = TRUE,
      tol = tol, prior = prior, log_omega = log_omega, null_prob = null_prob
    )
  })[3]
  
  # determine MICH credible sets and change-points
  cs <- numeric(0); est_cp <- numeric(0)
  n_covered <- 0; n_covering <- 0; n_covering_d <- 0; MMC <- 0; MMC_d <- 0;
  if (fit$L > 0) {
    post_sets <- mich_sets(fit$pi_bar, level, detect = FALSE)
    # map true changes to credible sets
    A <- vapply(post_sets$sets, function(s) true_cp %in% s, logical(length(true_cp)))
    A <- matrix(A, nrow = length(true_cp)) 
    n_covered <- sum(apply(A, 1, max)) # number of changes covered
    n_covering <- sum(apply(A, 2, max)) # number of sets covering change
    g <- graph_from_biadjacency_matrix(A)
    MMC <- sum(!is.na(max_bipartite_match(g)$matching[seq_len(nrow(A))]))
    
    post_sets_detect <- mich_sets(fit$pi_bar, level, detect = TRUE)
    est_cp <- post_sets_detect$cp
  }
  
  # estimate number of change-points
  L_est <- length(est_cp)
  
  # detected true change-points
  detected <- true_cp[true_cp %in% unlist(lapply(est_cp, function(x) (x - window):(x + window)))]
  n_detected <- length(detected)
  
  if (L_est > 0) {
    # map true changes to credible sets
    A <- vapply(post_sets_detect$sets, function(s) detected %in% s, logical(length(detected)))
    A <- matrix(A, nrow = length(detected)) 
    n_covering_d <- sum(apply(A, 2, max)) # number of sets covering change
    g <- graph_from_biadjacency_matrix(A)
    MMC_d <- sum(!is.na(max_bipartite_match(g)$matching[seq_len(nrow(A))]))
  }
  
  # average credible set length
  set_len <- ifelse(fit$L == 0, NA, mean(sapply(post_sets$sets, function(x) length(x))))
  set_len_d <- ifelse(L_est == 0, NA, mean(sapply(post_sets_detect$sets, function(x) length(x))))
  
  # store results
  result[nrow(result) + 1,"method"] <- "MICH Ora rev"
  result[nrow(result),-1] <- c(
    C, L, d, p, rho, adapt, prior, log_omega, null_prob,
    time, fit$L, L_est, n_detected, n_covered, n_covering, 
    n_covering_d, MMC, MMC_d, set_len, set_len_d, 
    hausdorff(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fpsle(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fnsle(c(1, true_cp, T+1), c(1, est_cp, T+1))
  )
  
  #### MICH Ora best ####
  result[nrow(result) + 1,"method"] <- "MICH Ora Best"
  if (elbo_fwd > max(fit$elbo)) {
    result[nrow(result),-1] <- result[nrow(result)-2,-1]
  } else {
    result[nrow(result),-1] <- result[nrow(result)-1,-1]
  }
  result[nrow(result),"time"] <- max(time_fwd, time)
  
  #### MICH Min Space ####
  # fit model and time process
  time <- system.time({
    fit <- mich(
      cp_data$Y, L = T %/% min_space, max_iter = Inf, restart = FALSE, 
      fit_intercept = TRUE, fit_scale = TRUE, standardize = TRUE,
      tol = tol, prior = prior, log_omega = log_omega, null_prob = null_prob
    )
  })[3]
  
  # determine MICH credible sets and change-points
  cs <- numeric(0); est_cp <- numeric(0)
  n_covered <- 0; n_covering <- 0; n_covering_d <- 0; MMC <- 0; MMC_d <- 0;
  if (fit$L > 0) {
    post_sets <- mich_sets(fit$pi_bar, level, detect = FALSE)
    # map true changes to credible sets
    A <- vapply(post_sets$sets, function(s) true_cp %in% s, logical(length(true_cp)))
    A <- matrix(A, nrow = length(true_cp)) 
    n_covered <- sum(apply(A, 1, max)) # number of changes covered
    n_covering <- sum(apply(A, 2, max)) # number of sets covering change
    g <- graph_from_biadjacency_matrix(A)
    MMC <- sum(!is.na(max_bipartite_match(g)$matching[seq_len(nrow(A))]))
    
    post_sets_detect <- mich_sets(fit$pi_bar, level, detect = TRUE)
    est_cp <- post_sets_detect$cp
  }
  
  # estimate number of change-points
  L_est <- length(est_cp)
  
  # detected true change-points
  detected <- true_cp[true_cp %in% unlist(lapply(est_cp, function(x) (x - window):(x + window)))]
  n_detected <- length(detected)
  
  if (L_est > 0) {
    # map true changes to credible sets
    A <- vapply(post_sets_detect$sets, function(s) detected %in% s, logical(length(detected)))
    A <- matrix(A, nrow = length(detected)) 
    n_covering_d <- sum(apply(A, 2, max)) # number of sets covering change
    g <- graph_from_biadjacency_matrix(A)
    MMC_d <- sum(!is.na(max_bipartite_match(g)$matching[seq_len(nrow(A))]))
  }
  
  # average credible set length
  set_len <- ifelse(fit$L == 0, NA, mean(sapply(post_sets$sets, function(x) length(x))))
  set_len_d <- ifelse(L_est == 0, NA, mean(sapply(post_sets_detect$sets, function(x) length(x))))
  
  # store results
  result[nrow(result) + 1,"method"] <- "MICH Min Space"
  result[nrow(result),-1] <- c(
    C, L, d, p, rho, adapt, prior, log_omega, null_prob,
    time, fit$L, L_est, n_detected, n_covered, n_covering, 
    n_covering_d, MMC, MMC_d, set_len, set_len_d, 
    hausdorff(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fpsle(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fnsle(c(1, true_cp, T+1), c(1, est_cp, T+1))
  )
  
  #### MICH Min Space Rev ####
  # fit model and time process
  time_fwd <- time
  elbo_fwd <- max(fit$elbo)
  time <- system.time({
    fit <- mich(
      cp_data$Y, L = T %/% min_space, max_iter = Inf, restart = FALSE, 
      fit_intercept = TRUE, fit_scale = TRUE, standardize = TRUE, reverse = TRUE,
      tol = tol, prior = prior, log_omega = log_omega, null_prob = null_prob
    )
  })[3]
  
  # determine MICH credible sets and change-points
  cs <- numeric(0); est_cp <- numeric(0)
  n_covered <- 0; n_covering <- 0; n_covering_d <- 0; MMC <- 0; MMC_d <- 0;
  if (fit$L > 0) {
    post_sets <- mich_sets(fit$pi_bar, level, detect = FALSE)
    # map true changes to credible sets
    A <- vapply(post_sets$sets, function(s) true_cp %in% s, logical(length(true_cp)))
    A <- matrix(A, nrow = length(true_cp)) 
    n_covered <- sum(apply(A, 1, max)) # number of changes covered
    n_covering <- sum(apply(A, 2, max)) # number of sets covering change
    g <- graph_from_biadjacency_matrix(A)
    MMC <- sum(!is.na(max_bipartite_match(g)$matching[seq_len(nrow(A))]))
    
    post_sets_detect <- mich_sets(fit$pi_bar, level, detect = TRUE)
    est_cp <- post_sets_detect$cp
  }
  
  # estimate number of change-points
  L_est <- length(est_cp)
  
  # detected true change-points
  detected <- true_cp[true_cp %in% unlist(lapply(est_cp, function(x) (x - window):(x + window)))]
  n_detected <- length(detected)
  
  if (L_est > 0) {
    # map true changes to credible sets
    A <- vapply(post_sets_detect$sets, function(s) detected %in% s, logical(length(detected)))
    A <- matrix(A, nrow = length(detected)) 
    n_covering_d <- sum(apply(A, 2, max)) # number of sets covering change
    g <- graph_from_biadjacency_matrix(A)
    MMC_d <- sum(!is.na(max_bipartite_match(g)$matching[seq_len(nrow(A))]))
  }
  
  # average credible set length
  set_len <- ifelse(fit$L == 0, NA, mean(sapply(post_sets$sets, function(x) length(x))))
  set_len_d <- ifelse(L_est == 0, NA, mean(sapply(post_sets_detect$sets, function(x) length(x))))
  
  # store results
  result[nrow(result) + 1,"method"] <- "MICH Min Space rev"
  result[nrow(result),-1] <- c(
    C, L, d, p, rho, adapt, prior, log_omega, null_prob,
    time, fit$L, L_est, n_detected, n_covered, n_covering, 
    n_covering_d, MMC, MMC_d, set_len, set_len_d, 
    hausdorff(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fpsle(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fnsle(c(1, true_cp, T+1), c(1, est_cp, T+1))
  )
  
  #### MICH Ora best ####
  result[nrow(result) + 1,"method"] <- "MICH Min Space Best"
  if (elbo_fwd > max(fit$elbo)) {
    result[nrow(result),-1] <- result[nrow(result)-2,-1]
  } else {
    result[nrow(result),-1] <- result[nrow(result)-1,-1]
  }
  result[nrow(result),"time"] <- max(time_fwd, time)
  
  #### write result ####
  write.csv(result, file_name, row.names = FALSE)
}