source("../simulation_functions.R")
Rcpp::sourceCpp("../simulation_functions.cpp")

# change-point packages
library(InspectChangepoint)
library(L2hdchange)
library(ecp)
library(igraph)

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
  d = c(10, 50, 100), 
  p = c(0.1, 0.5, 1)
)
C <- as.numeric(Sys.getenv("C"))
rho <- as.numeric(Sys.getenv("RHO"))
adapt <- as.logical(Sys.getenv("ADAPT"))

# significance level
level <- 0.9

# MICH parameters
tol <- 1e-6
prior = "var.weighted"
log_omega <- -7.0
null_prob <- 1e-10

# conditional detection window
window <- floor(min(sqrt(T) / 2, 15))

# initialize results matrix
cols <- c(
  "method", "C", "L", "d", "p", "rho", "adapt", 
  "time", "L_fit", "L_est", "n_detected", "n_covered", "n_covering", 
  "n_covering_d", "MMC", "MMC_d", "set_len", "set_len_d", 
  "hausdorff", "fpsle", "fnsle"
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
  true_cp <- cp_data$changepoints

  #### ECP ####
  time <- system.time({
    fit <- e.divisive(
      cp_data$Y, sig.lvl = 1 - level, min.size = min_space, alpha = 2, R = 499
    )
  })[3]
  est_cp <- fit$estimates
  # estimate number of change-points
  L_est <- length(est_cp) - 2

  # store results
  result[nrow(result) + 1,"method"] <- "ecp"
  result[nrow(result),-1] <- c(
    C, L, d, p, rho, adapt, 
    time, NA, L_est, NA, NA, NA, NA, NA, NA, NA, NA,
    hausdorff(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fpsle(c(1, true_cp, T+1), est_cp),
    fnsle(c(1, true_cp, T+1), est_cp)
  )

  #### Inspect ####
  penalty <- log(log(T) * d / 2)
  while(TRUE) {
    time <- try(system.time({
      fit <- inspect(t(cp_data$Y), lambda = penalty)
    })[3])
    if(!"try-error" %in% class(time)) break
    penalty <- penalty + 1
  }

  est_cp <- fit$changepoints[,1] + 1
  # estimate number of change-points
  L_est <- length(est_cp)

  # store results
  result[nrow(result) + 1,"method"] <- "Inspect"
  result[nrow(result),-1] <- c(
    C, L, d, p, rho, adapt, 
    time, NA, L_est, NA, NA, NA, NA, NA, NA, NA, NA,
    hausdorff(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fpsle(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fnsle(c(1, true_cp, T+1), c(1, est_cp, T+1))
  )

  #### L2HDC ####
  time <- system.time({
    ts_l2_fit <- ts_hdchange(t(cp_data$Y), window_size=min(d,sqrt(T)), N_rep=1e4); 
    fit <- hdchange(ts_l2_fit);
  })[3]
  est_cp <- fit$time_stamps + 1

  # estimate number of change-points
  L_est <- length(est_cp)

  # store results
  result[nrow(result) + 1,"method"] <- "L2HDC"
  result[nrow(result),-1] <- c(
    C, L, d, p, rho, adapt, 
    time, NA, L_est, NA, NA, NA, NA, NA, NA, NA, NA,
    hausdorff(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fpsle(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fnsle(c(1, true_cp, T+1), c(1, est_cp, T+1))
  )
  
  #### MICH ####
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
    C, L, d, p, rho, adapt, 
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
    C, L, d, p, rho, adapt, 
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
    C, L, d, p, rho, adapt, 
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
    C, L, d, p, rho, adapt, 
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
      cp_data$Y, L = T %/% min_pace, max_iter = Inf, restart = FALSE, 
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
    C, L, d, p, rho, adapt, 
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
      cp_data$Y, L = T %/% min_pace, max_iter = Inf, restart = FALSE, 
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
    C, L, d, p, rho, adapt, 
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
}

write.csv(
  result, 
  paste0("./results/result_", 
         "C_", C, "rho_", rho, "adapt_", adapt, "j_",jobid,
         ".csv"), 
  row.names = FALSE
)