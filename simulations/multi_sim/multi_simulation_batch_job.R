source("../simulation_functions.R")

# change-point packages
library(InspectChangepoint)
library(L2hdchange)
library(ecp)

# load MICH functions
library(mich)

# get job id and set seed
jobid = as.integer(Sys.getenv("SGE_TASK_ID"))
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
log_omega <- -5.0
null_prob <- 0.05

# conditional detection window
window <- floor(min(sqrt(T) / 2, 15))

# initialize results matrix
cols <- c(
  "method", "C", "L", "d", "p", "rho", "adapt", "time","L_est", "n_detected",
  "n_covered", "avg_len", "hausdorff", "fpsle", "fnsle"
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
    C, L, d, p, rho, adapt, time, L_est, NA, NA, NA,
    hausdorff(c(1, true_cp, T+1), est_cp),
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
    C, L, d, p, rho, adapt, time, L_est, NA, NA, NA,
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
    C, L, d, p, rho, adapt, time, L_est, NA, NA, NA,
    hausdorff(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fpsle(c(1, true_cp, T+1), c(1, est_cp, T+1)),
    fnsle(c(1, true_cp, T+1), c(1, est_cp, T+1))
  )
  
  #### MICH ####
  #### MICH Auto ####
  # fit model and time process
  time <- system.time({
    fit <- mich(
      cp_data$Y, L_auto = TRUE, max_iter = Inf, tol = tol, restart = FALSE
    )
  })[3]

  # determine MICH credible sets and change-points
  cs <- numeric(0)
  est_cp <- numeric(0)
  if (fit$L > 0) {
    post_sets <- mich_sets(fit$pi_bar, max_length, level)
    cs <- post_sets$sets
    est_cp <- post_sets$cp
  }

  # estimate number of change-points
  L_est <- length(est_cp)

  # detected true change-points
  detected <- true_cp[true_cp %in% unlist(lapply(est_cp, function(x) (x - window):(x + window)))]
  n_detected <- length(detected)
  # number of credible sets that cover a detected change-point
  n_covered <- ifelse(L_est == 0, 0, sum(detected %in% unlist(cs)))
  # average credible set length
  avg_len <- ifelse(L_est == 0, NA, mean(sapply(cs, function(x) length(x))))

  # store results
  result[nrow(result) + 1,"method"] <- "MICH Auto"
  result[nrow(result),-1] <- c(L, d, p, rho, adapt, time, L_est, n_detected,
                               n_covered, avg_len,
                               hausdorff(c(1, true_cp, T+1), c(1, est_cp, T+1)),
                               hausdorff(c(1, est_cp, T+1), c(1, true_cp, T+1)),
                               fpsle(c(1, true_cp, T+1), c(1, est_cp, T+1)),
                               fnsle(c(1, true_cp, T+1), c(1, est_cp, T+1)))

  #### MICH Auto Rev ####
  # fit model and time process
  time_rev <- time
  time <- system.time({fit_rev <- mich(cp_data$Y, L_auto = TRUE, max_iter = Inf, tol = tol, reverse = TRUE, restart = FALSE)})[3]
  time <- max(time, time_rev)
  if (max(fit$elbo) < max(fit_rev$elbo)) {
    fit <- fit_rev
  }

  # determine MICH credible sets and change-points
  cs <- numeric(0)
  est_cp <- numeric(0)
  if (fit$L > 0) {
    post_sets <- mich_sets(fit$pi_bar, max_length, level)
    cs <- post_sets$sets
    est_cp <- post_sets$cp
  }

  # estimate number of change-points
  L_est <- length(est_cp)

  # detected true change-points
  detected <- true_cp[true_cp %in% unlist(lapply(est_cp, function(x) (x - window):(x + window)))]
  n_detected <- length(detected)
  # number of credible sets that cover a detected change-point
  n_covered <- ifelse(L_est == 0, 0, sum(detected %in% unlist(cs)))
  # average credible set length
  avg_len <- ifelse(L_est == 0, NA, mean(sapply(cs, function(x) length(x))))

  # store results
  result[nrow(result) + 1,"method"] <- "MICH Auto rev"
  result[nrow(result),-1] <- c(L, d, p, rho, adapt, time, L_est, n_detected,
                               n_covered, avg_len,
                               hausdorff(c(1, true_cp, T+1), c(1, est_cp, T+1)),
                               hausdorff(c(1, est_cp, T+1), c(1, true_cp, T+1)),
                               fpsle(c(1, true_cp, T+1), c(1, est_cp, T+1)),
                               fnsle(c(1, true_cp, T+1), c(1, est_cp, T+1)))

  #### MICH Oracle ####
  # fit model and time process
  time <- system.time({fit <- mich(cp_data$Y, L = L, max_iter = Inf, tol = tol)})[3]

  # determine MICH credible sets and change-points
  cs <- numeric(0)
  est_cp <- numeric(0)
  if (fit$L > 0) {
    post_sets <- mich_sets(fit$pi_bar, max_length, level)
    cs <- post_sets$sets
    est_cp <- post_sets$cp
  }

  # estimate number of change-points
  L_est <- length(est_cp)

  # detected true change-points
  detected <- true_cp[true_cp %in% unlist(lapply(est_cp, function(x) (x - window):(x + window)))]
  n_detected <- length(detected)
  # number of credible sets that cover a detected change-point
  n_covered <- ifelse(L_est == 0, 0, sum(detected %in% unlist(cs)))
  # average credible set length
  avg_len <- ifelse(L_est == 0, NA, mean(sapply(cs, function(x) length(x))))

  # store results
  result[nrow(result) + 1,"method"] <- "MICH Ora"
  result[nrow(result),-1] <- c(L, d, p, rho, adapt, time, L_est, n_detected,
                               n_covered, avg_len,
                               hausdorff(c(1, true_cp, T+1), c(1, est_cp, T+1)),
                               hausdorff(c(1, est_cp, T+1), c(1, true_cp, T+1)),
                               fpsle(c(1, true_cp, T+1), c(1, est_cp, T+1)),
                               fnsle(c(1, true_cp, T+1), c(1, est_cp, T+1)))

  #### MICH Ora Rev ####
  # fit model and time process
  time_rev <- time
  time <- system.time({fit_rev <- mich(cp_data$Y, L = L, max_iter = Inf, reverse = TRUE, tol = tol)})[3]
  time <- max(time, time_rev)
  if (max(fit$elbo) < max(fit_rev$elbo)) {
    fit <- fit_rev
  }

  # determine MICH credible sets and change-points
  cs <- numeric(0)
  est_cp <- numeric(0)
  if (fit$L > 0) {
    post_sets <- mich_sets(fit$pi_bar, max_length, level)
    cs <- post_sets$sets
    est_cp <- post_sets$cp
  }

  # estimate number of change-points
  L_est <- length(est_cp)

  # detected true change-points
  detected <- true_cp[true_cp %in% unlist(lapply(est_cp, function(x) (x - window):(x + window)))]
  n_detected <- length(detected)
  # number of credible sets that cover a detected change-point
  n_covered <- ifelse(L_est == 0, 0, sum(detected %in% unlist(cs)))
  # average credible set length
  avg_len <- ifelse(L_est == 0, NA, mean(sapply(cs, function(x) length(x))))

  # store results
  result[nrow(result) + 1,"method"] <- "MICH Ora rev"
  result[nrow(result),-1] <- c(L, d, p, rho, adapt, time, L_est, n_detected,
                               n_covered, avg_len,
                               hausdorff(c(1, true_cp, T+1), c(1, est_cp, T+1)),
                               hausdorff(c(1, est_cp, T+1), c(1, true_cp, T+1)),
                               fpsle(c(1, true_cp, T+1), c(1, est_cp, T+1)),
                               fnsle(c(1, true_cp, T+1), c(1, est_cp, T+1)))

}

write.csv(
  result, 
  paste0("./results/result_", 
         "C_", C, "rho_", rho, "adapt_", adapt, "j_",jobid,
         ".csv"), 
  row.names = FALSE
)