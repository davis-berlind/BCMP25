library(dplyr)
results <- readRDS("results.rds")

results %>%
  filter(method == "MICH Ora Rev") %>%
  filter(delta == 0.5, prior == 1e-3) %>%
  group_by(T, L, min_space, tol) %>%
  summarize("|J - J_hat|" = mean(abs(L - L_est)),
            hausdorff = mean(hausdorff_1 + hausdorff_2, na.rm = TRUE),
            fpsle = mean(fpsle, na.rm = TRUE),
            fnsle = mean(fnsle, na.rm = TRUE),
            ci_length = sum(L_est * avg_len, na.rm = TRUE) / sum(L_est, na.rm = TRUE),
            coverage = sum(n_covered) / sum(n_detected),
            time = mean(time, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric) & !matches("tol"), ~format(round(.,3), nsmall = 3))) %>%
  arrange(T, L, min_space, tol) %>%
  write.csv("ora_tol_sa.csv", row.names = FALSE)

results %>%
  filter(method == "MICH AutoFast Rev") %>%
  filter(delta == 0.5, prior == 1e-3) %>%
  group_by(T, L, min_space, tol) %>%
  summarize("|J - J_hat|" = mean(abs(L - L_est)),
            hausdorff = mean(hausdorff_1 + hausdorff_2, na.rm = TRUE),
            fpsle = mean(fpsle, na.rm = TRUE),
            fnsle = mean(fnsle, na.rm = TRUE),
            ci_length = sum(L_est * avg_len, na.rm = TRUE) / sum(L_est, na.rm = TRUE),
            coverage = sum(n_covered) / sum(n_detected),
            time = mean(time, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric) & !matches("tol"), ~format(round(.,3), nsmall = 3))) %>%
  arrange(T, L, min_space, tol) %>%
  write.csv("auto_tol_sa.csv", row.names = FALSE)
