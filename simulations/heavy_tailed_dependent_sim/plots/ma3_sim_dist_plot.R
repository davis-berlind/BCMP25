library(dplyr)
library(paletteer)
library(ggplot2)

results <- readRDS("heavy_results.rds")

methods <- c("NSP", "NOT", "MOSUM BUM", "MOSUM LP", "PELT",
             "HSMUCE (0.1)", "HSMUCE (0.5)",
             "MICH Ora Rev", "MICH AutoFast Rev")

png("

    ", width = 1100, height = 800)
results %>%
  filter(method %in% methods, family == "MA 0.3") %>%
  mutate(method = gsub("MICH AutoFast Rev", "MICH-Auto", method)) %>%
  mutate(method = gsub("HSMUCE", "H-SMUCE", method)) %>%
  mutate(method = gsub("MICH Ora Rev", "MICH-Ora", method)) %>%
  mutate(Bias = abs(L - L_est),
         Hausdorff = hausdorff_1 + hausdorff_2,
         FPSLE = fpsle,
         FNSLE = fnsle,
         ci_length = ifelse(avg_len == 0, NA, avg_len),
         Time = time,
         CCD = n_covered / n_detected) %>%
  select(method, T, L, Bias, Hausdorff, FPSLE, FNSLE, ci_length, CCD, Time) %>%
  pivot_longer(cols = c( Bias, Hausdorff, FPSLE, FNSLE, ci_length, CCD, Time),
               names_to = "measurement",
               values_to = "value") %>%
  mutate(measurement = ifelse(measurement == "ci_length", "Set Length", measurement),
         alpha =  ifelse(measurement == "CCD", 0.9, NA)) %>%
  ggplot(aes(x = as.factor(L), y = value, color = method, fill = method)) +
  geom_boxplot(outliers = FALSE, alpha = 0.2, fatten = 3) +
  stat_summary(fun.y="mean", geom="point", position = position_dodge(width = .75), shape = 5, size = 2.5) +
  geom_hline(aes(yintercept = alpha), linetype = "dashed") +
  facet_grid(factor(measurement, levels = c("Bias", "Hausdorff", "FPSLE", "FNSLE", "Set Length", "CCD", "Time")) ~ T,
             scales = "free", labeller = labeller(T = T.labs)) +
  scale_shape_manual(values=c(25:21,25:24)) +
  scale_color_paletteer_d("rcartocolor::Safe") +
  scale_fill_paletteer_d("rcartocolor::Safe") +
  theme_bw() +
  theme(strip.background = element_rect(fill="white"),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size=24),
        legend.title=element_blank(),
        legend.text=element_text(size=20)) +
  labs(y=NULL, x = "Number of Change-Points")
dev.off()
