library(ggplot2)
library(paletteer)
library(tidyverse)
library(InspectChangepoint)
library(L2hdchange)
library(ecp)
library(mich)

source("../simulations/simulation_functions.R")

# read in well log data
well_log <- read.csv("./facies_data.csv")

# subset B/C Layers of shankle well
well <- well_log %>%
  filter(grepl("(B|C)", Formation)) %>%
  filter(Well.Name == "SHANKLE") %>%
  select(Facies, Depth, GR, ILD_log10, DeltaPHI, PHIND, PE, NM_M) %>%
  rename(MnM = NM_M, Rt = ILD_log10, DeltaPhi = DeltaPHI, AvgPhi = PHIND)

# extract changepoints
true_cp <- which(diff(well$Facies) != 0) + 1
T <- nrow(well)
depth = well$Depth
well$Depth = 1:T

well <- well %>% mutate(Facies = case_when(Facies == 1 ~ "Nonmarine Sandstone",
                                           Facies == 2 ~ "Nonmarine Coarse Siltstone",
                                           Facies == 3 ~ "Nonmarine Fine Siltstone",
                                           Facies == 4 ~ "Marine Siltstone/Shale",
                                           Facies == 5 ~ "Mudstone",
                                           Facies == 6 ~ "Wackestone",
                                           Facies == 7 ~ "Dolomite",
                                           Facies == 8 ~ "Packstone-Grainstone",
                                           Facies == 9 ~ "Phylloid-Algal Bafflestone"))

well_pivot <- well[-nrow(well), ] %>%
  pivot_longer(cols = c(GR, Rt, DeltaPhi, AvgPhi, PE, MnM),
               names_to = "Measurement",
               values_to = "Value")
well_pivot2<- well[-1, ] %>%
  pivot_longer(cols = c(GR, Rt, DeltaPhi, AvgPhi, PE, MnM),
               names_to = "Measurement",
               values_to = "Value")

# fit MICH ####
fit <- mich(well[,-c(1,2)], L_auto = TRUE, tol = 1e-10, restart = FALSE, verbose = TRUE)

est_cp <- mich_sets(fit$pi_bar, level = 0.99)$cp
sets <- mich_sets(fit$pi_bar, level = 0.99)$sets

abs(length(true_cp) - length(est_cp))
fnsle(c(1,true_cp,T), c(1,est_cp,T))
fpsle(c(1,true_cp,T), c(1,est_cp,T))

sum(apply(abs(outer(unlist(sets), true_cp, `-`)), 1, min) < 1)
sum(apply(abs(outer(unlist(sets), true_cp, `-`)), 1, min) <= 1)
length(est_cp[sapply(sets, function(set) min(apply(abs(outer(set, true_cp, `-`)),1,min))) <= 0])
length(est_cp[sapply(sets, function(set) min(apply(abs(outer(set, true_cp, `-`)),1,min))) <= 1])

png("./mich_well_log.png", width = 1300, height = 500)
well_pivot %>%
  mutate(Value_end = well_pivot2$Value,
         Depth_end = well_pivot2$Depth) %>%
  ggplot() +
  geom_vline(xintercept = unlist(sets), color = "lightblue", alpha = 0.6, size = 2.5) +
  geom_segment(aes(x = Depth, xend = Depth_end, y = Value, yend = Value_end, color = Facies), size = 4.5) +
  geom_point(aes(x = Depth, y = Value, color = Facies), size = 4) +
  scale_colour_paletteer_d("tvthemes::Bismuth") +
  geom_vline(xintercept = est_cp[sapply(sets, function(set) min(apply(abs(outer(set, true_cp, `-`)),1,min))) >= 1],
             linetype = "dashed", linewidth = 1.1, color = "black", alpha = 0.6) +
  geom_vline(xintercept =  est_cp[sapply(sets, function(set) min(apply(abs(outer(set, true_cp, `-`)),1,min))) < 1],
             linetype = "solid", linewidth = 1.25, color = "red", alpha = 0.6) +
  facet_grid(rows = vars(Measurement), scales = "free_y") +
  theme_minimal() +
  theme(strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 10),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5, size=24),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20)) +
  labs(title = "Lithology of Shankle Oil Well",
       y=NULL, x = "Depth (ft)") +
  scale_x_continuous(breaks = seq(1, T, 40), labels = depth[seq(1, T, 40)], expand = expansion(mult = 0.01)) 
dev.off()

# fit inspect ####
inspect_fit <- inspect(t(as.matrix(well[,-c(1,2,8)])))
est_cp = inspect_fit$changepoints[,1] + 1
est_cp = est_cp[-c(1,length(est_cp))]

abs(length(true_cp) - length(est_cp))
fnsle(c(1,true_cp,T), c(1,est_cp,T))
fpsle(c(1,true_cp,T), c(1,est_cp,T))

sum(apply(abs(outer(est_cp, true_cp, `-`)), 2, min) < 1)
sum(apply(abs(outer(est_cp, true_cp, `-`)), 2, min) <= 1)

png("./inspect_well_log.png", width = 1300, height = 800)
well_pivot %>%
  mutate(Value_end = well_pivot2$Value,
         Depth_end = well_pivot2$Depth) %>%
  ggplot() +
  geom_segment(aes(x = Depth, xend = Depth_end, y = Value, yend = Value_end, color = Facies), size = 4.5) +
  geom_point(aes(x = Depth, y = Value, color = Facies), size = 4) +
  scale_colour_paletteer_d("tvthemes::Bismuth") +
  geom_vline(xintercept = est_cp[apply(abs(outer(est_cp, true_cp, `-`)),1,min) >= 1],
             linetype = "dashed", linewidth = 1.1, color = "black", alpha = 0.6) +
  geom_vline(xintercept = est_cp[apply(abs(outer(est_cp, true_cp, `-`)),1,min) < 1],
             linetype = "solid", linewidth = 1.25, color = "red", alpha = 0.6) +
  facet_grid(rows = vars(Measurement), scales = "free_y") +
  theme_minimal() +
  theme(strip.text = element_text(size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5, size=24),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20)) +
  labs(title = "Lithology of Shankle Oil Well",
       y=NULL, x = "Depth (ft)") + 
  scale_x_continuous(breaks = seq(1, T, 40), labels = depth[seq(1, T, 40)], expand = expansion(mult = 0.01))
dev.off()

# fit l2hdchange ####
ts_l2_fit <- ts_hdchange(t(as.matrix(well[,-c(1,2)])), window_size = 5, N_rep = 1e5)
l2_fit <- hdchange(ts_l2_fit)
est_cp = l2_fit$time_stamps + 1

abs(length(true_cp) - length(est_cp))
fnsle(c(1,true_cp,T), c(1,est_cp,T))
fpsle(c(1,true_cp,T), c(1,est_cp,T))
sum(apply(abs(outer(est_cp, true_cp, `-`)), 2, min) < 1)
sum(apply(abs(outer(est_cp, true_cp, `-`)), 2, min) <= 1)

png("./l2hdc_well_log.png", width = 1300, height = 800)
well_pivot %>%
  mutate(Value_end = well_pivot2$Value,
         Depth_end = well_pivot2$Depth) %>%
  ggplot() +
  geom_segment(aes(x = Depth, xend = Depth_end, y = Value, yend = Value_end, color = Facies), size = 4.5) +
  geom_point(aes(x = Depth, y = Value, color = Facies), size = 4) +
  scale_colour_paletteer_d("tvthemes::Bismuth") +
  geom_vline(xintercept = est_cp[apply(abs(outer(est_cp, true_cp, `-`)),1,min) >= 1],
             linetype = "dashed", linewidth = 1.1, color = "black", alpha = 0.6) +
  geom_vline(xintercept = est_cp[apply(abs(outer(est_cp, true_cp, `-`)),1,min) < 1],
             linetype = "solid", linewidth = 1.25, color = "red", alpha = 0.6) +
  facet_grid(rows = vars(Measurement), scales = "free_y") +
  theme_minimal() +
  theme(strip.text = element_text(size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5, size=24),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20)) +
  labs(title = "Lithology of Shankle Oil Well",
       y=NULL, x = "Depth (ft)") + 
  scale_x_continuous(breaks = seq(1, T, 40), labels = depth[seq(1, T, 40)], expand = expansion(mult = 0.01))
dev.off()

# fit ecp ####
ecp_fit <- e.divisive(as.matrix(well[,-c(1,2)]), sig.lvl = 0.01,
                          min.size = 2, alpha = 2,
                          R=499)
est_cp <- ecp_fit$estimates[-c(1, length(ecp_fit$estimates))]

abs(length(true_cp) - length(est_cp))
fnsle(c(1,true_cp,T), c(1,est_cp,T))
fpsle(c(1,true_cp,T), c(1,est_cp,T))
sum(apply(abs(outer(est_cp, true_cp, `-`)), 2, min) < 1)
sum(apply(abs(outer(est_cp, true_cp, `-`)), 2, min) <= 1)

png("./ecp_well_log.png", width = 1300, height = 800)
well_pivot %>%
  mutate(Value_end = well_pivot2$Value,
         Depth_end = well_pivot2$Depth) %>%
  ggplot() +
  geom_segment(aes(x = Depth, xend = Depth_end, y = Value, yend = Value_end, color = Facies), size = 4.5) +
  geom_point(aes(x = Depth, y = Value, color = Facies), size = 4) +
  scale_colour_paletteer_d("tvthemes::Bismuth") +
  geom_vline(xintercept = est_cp[apply(abs(outer(est_cp, true_cp, `-`)),1,min) >= 1],
             linetype = "dashed", linewidth = 1.1, color = "black", alpha = 0.6) +
  geom_vline(xintercept =  est_cp[apply(abs(outer(est_cp, true_cp, `-`)),1,min) < 1],
             linetype = "solid", linewidth = 1.25, color = "red", alpha = 0.6) +
  facet_grid(rows = vars(Measurement), scales = "free_y") +
  theme_minimal() +
  theme(strip.text = element_text(size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5, size=24),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20)) +
  labs(title = "Lithology of Shankle Oil Well",
       y=NULL, x = "Depth (ft)") + 
  scale_x_continuous(breaks = seq(1, T, 40), labels = depth[seq(1, T, 40)], expand = expansion(mult = 0.01))
dev.off()
