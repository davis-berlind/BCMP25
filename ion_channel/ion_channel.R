library(stepR)
library(changepoint)
library(latex2exp)
library(mich)
library(ggplot2)

# read data
ion <- read.csv("./ICdata.csv")
id <- seq(403300, 435811, 11)
x <- ion$time[id]
unit <-  mean(diff(x))
y <- ion$data[id]
T <- length(y)

#### MICH MeanVar ####
fit_mean_var <- mich(y, J_auto = TRUE, max_iter = Inf, verbose = TRUE, tol = 1e-4)
cred_sets <- mich_sets(fit_mean_var$meanvar_model$pi_bar, level = 0.95)
mu <- fit_mean_var$mu
est_cp <- cred_sets$cp

plot_data = data.frame(x = x, 
                       y = y, 
                       mu = mu, 
                       method = paste0("MICH \n (J = ", fit_mean_var$J, ")"))

rect_data = data.frame(xmin = x[unlist(cred_sets$sets)] - unit, 
                       xmax = x[unlist(cred_sets$sets)] + unit,
                       method = paste0("MICH \n (J = ", fit_mean_var$J, ")"))

change_data = data.frame(x = x[est_cp], method = paste0("MICH \n (J = ", fit_mean_var$J, ")"))

#### H-SMUCE 0.05 ####
fit <- stepFit(y, alpha = 0.05, confband = TRUE,
               jumpint = TRUE, family = "hsmuce")
mu <- rep(fit$value, diff(c(fit$leftEnd,T+1)))
est_cp <- fit$leftEnd[-1]

plot_data = rbind(plot_data,
                  data.frame(x = x, y = y, mu = mu, 
                             method = "H-SMUCE \n (alpha = 0.05)"))

rect_data = rbind(rect_data, 
                  data.frame(xmin = x[fit$leftEndLeftBound[-1]-1] - 0.5 * unit, 
                             xmax = x[fit$leftEndRightBound[-1]] + 0.5 * unit,
                             method = "H-SMUCE \n (alpha = 0.05)"))

change_data = rbind(change_data, 
                    data.frame(x = x[est_cp], method = "H-SMUCE \n (alpha = 0.05)"))

#### PELT ####
fit <- cpt.meanvar(y, method = "PELT")
L_est <- length(fit@cpts) - 1
est_cp <- fit@cpts[-(L_est+1)] + 1
mu <- rep(fit@param.est$mean, diff(c(1, est_cp, T+1)))

plot_data = rbind(plot_data,
                  data.frame(x = x, y = y, mu = mu, 
                             method = "PELT"))

change_data = rbind(change_data, 
                    data.frame(x = x[est_cp], method = "PELT"))

plot_data$method <- factor(plot_data$method, levels = c(paste0("MICH \n (J = ", fit_mean_var$J, ")"), "H-SMUCE \n (alpha = 0.05)", "PELT"))
rect_data$method <- factor(rect_data$method, levels = c(paste0("MICH \n (J = ", fit_mean_var$J, ")"), "H-SMUCE \n (alpha = 0.05)", "PELT"))
change_data$method <- factor(change_data$method, levels = c(paste0("MICH \n (J = ", fit_mean_var$J, ")"), "H-SMUCE \n (alpha = 0.05)", "PELT"))

png("./ion_plot_1.png", width = 1250, height = (1250 / 2))
ggplot(plot_data) +
  geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            alpha = 0.5, fill = "lightblue") +
  geom_line(aes(x = x, y = y), color = "black") +
  geom_line(aes(x = x, y = mu), color = "red", linewidth = 1.1) +
  geom_vline(data = change_data, aes(xintercept = x), color = "blue", linetype = "dashed") +
  facet_grid(vars(method)) +
  theme_minimal() +
  labs(x = "Time (s)", y = "Conductance (nS)")
dev.off()

png("./ion_plot_2.png", width = 1250, height = 1250 / 2)
par(mfrow = c(2,1), oma = c(0,1.5,0,0), mar = c(4,4,3,2))

#### MICH Mean ####
fit_mean <- mich(y, L_auto = TRUE, max_iter = Inf, verbose = TRUE, tol = 1e-6, restart = FALSE)
cred_sets <- mich_sets(fit_mean$mean_model$pi_bar, level = 0.95)
mu <- fit_mean$mu
est_cp <- cred_sets$cp

plot(x, y, type = "l", main = paste0("MICH (L = ", fit_mean$L,")"),
     xlab = "", ylab = "", col = "white",
     cex.main=2.5, cex.lab=2, cex.axis=2)
for(i in unlist(cred_sets$sets)) {
  rect(xleft = x[i]-2*unit, xright = x[i] + 2*unit,
       ybottom = par("usr")[3], ytop = par("usr")[4],
       col =  adjustcolor("lightblue", alpha = 1),
       border = NA)
}
lines(x, y, type = "l")
lines(x, mu, col = "red", lwd = 2.5)
abline(v = x[est_cp], lwd = 2, lty = 2, col = "blue")
length(est_cp)

#### H-SMUCE 0.5 ####
fit <- stepFit(y, alpha = 0.5, confband = TRUE,
               jumpint = TRUE, family = "hsmuce")
mu <- rep(fit$value, diff(c(fit$leftEnd,T+1)))
est_cp <- fit$leftEnd[-1]
plot(x, y, type = "l", main = TeX("H-SMUCE ($\\alpha$ = 0.5)", bold = TRUE),
     xlab = "Time (s)", ylab = "",
     cex.main=2.5, cex.lab=2, cex.axis=2)
for(i in 1:length(est_cp)) {
  rect(xleft = x[fit$leftEndLeftBound[i+1]-1]-0.5*unit, xright = x[fit$leftEndRightBound[i+1]] + 0.5*unit,
       ybottom = par("usr")[3], ytop = par("usr")[4],
       col =  adjustcolor("lightblue", alpha = 1),
       border = NA)
}
lines(x, y, type = "l")
lines(x, mu, col = "red", lwd = 2.5)
abline(v = x[est_cp], lwd = 2, lty = 2, col = "blue")
length(est_cp)
mtext("Conductance (nS)", side = 2, outer = TRUE, line = -1, cex = 2)

dev.off()
