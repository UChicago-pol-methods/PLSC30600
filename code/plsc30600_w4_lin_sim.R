# Simple DGP + Lin estimator by hand + separate linear models
set.seed(60637)
library(ggplot2)

# DGP setup (shared covariates and treatment assignment)
n <- 1e3
mu_x1 <- 2
X1 <- rnorm(n, mean = mu_x1)
D <- rbinom(n, size = 1, prob = 0.5)
u <- rnorm(n, sd = 1.0)

# -------- DGP 1: constant TE, no X relationship
# y0 = a0_1 + u, y1 = a0_1 + tau_1 + u
a0_1 <- 0.2
tau_1 <- 0.6
y0_1 <- a0_1 + u
y1_1 <- a0_1 + tau_1 + u

# -------- DGP 2: constant TE, common X relationship
# y0 = a0_2 + b_x*X1 + u, y1 = a0_2 + tau_2 + b_x*X1 + u
a0_2 <- 0.2
tau_2 <- 0.6
b_x <- 0.5
y0_2 <- a0_2 + b_x * X1 + u
y1_2 <- a0_2 + tau_2 + b_x * X1 + u

# -------- DGP 3: different X effects under treatment/control (Lin interactions)
# y0 = a0_3 + b0*X1 + u, y1 = a1_3 + b1*X1 + u
a0_3 <- 0.2
b0 <- 0.8
a1_3 <- 0.6
b1 <- 0.2
y0 <- a0_3 + b0 * X1 + u
y1 <- a1_3 + b1 * X1 + u
Y <- D * y1 + (1 - D) * y0


# -------- Visualize simple linear models before interactions (DGP 1 & 2)

x1_grid <- seq(min(X1), max(X1), length.out = 200)

# DGP 1 curves
mu0_const <- rep(a0_1, length(x1_grid))
mu1_const <- rep(a0_1 + tau_1, length(x1_grid))

# DGP 2 curves
mu0_common <- a0_2 + b_x * x1_grid
mu1_common <- a0_2 + tau_2 + b_x * x1_grid

viz_df <- data.frame(
  x1 = c(x1_grid, x1_grid, x1_grid, x1_grid),
  mu = c(mu0_const, mu1_const, mu0_common, mu1_common),
  series = rep(c("Y0 (control)", "Y1 (treatment)", "Y0 (control)", "Y1 (treatment)"),
               each = length(x1_grid)),
  panel = rep(c("Constant TE, no X effect", "Constant TE, common X effect"),
              each = 2 * length(x1_grid))
)

p_simple <- ggplot(viz_df, aes(x = x1, y = mu, color = series)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  labs(
    title = "Simple linear models before interactions",
    x = "X1",
    y = "E[Y | D, X1]",
    color = NULL
  ) +
  scale_color_manual(values = c("Y0 (control)" = "steelblue",
                                "Y1 (treatment)" = "firebrick")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top") + 
  coord_cartesian(ylim = c(-1, 5))

p_simple


X1c <- X1 - mean(X1)

X_lin <- cbind(
  1,              # intercept
  D,              # treatment
  X1c,       # centered covariates
  D * X1c
)
colnames(X_lin) <- c("(Intercept)", "D", "X1c", "D:X1c")

beta_hat <- solve(t(X_lin) %*% X_lin, t(X_lin) %*% Y)
lin_tau_hat <- as.numeric(beta_hat["D",])

# -------- Separate linear models under treatment and control
# Equivalent to Y ~ X1 (with intercept) in each arm

fit_0 <- lm(Y ~ X1, subset = (D == 0))
fit_1 <- lm(Y ~ X1, subset = (D == 1))

Y0_hat <- predict(fit_0, newdata = data.frame(X1 = X1))
Y1_hat <- predict(fit_1, newdata = data.frame(X1 = X1))

sep_tau_hat <- mean(Y1_hat - Y0_hat)

# -------- Compare
cat("Lin (by hand) ATE:", round(lin_tau_hat, 4), "\n")
cat("Separate models ATE:", round(sep_tau_hat, 4), "\n")
cat("Abs difference:", round(abs(lin_tau_hat - sep_tau_hat), 6), "\n")

# -------- Plot: mean potential outcomes and treatment effect vs X1



x1_grid <- seq(min(X1), max(X1), length.out = 200)

mu0_grid <- a0_3 + b0 * x1_grid
mu1_grid <- a1_3 + b1 * x1_grid
te_grid <- mu1_grid - mu0_grid

# Reference quantities
x1_bar <- mean(X1)
te_at_mean_x1 <- (a1_3 - a0_3) + (b1 - b0) * x1_bar
te_avg <- mean((a1_3 - a0_3) + (b1 - b0) * X1)

cat("TE at mean X1 (on curve):", round(te_at_mean_x1, 4), "\n")
cat("Average TE over sample:", round(te_avg, 4), "\n")

po_df <- data.frame(
  x1 = c(x1_grid, x1_grid),
  value = c(mu0_grid, mu1_grid),
  series = rep(c("Y0 (control)", "Y1 (treatment)"), each = length(x1_grid)),
  panel = "Potential outcomes"
)
te_df <- data.frame(
  x1 = x1_grid,
  value = te_grid,
  series = "TE (Y1 - Y0)",
  panel = "Treatment effect"
)
dens <- density(X1, n = 200)
dens_df <- data.frame(
  x1 = dens$x,
  value = dens$y,
  series = "Density",
  panel = "Density of X1"
)
plot_df <- rbind(po_df, te_df, dens_df)
te_fill <- subset(plot_df, panel == "Treatment effect")

ref_lines <- data.frame(panel = "Treatment effect", x1_bar = x1_bar, te_avg = te_avg)
ref_lines2 <- data.frame(panel = "Density of X1", x1_bar = x1_bar, te_avg = te_avg)
ref_points <- data.frame(
  panel = "Treatment effect",
  x1 = c(x1_bar, x1_bar),
  value = c(te_at_mean_x1, te_avg),
  label = c("TE at mean X1", "Average TE")
)

p <- ggplot(plot_df, aes(x = x1, y = value, color = series)) +
  geom_ribbon(
    data = te_fill,
    aes(x = x1, ymin = pmin(value, 0), ymax = pmax(value, 0)),
    inherit.aes = FALSE,
    fill = "gray70",
    alpha = 0.3
  ) +
  geom_line(linewidth = 1.1) +
  geom_vline(
    data = ref_lines,
    aes(xintercept = x1_bar),
    linetype = "dashed",
    color = "gray50"
  ) +
  geom_vline(
    data = ref_lines2,
    aes(xintercept = x1_bar),
    linetype = "dashed",
    color = "gray50"
  ) +
    geom_vline(
    data = ref_lines,
    aes(xintercept = x1_bar),
    linetype = "dashed",
    color = "gray50"
  ) +
  geom_hline(
    data = ref_lines,
    aes(yintercept = te_avg),
    linetype = "dotted",
    color = "gray30"
  ) +
  geom_point(
    data = ref_points,
    aes(x = x1, y = value, shape = label),
    inherit.aes = FALSE,
    color = "gray10",
    size = 2.7,
    stroke = 0.9
  ) +
  scale_shape_manual(values = c("Average TE" = 1, "TE at mean X1" = 19)) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  labs(
    title = "Potential outcomes and TE vs X1",
    subtitle = "Dashed = mean(X1); dotted = average TE",
    x = "X1",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  scale_color_manual(values = c(
    "Y0 (control)" = "steelblue",
    "Y1 (treatment)" = "firebrick",
    "TE (Y1 - Y0)" = "black",
    "Density" = "gray80"
  )) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

p


# D is a function of X1
D <- rbinom(n, size = 1, prob = plogis(-0.5 + 0.3 * X1))

# Observed outcomes recomputed when D changes (potential outcomes can stay fixed).
Y <- D * y1 + (1 - D) * y0


ggplot(data.frame(X1 = X1, D = D), aes(x = X1, y = D)) +
  geom_jitter(height = 0.01, alpha = 0.3) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue", se = FALSE) +
  labs(
    title = "Treatment assignment D vs X1",
    x = "X1",
    y = "D (treatment indicator)"
  ) +
  theme_minimal(base_size = 12)


# ipw estimates (estimated propensity scores)
pmodel <- glm(D ~ X1, family = binomial(link = "logit"))
pscores <- predict(pmodel, type = "response")
pscores <- pmin(pmax(pscores, 1e-6), 1 - 1e-6)

pmat <- cbind(pscores, 1- pscores)
head(pmat)

dmat <- cbind(D, 1 - D)
head(dmat)

weights <- dmat / pmat
head(weights)

est_mat <- weights * Y
head(est_mat)

colMeans(est_mat)

(ipw_ate <- mean(est_mat[,1]) - mean(est_mat[,2]))


# alternatively, with scalars
(ipw_ate2 <- mean(D * Y / pscores) - mean((1 - D) * Y / (1 - pscores)))



# doubly robust scores

outcome_model <- lm(Y ~ D + X1)

m0 <- predict(outcome_model, newdata = data.frame(D = 0, X1 = X1))
m1 <- predict(outcome_model, newdata = data.frame(D = 1, X1 = X1))

mmat <- cbind(m1, m0)
rbcmat <- cbind(Y - m1, Y - m0)

dr_mat <- mmat + rbcmat*weights

# OR
# dr_mat <- cbind( 
#   m1 + D * (Y - m1) / pscores,
#   m0 + (1 - D) * (Y - m0) / (1 - pscores)
# )

head(dr_mat)

colMeans(dr_mat)

(dr_ate <- mean(dr_mat[,1]) - mean(dr_mat[,2]))

(dr_ate2 <- mean( m1 - m0 + D * (Y - m1) / pscores - (1 - D) * (Y - m0) / (1 - pscores) ))


# standard error for ipw
# re-estimate with true propensity scores
apply(est_mat, 2, function(x) sd(x) / sqrt(n))

rowdiffs <- est_mat[,1] -  est_mat[,2]
sd(rowdiffs) / sqrt(n)

# standard error for DR
apply(dr_mat, 2, function(x) sd(x) / sqrt(n))

rowdiffs_dr <- dr_mat[,1] -  dr_mat[,2]
sd(rowdiffs_dr) / sqrt(n)

# Monte Carlo: re-simulate whole datasets and compare empirical SD to estimated SE
# Goal: show that
# - IPW SE based on the IPW score works when using the *true* propensity scores
# - DR/AIPW SE based on the DR score works even when using *estimated* propensity scores
B <- 2000
ipw_true_ates <- ipw_true_ses <- dr_hat_ates <- dr_hat_ses <- numeric(B)

# Population ATE for this DGP (since E[u1 - u0] = 0 and X1 ~ N(mu_x1, 1))
true_ate <- (a1_3 - a0_3) + (b1 - b0) * mu_x1

for (b in 1:B) {
  # Resimulate a full dataset draw
  X1_b <- rnorm(n, mean = mu_x1)
  u0_b <- rnorm(n, sd = 1.0)
  u1_b <- rnorm(n, sd = 1.0)
  y0_b <- a0_3 + b0 * X1_b + u0_b
  y1_b <- a1_3 + b1 * X1_b + u1_b
  D_b <- rbinom(n, size = 1, prob = plogis(-0.5 + 0.3 * X1_b))
  Y_b <- D_b * y1_b + (1 - D_b) * y0_b

  df_b <- data.frame(Y = Y_b, D = D_b, X1 = X1_b)

  # IPW (true propensity scores)
  pscores_true_b <- plogis(-0.5 + 0.3 * X1_b)
  pscores_true_b <- pmin(pmax(pscores_true_b, 1e-6), 1 - 1e-6)
  psi_ipw_true_b <- (df_b$D * df_b$Y / pscores_true_b) -
    ((1 - df_b$D) * df_b$Y / (1 - pscores_true_b))
  ipw_true_ates[b] <- mean(psi_ipw_true_b)
  ipw_true_ses[b] <- sd(psi_ipw_true_b) / sqrt(n)

  # DR/AIPW (estimated propensity scores; outcome model is *not* interacted, on purpose)
  pmodel_hat_b <- glm(D ~ X1, data = df_b, family = binomial(link = "logit"))
  pscores_hat_b <- predict(pmodel_hat_b, type = "response")
  pscores_hat_b <- pmin(pmax(pscores_hat_b, 1e-6), 1 - 1e-6)

  outcome_model_b <- lm(Y ~ D + X1, data = df_b)
  m0_b <- predict(outcome_model_b, newdata = data.frame(D = 0, X1 = df_b$X1))
  m1_b <- predict(outcome_model_b, newdata = data.frame(D = 1, X1 = df_b$X1))
  psi_dr_hat_b <- (m1_b - m0_b) +
    df_b$D * (df_b$Y - m1_b) / pscores_hat_b -
    (1 - df_b$D) * (df_b$Y - m0_b) / (1 - pscores_hat_b)
  dr_hat_ates[b] <- mean(psi_dr_hat_b)
  dr_hat_ses[b] <- sd(psi_dr_hat_b) / sqrt(n)
}

cat("\nMonte Carlo (resimulate whole datasets)\n")
cat("B:", B, "n:", n, "\n")
cat("True ATE (population):", round(true_ate, 4), "\n")
cat("IPW (true e): mean =", round(mean(ipw_true_ates), 4),
    "emp SD =", round(sd(ipw_true_ates), 4),
    "mean SEhat =", round(mean(ipw_true_ses), 4), "\n")
cat("DR  (est e): mean =", round(mean(dr_hat_ates), 4),
    "emp SD =", round(sd(dr_hat_ates), 4),
    "mean SEhat =", round(mean(dr_hat_ses), 4), "\n")
cat("IPW (true e) 95% CI coverage:", round(mean(abs(ipw_true_ates - true_ate) <= 1.96 * ipw_true_ses), 3), "\n")
cat("DR  (est e)  95% CI coverage:", round(mean(abs(dr_hat_ates - true_ate) <= 1.96 * dr_hat_ses), 3), "\n")
