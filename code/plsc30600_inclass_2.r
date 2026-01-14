## Week 2 Exercise: Post-stratification plug-in under MAR
##  (1) Stratify on ALL covariates X
##  (2) Stratify on estimated true propensity score

set.seed(30600)
library(dplyr)
library(tidyr)
library(ggplot2)

custom_theme <- theme(
  plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 11),
  legend.position = "bottom",
  panel.grid.minor = element_blank()
)



cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## ---------------------------------------------------------
## 0) Simulate an experimental study with known truth
##    (Discrete covariates so post-stratification is literal)
## ---------------------------------------------------------
n  <- 5000
x1 <- rbinom(n, 1, 0.5)
x2 <- sample(c("A","B","C"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2))
x3 <- rbinom(n, 1, 0.3)

# True treatment propensity e_true(X): uses all covariates but coarsens strata
ps_group <- (x1*x3 + (x2 == "B"))
ps_map <- c("0" = 0.25, "1" = 0.50, "2" = 0.75)
e_true <- ps_map[as.character(ps_group)]
d <- rbinom(n, 1, e_true)

# Potential outcomes and observed outcome
tau <- 1.5
y0  <- 1 + 1.0*x1 - 0.7*(x2=="B") + 0.4*(x2=="C") + 0.9*x3 + rnorm(n, sd=1)
y1  <- y0 + tau
y   <- ifelse(d==1, y1, y0)

dat <- data.frame(y=y, d=d, x1=x1, x2=x2, x3=x3, e_true=e_true, y0=y0, y1=y1)

# Naive (confounded) ATE estimate: difference in sample means
ate_naive <- mean(dat$y[dat$d==1]) - mean(dat$y[dat$d==0])

# equivalent matrix estimator
D_vec <- matrix(dat$d, ncol = 1)
Y_vec <- matrix(dat$y, ncol = 1)
one_vec <- matrix(1, nrow = n, ncol = 1)
n0 <- c(t(one_vec) %*% (one_vec - D_vec))
n1 <- c(t(one_vec) %*% D_vec)
t((D_vec / n1) - ((one_vec - D_vec) / n0)) %*% Y_vec

## ---------------------------------------------------------
## 1) Plug-in post-stratification on ALL covariates X
##
## Under MAR/ignorability:
##   E[Y(1)] = sum_x E[Y | d=1, X=x] * P(X=x)
##   E[Y(0)] = sum_x E[Y | d=0, X=x] * P(X=x)
##   ATE = E[Y(1)] - E[Y(0)]
## ---------------------------------------------------------
dat$strata_X <- interaction(dat$x1, dat$x2, dat$x3, drop=TRUE)

w_X  <- prop.table(table(dat$strata_X))                 # P(X=x) plug-in
m1_X <- tapply(dat$y[dat$d==1], dat$strata_X[dat$d==1], mean)  # E[Y | d=1, X=x]
m0_X <- tapply(dat$y[dat$d==0], dat$strata_X[dat$d==0], mean)  # E[Y | d=0, X=x]

# Align by strata names
all_s <- names(w_X)
m1    <- m1_X[all_s]
m0    <- m0_X[all_s]
w     <- as.numeric(w_X); names(w) <- all_s

# Keep only strata with both treated and control in sample (sample-positivity check)
ok <- !is.na(m1) & !is.na(m0)
mass_ok <- sum(w[ok])

E1_X <- sum((w[ok]/mass_ok) * m1[ok])
E0_X <- sum((w[ok]/mass_ok) * m0[ok])
ate_post_X <- E1_X - E0_X

## ---------------------------------------------------------
## 2) Plug-in post-stratification on PROPENSITY SCORE strata
##
## Rosenbaum-Rubin dimension reduction: instead of stratifying on X,
## stratify on propensity score of X
## Same plug-in logic:
##   E[Y(1)] = sum_s E[Y | d=1, S=s] * P(S=s)
## ---------------------------------------------------------
#ps_est <- glm(d ~ x1 + x2 + x3, family = binomial(), data = dat)

w_S  <- prop.table(table(dat$e_true))                 # P(S=s) plug-in
m1_S <- tapply(dat$y[dat$d==1], dat$e_true[dat$d==1], mean)  # E[Y | d=1, S=s]
m0_S <- tapply(dat$y[dat$d==0], dat$e_true[dat$d==0], mean)  # E[Y | d=0, S=s]

# Align by strata names
all_s <- names(w_S)
m1    <- m1_S[all_s]
m0    <- m0_S[all_s]
w     <- as.numeric(w_S); names(w) <- all_s

# Keep only strata with both treated and control in sample (empirical positivity check)
ok <- !is.na(m1) & !is.na(m0)
mass_ok <- sum(w[ok])

E1_S <- sum((w[ok]/mass_ok) * m1[ok])
E0_S <- sum((w[ok]/mass_ok) * m0[ok])
ate_post_ps <- E1_S - E0_S

## ---------------------------------------------------------
## 4) Sampling distribution across repeated experiments
## ---------------------------------------------------------
simulate_once <- function(n) {
  x1 <- rbinom(n, 1, 0.5)
  x2 <- sample(c("A","B","C"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2))
  x3 <- rbinom(n, 1, 0.3)
  
  ps_group <- (x1 + (x2 == "B") + 2 * (x2 == "C") + x3) %% 3
  ps_map <- c("0" = 0.25, "1" = 0.50, "2" = 0.75)
  e_true <- ps_map[as.character(ps_group)]
  d <- rbinom(n, 1, e_true)
  
  y0 <- 1 + 1.0 * x1 - 0.7 * (x2 == "B") + 0.4 * (x2 == "C") + 0.9 * x3 + rnorm(n, sd = 1)
  y1 <- y0 + tau
  y <- ifelse(d == 1, y1, y0)
  
  dat <- data.frame(y = y, d = d, x1 = x1, x2 = x2, x3 = x3, e_true = e_true)
  
  ate_naive <- mean(dat$y[dat$d == 1]) - mean(dat$y[dat$d == 0])
  
  D_vec <- matrix(dat$d, ncol = 1)
  Y_vec <- matrix(dat$y, ncol = 1)
  one_vec <- matrix(1, nrow = n, ncol = 1)
  n0 <- c(t(one_vec) %*% (one_vec - D_vec))
  n1 <- c(t(one_vec) %*% D_vec)
  ate_naive_mat <- as.numeric(t((D_vec / n1) - ((one_vec - D_vec) / n0)) %*% Y_vec)
  
  dat$strata_X <- interaction(dat$x1, dat$x2, dat$x3, drop = TRUE)
  w_X <- prop.table(table(dat$strata_X))
  m1_X <- tapply(dat$y[dat$d == 1], dat$strata_X[dat$d == 1], mean)
  m0_X <- tapply(dat$y[dat$d == 0], dat$strata_X[dat$d == 0], mean)
  
  all_s <- names(w_X)
  m1 <- m1_X[all_s]
  m0 <- m0_X[all_s]
  w <- as.numeric(w_X)
  names(w) <- all_s
  
  ok <- !is.na(m1) & !is.na(m0)
  mass_ok <- sum(w[ok])
  E1_X <- sum((w[ok] / mass_ok) * m1[ok])
  E0_X <- sum((w[ok] / mass_ok) * m0[ok])
  ate_post_X <- E1_X - E0_X
  
  w_S <- prop.table(table(dat$e_true))
  m1_S <- tapply(dat$y[dat$d == 1], dat$e_true[dat$d == 1], mean)
  m0_S <- tapply(dat$y[dat$d == 0], dat$e_true[dat$d == 0], mean)
  
  all_ps <- names(w_S)
  m1ps <- m1_S[all_ps]
  m0ps <- m0_S[all_ps]
  wps <- as.numeric(w_S)
  names(wps) <- all_ps
  
  ok2 <- !is.na(m1ps) & !is.na(m0ps)
  mass_ok2 <- sum(wps[ok2])
  E1_S <- sum((wps[ok2] / mass_ok2) * m1ps[ok2])
  E0_S <- sum((wps[ok2] / mass_ok2) * m0ps[ok2])
  ate_post_ps <- E1_S - E0_S
  
  c(
    ate_naive = ate_naive,
    ate_naive_mat = ate_naive_mat,
    ate_post_X = ate_post_X,
    ate_post_ps = ate_post_ps
  )
}

B <- 1000
sim_mat <- replicate(B, simulate_once(n = n))
sim_df <- as.data.frame(t(sim_mat))

est_names <- c("ate_naive", "ate_naive_mat", "ate_post_X", "ate_post_ps")
summary_tbl <- data.frame(
  estimator = est_names,
  mean = sapply(sim_df[est_names], mean),
  variance = sapply(sim_df[est_names], var),
  bias = sapply(sim_df[est_names], mean) - tau,
  mse = sapply(sim_df[est_names], function(x) mean((x - tau)^2))
)


# Plotting

sim_long <- sim_df |>
  pivot_longer(cols = c(ate_naive, ate_naive_mat, ate_post_X, ate_post_ps),
               names_to = "estimator", values_to = "estimate") |>
  mutate(
    estimator = factor(estimator,
                       levels = c("ate_naive", "ate_naive_mat", "ate_post_X", "ate_post_ps"),
                       labels = c(
                         "Naive\ndifference in means",
                         "Difference in means\n(matrix)",
                         "Post-stratification\non X",
                         "Post-stratification\non e(X)"
                       ))
  )

plot_sims <- ggplot(sim_long, aes(x = estimator, y = estimate, 
                                  fill = estimator)) +
  ggdist::stat_slab(aes(thickness = after_stat(pdf * n)),
                    scale = 0.7, orientation = "vertical") +
  geom_hline(yintercept = tau, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c(cbPalette[2:5])) + 
  labs(
    title = "Sampling Distribution of ATE Estimators",
    x = "",
    y = "Estimate",
    fill = ""
  ) +
  custom_theme + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


plot_sims
