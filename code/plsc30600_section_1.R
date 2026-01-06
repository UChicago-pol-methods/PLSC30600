#' Simulation Study
#'
#' Design a simulation that compares the behavior of three approaches:
#' - Manski bounds
#' - MCAR estimator (respondent mean)
#' - MAR estimator (stratified by X or modeled via E[Y | R = 1, X])
#'
#'
#' Requirements:
#' 1. Specify a data-generating process for (X, Y) with X in {0, 1} and
#'    Y in {0, 1}.
#' 2. Consider three missingness mechanisms for R:
#'    - MCAR: R is independent of (X, Y)
#'    - MAR: R depends on X but not on Y given X
#'    - MNAR (Missing Not at Random): R depends on Y even after conditioning
#'      on X
#' 3. Estimation:
#'    - Write code that takes (Y*, R) and returns the plug-in Manski
#'     bounds for E[Y] when Y is in [0, 1].
#'    - Write code that computes the MCAR plug-in estimator mu_hat_MCAR.
#'    - Write code that computes the MAR plug-in estimator mu_hat_MAR by
#'    stratification on X in {A, B}.
#'
#' 3. For each mechanism, run at least S = 500 simulated datasets
#'    (choose n >= 200).
#' 4. For each mechanism and estimator, report:
#'    - empirical bias (average estimate minus true mu)
#'    - empirical variance
#'    - at least one figure (e.g., histograms/boxplots of estimates, or
#'      bias/variance summaries)
#' 5. For Manski bounds, also report the fraction of simulations in which the
#'    true mu lies inside the sample plug-in bounds (and briefly comment on
#'    what you see).
