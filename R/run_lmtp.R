# set package library via renv
renv::activate(here::here())

# read in command line arguments and print for logging
args <- R.utils::commandArgs(
  trailingOnly = TRUE,
  asValues = TRUE,
  defaults = list(
    int_type = "mtp", # "static"
    est_type = "sdr"  # "tmle"
  )
)
print(args)

# load packages and helpers
library(here)
library(sl3)
library(lmtp)
library(tidyverse)
library(future)
library(future.apply)
library(earth)
library(arm)
library(glmnet)
library(speedglm)
library(ranger)
library(xgboost)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
source(here("R", "sl_lib.R"))
set.seed(11249)

# useful constants
trim <- 0.995     # propensity score trimming?
folds <- 5        # "outer" folds for cross-fitting
SL_folds <- 5     # "inner" folds for super learning
k <- 2            # how much history is used at each t

# parallelize and fix multithreading
progressr::handlers(global = TRUE)
if (availableCores() < 10L) {
  plan(multicore)
} else {
  # NOTE: bookkeeping for future topologies
  plan(list(
    tweak(multicore, workers = availableCores() %/% SL_folds),
    tweak(multicore, workers = SL_folds - 1L)
  ))
}
openblasctl::openblas_set_num_threads(1L)
OpenMPController::omp_set_num_threads(1L)

# load data and set outcome days window
dat_lmtp <- read_rds(here("data", "derived", "dat_final.rds"))
outcome_day <- 14
padded_days <- str_pad(0:(outcome_day - 1), 2, pad = "0")
padded_days_out <- str_pad(1:outcome_day, 2, pad = "0")

# set parameters of outcome, trt, and adjustment vars
a <-  paste0("I_", padded_days) # treatment
bs <- dat_lmtp %>% # baseline covariates
  select(-id, -fu, -event,
         -starts_with("L_"), -starts_with("C_"),
         -starts_with("Y_"), -starts_with("A_"),
         -starts_with("I_"), -starts_with("CR_"),
         -starts_with("H_")) %>% names()
y <- paste0("Y_",padded_days_out) # outcome (AKI)
cr <- paste0("CR_",padded_days_out) # competing risk (death)
censoring <- paste0("C_",padded_days) # observed at next time

used_letters <- dat_lmtp %>% # letters for time varying
  select(starts_with("L_"),
         starts_with("A_"),
         -starts_with("L_NA_"),
         -ends_with(paste0("_",outcome_day))) %>%
  names()
tv <- map(0:(outcome_day - 1), function(x) { # list for time varying covars
  used_letters[str_detect(used_letters, str_pad(x, 2, pad = "0"))]
})

# MTP is to delay everyone's intubation (I_* == 2) by 1 day.
# Instead, set them to non-invasive supp O2 (I_* == 1)
mtp <- function(data, trt) {
  # extract time point
  tau <- readr::parse_number(trt)
  # get the col name of previous trt
  trt_prev <- paste0("I_", stringr::str_pad(tau - 1, 2, "left", "0"))

  if(trt == "I_00") {
    # if first time point and intubated, set to 1
    data[data[[trt]] == 2, trt] <- 1
  } else {
    # if intubated at time T but not T-1, set to 1
    data[which(data[[trt]] == 2 & data[[trt_prev]] != 2), trt] <- 1
  }
  return(data[[trt]])
}

# set intervention type to MTP or null static intervention
shift_fun <- if (args$int_type == "mtp") {
  mtp
} else {
  NULL
}

# estimation of LMTP-based effects
if (args$est_type == "sdr") {
  # loop over time-points to estimate P(T >= t) at given t
  out_all_t <- future_lapply(seq_len(outcome_day), function(this_time) {
    # estimate survival probability under LMTP at given t
    # NOTE (special case): not really survival at t = 1, and package fails
    out <-
      lmtp_sdr(
        dat_lmtp,
        trt = a[seq_len(this_time)],
        outcome = y[seq_len(this_time)],
        comp_risk = cr[seq_len(this_time)],
        baseline = bs,
        time_vary = tv[seq_len(this_time)],
        cens = censoring[seq_len(this_time)],
        shift = shift_fun,
        outcome_type = ifelse(this_time == 1, "binomial", "survival"),
        learners_outcome = learner_stack,
        learners_trt = learner_stack,
        folds = folds,
        .SL_folds = SL_folds,
        .trim = trim,
        k = k,
        intervention_type = ifelse(args$int_type == "mtp", "mtp", "static")
      )

    # NOTE: at t = 1, estimating P(Y = 1), which is incompatible with survival
    # instead, survival would be ~= P(Y = 0) = 1 - P(Y = 1)
    if (this_time == 1) {
      # simple delta method for point and EIF estimates, set CI multiplier
      ci_mult <- abs(qnorm(p = (0.05) / 2))
      out$theta <- est <- 1 - out$theta
      out$eif <- eif <- 1 - out$eif
      out$standard_error <- std_err <- sqrt(var(eif) / length(eif))

      # binary outcome: compute on logit scale and back-transform for safety
      est_logit <- qlogis(est)
      gradient_est <- (1 / est) + (1 / (1 - est))
      std_err_logit <- sqrt(gradient_est^2 * std_err^2)
      out$low <- ci_lwr <- plogis(est_logit - ci_mult * std_err_logit)
      out$high <- ci_upr <- plogis(est_logit + ci_mult * std_err_logit)
    }

    # output
    return(out)
  }, future.seed = TRUE)

} else if (args$est_type == "tmle") {
  # loop over time-points to estimate P(T >= t) at given t
  out_all_t <- future_lapply(seq_len(outcome_day), function(this_time) {
    # estimate survival probability under LMTP at given t
    # NOTE (special case): not really survival at t = 1, and package fails
    out <-
      lmtp_tmle(
        dat_lmtp,
        trt = a[seq_len(this_time)],
        outcome = y[seq_len(this_time)],
        comp_risk = cr[seq_len(this_time)],
        baseline = bs,
        time_vary = tv[seq_len(this_time)],
        cens = censoring[seq_len(this_time)],
        shift = shift_fun,
        outcome_type = ifelse(this_time == 1, "binomial", "survival"),
        learners_outcome = learner_stack,
        learners_trt = learner_stack,
        folds = folds,
        .SL_folds = SL_folds,
        .trim = trim,
        k = k,
        intervention_type = ifelse(args$int_type == "mtp", "mtp", "static")
      )

    # NOTE: at t = 1, estimating P(Y = 1), which is incompatible with survival
    # instead, survival would be ~= P(Y = 0) = 1 - P(Y = 1)
    if (this_time == 1) {
      # simple delta method for point and EIF estimates, set CI multiplier
      ci_mult <- abs(qnorm(p = (0.05) / 2))
      out$theta <- est <- 1 - out$theta
      out$eif <- eif <- 1 - out$eif
      out$standard_error <- std_err <- sqrt(var(eif) / length(eif))

      # binary outcome: compute on logit scale and back-transform for safety
      est_logit <- qlogis(est)
      gradient_est <- (1 / est) + (1 / (1 - est))
      std_err_logit <- sqrt(gradient_est^2 * std_err^2)
      out$low <- ci_lwr <- plogis(est_logit - ci_mult * std_err_logit)
      out$high <- ci_upr <- plogis(est_logit + ci_mult * std_err_logit)
    }

    # output
    return(out)
  }, future.seed = TRUE)
}

# save results in output directory
trim_num <- str_split(as.character(trim), "\\.")[[1]][2]
date_stamp <- str_replace_all(Sys.Date(), "-", "")
file_to_save <- paste0("lmtp_", args$int_type, "_", args$est_type,
                       "_tv_locf_", trim_num, "_k", k, "_f", folds,
                       "_fullcohort_", date_stamp, ".rds")
saveRDS(object = out_all_t, file = here("data", "results", file_to_save))
