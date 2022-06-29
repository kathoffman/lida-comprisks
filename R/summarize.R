# set package library via renv
renv::activate(here::here())

# load packages and set seed
library(here)
library(lmtp)
library(sl3)
library(data.table)
library(tidyverse)
library(conflicted)
library(matrixStats)
library(isotone)
library(scales)
library(knitr)
library(kableExtra)
library(patchwork)
library(MetBrewer)
library(patchwork)

set.seed(11249)

# helpers, settings, and constants
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
source(here("R", "utils.R"))
source(here("R", "vis.R"))
ci_level <- 0.95
ci_type <- "simult" #"marginal"


# load results
results_files <- list(
  "lmtp_mtp_sdr_tv_locf_995_k2_f5_fullcohort_20220127.rds",
  "lmtp_mtp_tmle_tv_locf_995_k2_f5_fullcohort_20220127.rds",
  "lmtp_static_sdr_tv_locf_995_k2_f5_fullcohort_20220127.rds",
  "lmtp_static_tmle_tv_locf_995_k2_f5_fullcohort_20220127.rds"
)
results_sdr_mtp <- read_rds(here("data", "results", results_files[[1]]))
results_tmle_mtp <- read_rds(here("data", "results", results_files[[2]]))
results_sdr_static <- read_rds(here("data", "results", results_files[[3]]))
results_tmle_static <- read_rds(here("data", "results", results_files[[4]]))

# clean up results using helper functions
sdr_summary <- summarize_results(
  results_sdr_mtp, results_sdr_static,
  ci_level = ci_level, ci_type = ci_type
)
tmle_summary <- summarize_results(
  results_tmle_mtp, results_tmle_static,
  ci_level = ci_level, ci_type = ci_type
)

# create graphics of results
p_surv_sdr <- sdr_summary$surv_est %>%
  bind_rows(.id = "trt_type") %>%
  mutate(
    trt_type = case_when(
      trt_type == "1" ~ "Delayed intubation (MTP)",
      trt_type == "2" ~ "No intervention"
    )
  ) %>%
  plot_surv(est_lab = "SDR")
ggsave(p_surv_sdr, width = 12, height = 8,
       file = here("graphs", "sdr_surv_est.pdf"))

p_survdiff_sdr <- sdr_summary$diff_est %>%
  mutate(
    p_adj = p.adjust(pval, "bonferroni"),
  ) %>%
  select(-std_err, -test_stat, -pval) %>%
  plot_survdiff(est_lab = "SDR")
ggsave(p_survdiff_sdr, width = 12, height = 8,
       file = here("graphs", "sdr_survdiff_est.pdf"))

p_surv_sdr_paneled <- p_surv_sdr + p_survdiff_sdr +
  plot_layout(ncol=2) + plot_annotation(tag_levels = "A")
ggsave(p_surv_sdr_paneled, width = 20, height = 9,
       file = here("graphs", "sdr_surv_paneled.pdf"))

p_surv_tmle <- tmle_summary$surv_est %>%
  bind_rows(.id = "trt_type") %>%
  mutate(
    trt_type = case_when(
      trt_type == "trt" ~ "Delayed intubation (MTP)",
      trt_type == "ctl" ~ "No intervention"
    )
  ) %>%
  plot_surv(est_lab = "TMLE")
ggsave(p_surv_tmle, width = 12, height = 8,
       file = here("graphs", "tmle_surv_est.pdf"))

p_survdiff_tmle <- tmle_summary$diff_est %>%
  mutate(
    p_adj = p.adjust(pval, "bonferroni"),
  ) %>%
  select(-std_err, -test_stat, -pval) %>%
  plot_survdiff(est_lab = "TMLE")
ggsave(p_survdiff_tmle, width = 12, height = 8,
       file = here("graphs", "tmle_survdiff_est.pdf"))

p_surv_tmle_paneled <- (p_surv_tmle + xlab("")) | p_survdiff_tmle
ggsave(p_surv_tmle_paneled, width = 20, height = 12,
       file = here("graphs", "tmle_surv_paneled.pdf"))

# create tables of results
tab_survdiff_sdr <- sdr_summary$diff_est %>%
  mutate(
    p_adj = p.adjust(pval, "bonferroni"),
    p_adj = paste0("$", pvalue(p_adj), "$")
  ) %>%
  select(-std_err, -test_stat, -pval) %>%
  kable(
    col.names = c("Day", "Lower CL", "Estimate", "Upper CL", "Adj. P-value"),
    format = "latex",
    booktabs = TRUE,
    caption = "Estimated survival difference between delayed intubation and no intervention based on SDR.",
    label = "survdiff_sdr",
    digits = 4,
    escape = FALSE
  ) %>%
  kable_styling()
write(tab_survdiff_sdr, here("tables", "sdr_survdiff_summary.tex"))

tab_survdiff_tmle <- tmle_summary$diff_est %>%
  mutate(
    p_adj = p.adjust(pval, "bonferroni"),
    p_adj = paste0("$", pvalue(p_adj), "$")
  ) %>%
  select(-std_err, -test_stat, -pval) %>%
  kable(
    col.names = c("Day", "Lower CL", "Estimate", "Upper CL", "Adj. P-value"),
    format = "latex",
    booktabs = TRUE,
    caption = "Estimated survival difference between delayed intubation and no intervention based on TMLE.",
    label = "survdiff_tmle",
    digits = 4
  ) %>%
  kable_styling()
write(tab_survdiff_tmle, here("tables", "tmle_survdiff_summary.tex"))
