# LMTP Competing Risks Illustrative Application

![](output/graphs/figure_alluvial.jpg)

This repository contains the code used to generate the application for [*Causal survival analysis under competing risks using longitudinal modified treatment policies* (Diaz, Hoffman, and Hejazi 2022)](https://arxiv.org/abs/2202.03513). The data (`data/derived/dat_final.rds`) is synthetic, since the actual data set used contains protected health information.

The `R/` folder contains code to create a descriptive alluvial plot, define super learning libraries for estimation, call `lmtp::lmtp_tmle()` and `lmtp::lmtp_sdr()` for every time point, estimate confidence intervals, and visualize the results, as described in the paper. All code written by [Katherine Hoffman](https://github.com/kathoffman) or [Nima Hejazi](https://github.com/nhejazi).