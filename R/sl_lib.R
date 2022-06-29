# set up SL library
mars_grid_params <- list(
  degree = c(2, 3),
  penalty = c(1, 2, 3)
)
mars_grid <- expand.grid(mars_grid_params, KEEP.OUT.ATTRS = FALSE)
mars_learner_grid <- apply(mars_grid, MARGIN = 1, function(tuning_params) {
  do.call(Lrnr_earth$new, as.list(tuning_params))
})

# keeping it old school
mean_learner <- Lrnr_mean$new()
fglm_learner <- Lrnr_glm_fast$new()
bglm_learner <- Lrnr_bayesglm$new()

# regularized models
lasso_learner <- Lrnr_glmnet$new(alpha = 1)
ridge_learner <- Lrnr_glmnet$new(alpha = 0)
enet_learner <- Lrnr_glmnet$new(alpha = 0.5)

# xgboost
xgb_tune_grid <- list(
  nrounds = c(5, 20, 50),
  max_depth = c(6, 10),
  subsample = c(0.75, 1)
)
xgb_tune_grid <- expand.grid(xgb_tune_grid, KEEP.OUT.ATTRS = FALSE)
xgb_learner_grid <- apply(xgb_tune_grid, MARGIN = 1, function(tuning_params) {
  do.call(Lrnr_xgboost$new, as.list(tuning_params))
})

# random forests
rf_tune_grid <- list(
  num.trees = c(200, 500),
  max.depth = c(0, 1),
  sample.fraction = c(0.5, 0.75),
  oob.error = FALSE,
  verbose = FALSE
)
rf_tune_grid <- expand.grid(rf_tune_grid, KEEP.OUT.ATTRS = FALSE)
rf_learner_grid <- apply(rf_tune_grid, MARGIN = 1, function(tuning_params) {
  do.call(Lrnr_ranger$new, as.list(tuning_params))
})

# list learners + create stack for SL
learners <- unlist(
  list(
    mars_learner_grid,
    lasso_learner,
    ridge_learner,
    enet_learner,
    #bart_learner,
    xgb_learner_grid,
    rf_learner_grid,
    fglm_learner,
    bglm_learner,
    mean_learner
  ),
  recursive = TRUE
)
learner_stack <- make_learner(Stack, learners)
