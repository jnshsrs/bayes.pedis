#' Initialize the Model
#'
#' @return
#' @export
model <- function(data = pedis,
                  formula = any_amputation ~ p + e_ordinal_5 + d + i + s + alter_bei_aufnahme + gender,
                  prior = cauchy(scale = c(.5, .5, .5, .5, .5, .5, .5)),
                  n_iter = 1e3,
                  thinning = NULL,
                  seed = 234241,
                  ...) {

  options(mc.cores = parallel::detectCores())

  # create model
  model <- rstanarm::stan_glm(
    formula,
    data = data,
    prior_intercept = normal(),
    prior = prior,
    family = binomial(link = "logit"),
    QR = FALSE,
    thin = thinning,
    warmup = 500,
    iter = n_iter,
    seed = seed
  )

  # rstanarm::bayes_R2(model)
  # rstanarm::posterior_interval(model)
  # rstanarm::posterior_linpred(model, transform = TRUE)

  # Set names of predictors
  predictors <- c("Perfusion", "Extend", "Depth", "Infection", "Sensation")

  # Rename columnnames of posterior parameter distribution
  pedis_model <- as_pedis_model(model) %>%
    set_names(c("Intercept", "Perfusion", "Extend", "Depth", "Infection", "Sensation", "Age", "Gender"))

  # summarize model
  summary_model <- summary(pedis_model, predictors, ...)

  # plot posterior distribution (parameters, exponentiated)
  p <- plot_posterior(pedis_model, predictors = predictors, prior = prior)

  class(summary_model) <- append("bayesian_multivar_logreg", class(summary_model))

  l <- list(summary_model, pedis_model, p, model)

  return(l)

}
