#' Return Posterio Historgramm for given
#'
#' @param pedis The PEDIS Classes in a Vector
#' @param parameter The posterior distribution (a matrix n times 7)
#'
#' @return
#' @export
#'
#' @examples
#' pedis <- c("P" = 3, "E" = 3, "D" = 3, "I" = 1, "S" = 1, "Age" = 60, "Gender" = 1) # Gender Male
#'
histogramm_posterior <- function(pedis, parameter = param_dist) {

  data <- tibble::as_tibble(pedis) %>% tibble::add_column("Intercept" = 1, .before = "Perfusion")

  logit <- as.numeric(parameter %*% t(data))

  probability <- 1 / (1 + exp(-1 * logit))

  hist(probability, xlim = c(0, 1), main = "Posterior Probability of Amputation", breaks = 75)

}
