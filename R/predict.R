#' Predict Amputation Risk
#'
#' @param perfusion int: 1-3
#' @param extent int: 1-3
#' @param depth int: 1-3
#' @param infection int: 1-4
#' @param sensation int: 1-2
#' @param age int
#' @param gender int: 0-male, 1-male
#'
#' @return The estimated probability
#'
#' @export
#'
#' @examples
#' p <- predict()
#' mean(p)
risk_statistic <- function(perfusion = 3,
                           extent = 3,
                           depth = 1,
                           infection = 1,
                           sensation = 2,
                           age = 1,
                           gender = 1) {

  pedis <- as.numeric(c(intercept = 1, perfusion, extent, depth, infection, sensation, age, gender))

  logit <- as.numeric(param_dist %*% pedis)

  probabiliy <- 1 / (1 + exp(-1 * logit))

  return(probabiliy)

}

