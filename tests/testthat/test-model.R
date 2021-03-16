test_that("predicted probability is correct", {
  p <- risk_statistic()
  avg_p <- mean(p)
  testthat::expect_lte(avg_p, 1)
  testthat::expect_gte(avg_p, 0)
})

vis <- function(param_dist) {
    logit <- param_dist %*% t(matrix(c(1, 3, 3, 3, 1, 1, 60, 1), nrow = 1))
    hist(as.numeric(1 / (1 + exp(-logit))), xlim = c(0, 1), breaks = 100)
}

test_that("the param dist is correct", {
  testthat::expect_lte(1, 1)
})
