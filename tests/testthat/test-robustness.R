context("Robustness regions")
test_that("Basic computations", {


# first compare two-sided Bayesfactors
  mean <- -0.09129544
  se <- 0.1225285
  df <- 49
  rr <- bfrr(
    data_model = likelihood("student_t", mean, se, df),
    alternativeprior = prior("normal", mean = 0, sd = 0.25),
    nullprior = prior("point", 0),
    parameters = list(mean = c(-2, 2), sd = c(0, 2)),
    precision = 0.05,
    cutoff = 6,
    multicore = FALSE
  )


  # load data from bfrr package (see: github.com/debruine/bfrr)
  rr_test <- read.csv("rr_data_two.csv")


  testthat::expect_true(
    all(rr$mean == rr_test$M),
    label = "means equal (two)"
  )

  testthat::expect_true(
    all(rr$sd == rr_test$SD),
    label = "sds equal (two)"
  )

  comp <- function(x, y) {
    testthat::expect_equal(
      x,
      y,
      tolerance = 0.0001, scale = 1,
      label = paste0("two-sided BF values ", x, " ", y)
    )
  }

  res <- mapply(comp, rr$bf, rr_test$BF)

  # next compare one-sided tests

  # load data from bfrr package (see: github.com/debruine/bfrr)
  mean <- -0.09129544
  se <- 0.1225285
  df <- 49
  rr <- bfrr(
    data_model = likelihood("student_t", mean, se, df),
    alternativeprior = prior("normal", mean = 0, sd = 0.25, range = c(0, Inf)),
    nullprior = prior("point", 0),
    parameters = list(mean = c(-2, 2), sd = c(0, 2)),
    precision = 0.05,
    cutoff = 6,
    multicore = FALSE
  )

  rr_test <- read.csv("rr_data_one.csv")


  testthat::expect_true(
    all(rr$mean == rr_test$M),
    label = "means equal (one)"
  )

  testthat::expect_true(
    all(rr$sd == rr_test$SD),
    label = "sds equal (one)"
  )

  comp <- function(x, y) {
    testthat::expect_equal(
      x,
      y,
      tolerance = 0.0001, scale = 1,
      label = paste0("one-sided BF values ", x, " ", y)
    )
  }




})
