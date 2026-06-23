test_that("binomial likelihood", {
  l <- likelihood(family = "binomial", 3L, 12L)
  p1 <- prior(family = "beta", alpha = 1L, beta = 2L)
  p0 <- prior(family = "point", point = 0.5)
  expect_bf_equal(bayes_factor(l, p1, p0),
    1L / 0.4887695,
    label = "binomial likelihood, beta prior",
    tolerance = tol
  )

  l <- likelihood(family = "binomial", 3L, 12L)
  p1 <- prior(family = "uniform", min = 0L, max = 1L)
  p0 <- prior(family = "point", point = 0.5)
  expect_bf_equal(bayes_factor(l, p1, p0),
    1L / 0.6982422,
    label = "binomial likelihood, uniform prior",
    tolerance = tol
  )

  data_model <- likelihood(family = "binomial", 3L, trials = 12L)
  alt_prior <- prior(family = "beta", 1L, 1L)
  null_prior <- prior(family = "point", point = 0.5)
  b2 <- 1L / 0.6982422
  expect_bf_equal(bayes_factor(data_model, alt_prior, null_prior),
    b2,
    label = "binomial likelihood, beta uniform",
    tolerance = tol
  )

  data_model <- likelihood(family = "binomial", successes = 2L, trials = 10L)
  alt_prior <- prior(family = "beta", alpha = 1L, beta = 2.5)
  null_prior <- prior(family = "point", point = 0.5)
  b2 <- 3.3921325
  expect_bf_equal(bayes_factor(data_model, alt_prior, null_prior),
    b2,
    label = "binomial likelihood, beta prior",
    tolerance = tol
  )
})
