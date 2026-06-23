test_that("normal likelihoods", {
  l <- likelihood(family = "normal", mean = 5L, sd = 10L)
  p1 <- prior(family = "uniform", 0L, 20L)
  p0 <- prior(family = "point", point = 0L)
  expect_bf_equal(bayes_factor(l, p1, p0),
    unclass(0.89),
    tolerance = tol,
    label = "uniform prior"
  )

  l <- likelihood(family = "normal", mean = 5.5, sd = 32.35)
  p1 <- prior(
    family = "normal", mean = 0L, sd = 13.3,
    range = c(0L, Inf)
  )
  p0 <- prior(family = "point", point = 0L)
  expect_bf_equal(bayes_factor(l, p1, p0),
    unclass(0.97),
    tolerance = tol,
    label = "normal prior"
  )

  l <- likelihood(family = "normal", mean = 0.63, sd = 0.43)
  p1 <- prior(
    family = "normal", mean = 0L, sd = 2.69,
    range = c(0L, Inf)
  )
  p0 <- prior(family = "point", point = 0L)
  expect_bf_equal(bayes_factor(l, p1, p0),
    unclass(0.83),
    tolerance = tol,
    label = "half-normal prior"
  )

  l <- likelihood(family = "normal", mean = 15L, sd = 13L)
  p1 <- prior(family = "normal", mean = 50L, sd = 14L)
  p0 <- prior(family = "point", point = 0L)
  expect_bf_equal(bayes_factor(l, p1, p0),
    unclass(0.247),
    tolerance = tol,
    label = "normal prior"
  )
})
