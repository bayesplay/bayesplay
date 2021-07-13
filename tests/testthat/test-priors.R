context("Prior familys")
test_that("specifying priors", {
  tol <- 0.0000005
  half_norm <- prior(
    family = "normal",
    mean = 0,
    sd = 10,
    range = c(0, Inf)
  )

  testthat::expect_equal(half_norm@func(0),
    dnorm(0, mean = 0, sd = 10) * 2,
    tolerance = tol, scale = 1,
    label = "half normal (1)"
  )

  testthat::expect_equal(half_norm@func(-1),
    0,
    tolerance = tol, scale = 1,
    label = "half normal (2)"
  )


  half_t <- prior(
    family = "student_t",
    mean = 0,
    sd = 1,
    df = 10,
    range = c(0, Inf)
  )

  testthat::expect_equal(half_t@func(0),
    dt(0, df = 10) * 2,
    tolerance = tol, scale = 1,
    label = "half t (1)"
  )

  testthat::expect_equal(half_t@func(-1),
    0,
    tolerance = tol, scale = 1,
    label = "half t (2)"
  )

  cauchy_prior <- prior(
    family = "cauchy",
    scale = 1
  )

  testthat::expect_equal(cauchy_prior@func(0),
    dcauchy(0, 0, 1),
    tolerance = tol, scale = 1,
    label = "cauchy (1)"
  )

  cauchy_prior <- prior(
    family = "cauchy",
    location = 2,
    scale = 2
  )

  testthat::expect_equal(cauchy_prior@func(4),
    dcauchy(4, 2, 2),
    tolerance = tol, scale = 1,
    label = "cauchy (2)"
  )

  half_cauchy <- prior(
    family = "cauchy",
    location = 0,
    scale = 1,
    range = c(0, Inf)
  )

  testthat::expect_equal(half_cauchy@func(0),
    dcauchy(0, location = 0, scale = 1) * 2,
    tolerance = tol, scale = 1,
    label = "half cauchy (1)"
  )

  testthat::expect_equal(half_cauchy@func(-1),
    0,
    tolerance = tol, scale = 1,
    label = "half cauchy (2)"
  )
})
