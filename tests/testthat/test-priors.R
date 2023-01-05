context("Prior families")
test_that("specifying priors", {
  tol <- 0.0000005
  half_norm <- prior(
    family = "normal",
    mean = 0L,
    sd = 10L,
    range = c(0L, Inf)
  )

  testthat::expect_equal(half_norm@func(0L),
    dnorm(0L, mean = 0L, sd = 10L) * 2L,
    tolerance = tol, scale = 1L,
    label = "half normal (1)"
  )

  testthat::expect_equal(half_norm@func(-1L),
    0L,
    tolerance = tol, scale = 1L,
    label = "half normal (2)"
  )


  half_t <- prior(
    family = "student_t",
    mean = 0L,
    sd = 1L,
    df = 10L,
    range = c(0L, Inf)
  )

  testthat::expect_equal(half_t@func(0L),
    dt(0L, df = 10L) * 2L,
    tolerance = tol, scale = 1L,
    label = "half t (1)"
  )

  testthat::expect_equal(half_t@func(-1L),
    0L,
    tolerance = tol, scale = 1L,
    label = "half t (2)"
  )

  cauchy_prior <- prior(
    family = "cauchy",
    scale = 1L
  )

  testthat::expect_equal(cauchy_prior@func(0L),
    dcauchy(0L, 0L, 1L),
    tolerance = tol, scale = 1L,
    label = "cauchy (1)"
  )

  cauchy_prior <- prior(
    family = "cauchy",
    location = 2L,
    scale = 2L
  )

  testthat::expect_equal(cauchy_prior@func(4L),
    dcauchy(4L, 2L, 2L),
    tolerance = tol, scale = 1L,
    label = "cauchy (2)"
  )

  half_cauchy <- prior(
    family = "cauchy",
    location = 0L,
    scale = 1L,
    range = c(0L, Inf)
  )

  testthat::expect_equal(half_cauchy@func(0L),
    dcauchy(0L, location = 0L, scale = 1L) * 2L,
    tolerance = tol, scale = 1L,
    label = "half cauchy (1)"
  )

  testthat::expect_equal(half_cauchy@func(-1L),
    0L,
    tolerance = tol, scale = 1L,
    label = "half cauchy (2)"
  )
})
