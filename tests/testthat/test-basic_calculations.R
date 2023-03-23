context("Basic calculations")

# set the tolerance for the tests
tol <- 0.005

test_that("binomial likelihood", {

  l <- likelihood(family = "binomial", 3L, 12L)
  p1 <- prior(family = "beta", alpha = 1L, beta = 2L)
  p0 <- prior(family = "point", point = 0.5)
  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0

  testthat::expect_equal(as.numeric(unclass(b)),
   1L / 0.4887695,
    label = "binomial likelihood, beta prior",
    tolerance = tol, scale = 1L
  )

  l <- likelihood(family = "binomial", 3L, 12L)
  p1 <- prior(family = "uniform", min = 0L, max = 1L)
  p0 <- prior(family = "point", point = 0.5)
  p0 <- prior(family = "point", point = 0.5)
  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0
  testthat::expect_equal(as.numeric(unclass(b)),
    1L / 0.6982422,
    label = "binomial likelihood, uniform prior",
    tolerance = tol, scale = 1L
  )


  data_model <- likelihood(family = "binomial", 3L, trials = 12L)
  alt_prior <- prior(family = "beta", 1L, 1L)
  null_prior <- prior(family = "point", point = 0.5)
  m1 <- data_model * alt_prior
  m0 <- data_model * null_prior
  b1 <- integral(m1) / integral(m0)
  b2 <- 1L / 0.6982422
  testthat::expect_equal(as.numeric(unclass(b1)),
    unclass(unname(b2)),
    label = "binomial likelihood, beta uniform",
    tolerance = tol, scale = 1L
  )


  data_model <- likelihood(family = "binomial", successes = 2L, trials = 10L)
  alt_prior <- prior(family = "beta", alpha = 1L, beta = 2.5)
  null_prior <- prior(family = "point", point = 0.5)
  m1 <- data_model * alt_prior
  m0 <- data_model * null_prior
  b1 <- integral(m1) / integral(m0)
  b2 <- 3.3921325
  testthat::expect_equal(as.numeric(unclass(b1)),
    unclass(unname(b2)),
    label = "binomial likelihood, beta prior",
    tolerance = tol, scale = 1L
  )

})


test_that("noncentral_d likelhood", {


  l <- likelihood(family = "noncentral_d", d = 0.22696089971622862569, n = 80L)
  p1 <- prior("cauchy", location = 0L, scale = 1L)
  p0 <- prior("point", 0L)
  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0

  testthat::expect_equal(as.numeric(unclass(b)),
    0.64207642378841778275,
    tolerance = tol, scale = 1L,
    label = "default bayes t-test (noncentral_d likelihood)"
  )


  l <- likelihood(family = "noncentral_d", d = 0.22696089971622862569, n = 80L)
  p1 <- prior("cauchy", location = 0L, scale = 1L, range = c(0L, Inf))
  p0 <- prior("point", 0L)

  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0

  testthat::expect_equal(as.numeric(unclass(b)),
    1.25399711580663364430,
    tolerance = tol, scale = 1L,
    label = "default bayes t-test (noncentral_d likelihood)"
  )


  l <- likelihood("noncentral_d", 0.624524917476492, 51L)
  p1 <- prior("cauchy", 0L, 0.707)
  p0 <- prior("point", 0L)
  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0
  testthat::expect_equal(as.numeric(unclass(b)),
    460.2497,
    label = "previously anomalous t test v2 (as d)",
    tolerance = tol, scale = 1L
  )


  l <- likelihood("noncentral_d", -2.24, 34L)
  p1 <- prior("cauchy", 0L, 0.707, c(0L, Inf))
  p0 <- prior("point", 0L)
  b <- suppressWarnings(integral(l * p1) / integral(l * p0))

  testthat::expect_equal(as.numeric(unclass(b)),
    0.006772853,
    label = "previously anomalous t test v2 (as t)",
    tolerance = tol, scale = 1L
  )


})



test_that("noncentral_d2 likelihoods", {

  l <- likelihood("noncentral_d2", -0.16866426138921944422, 17L,  18L)
  p1 <- prior("cauchy", location = 0L, scale = 1L)
  p0 <- prior("point", 0L)

  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0

  testthat::expect_equal(as.numeric(unclass(b)),
   0.274111,
    tolerance = tol, scale = 1L,
    label = "default bayes t-test (noncentral_t likelihood)"
  )

# })
#
#
# test_that("noncentral_t likelihoods", {

  l <- likelihood("noncentral_t", t = -0.49871193286623344276, df = 33L)
  p1 <- prior("cauchy", location = 0L, scale = 2.95683228182748658597)
  p0 <- prior("point", 0L)
  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0

  testthat::expect_equal(as.numeric(unclass(b)),
   0.274111,
    tolerance = tol, scale = 1L,
    label = "default bayes t-test (noncentral_t likelihood)"
  )


  l <- likelihood("noncentral_t", t = 4.71774600375525743345, df = 9L)
  p1 <- prior("cauchy", location = 0L, scale = 3.16227766016837952279)
  p0 <- prior("point", 0L)
  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0

  testthat::expect_equal(as.numeric(unclass(b)),
    42.44814,
    tolerance = tol, scale = 1L,
    label = "default bayes t-test (noncentral_t likelihood)"
  )


  l <- likelihood(family = "noncentral_t", t = 2.03, df = 79L)
  p1 <- prior(family = "cauchy", location = 0L, scale = 8.94427190999915922021)
  p0 <- prior("point", 0L)
  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0

  testthat::expect_equal(as.numeric(unclass(b)),
  0.64207642378841778275,
    label = "default bayes t (t version)",
    tolerance = tol, scale = 1L
  )

  l <- likelihood(family = "noncentral_t", t = 4.46, df = 49L)
  p1 <- prior("cauchy", location = 0L, scale = sqrt(50L))
  p0 <- prior("point", 0L)
  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0

  testthat::expect_equal(as.numeric(unclass(b)),
    403.35222779080226018777,
    label = "anomalous t test v1 (as t)",
    tolerance = tol, scale = 1L
  )

  l <- likelihood(family = "noncentral_t", t = 4.46, df = 50L)
  p1 <- prior("cauchy", 0L, 5.0489898989798)
  p0 <- prior("point", 0L)

  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0


  testthat::expect_equal(as.numeric(unclass(b)),
    460.2497,
    label = "previously anomalous t test v2 (as t)",
    tolerance = tol, scale = 1L
  )

})

test_that("student_t likelihoods", {


  l <- likelihood("student_t", mean = 5.47, sd = 32.2, df = 119L)
  p1 <- prior("student_t", mean = 13.3, sd = 4.93, df = 72L)

  p0 <- prior("point", 0L)
  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0


  testthat::expect_equal(as.numeric(unclass(b)),
    unclass(0.97),
    tolerance = tol, scale = 1L,
    label = "student_t prior (student_t likelihood)"
  )

})

test_that("normal likelihoods", {


  l <- likelihood(family = "normal", mean = 5L, sd = 10L)
  p1 <- prior(family = "uniform", 0L, 20L)
  p0 <- prior(family = "point", point = 0L)
  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0

  testthat::expect_equal(as.numeric(unclass(b)),
    unclass(0.89),
    tolerance = tol, scale = 1L,
    label = "uniform prior"
  )


  l <- likelihood(family = "normal", mean = 5.5, sd = 32.35)
  p1 <- prior(
    family = "normal", mean = 0L, sd = 13.3,
    range = c(0L, Inf)
  )
  p0 <- prior(family = "point", point = 0L)
  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0

  testthat::expect_equal(as.numeric(unclass(b)),
    unclass(0.97),
    tolerance = tol, scale = 1L,
    label = "normal prior"
  )


  l <- likelihood(family = "normal", mean = 0.63, sd = 0.43)
  p1 <- prior(
    family = "normal", mean = 0L, sd = 2.69,
    range = c(0L, Inf)
  )
  p0 <- prior(family = "point", point = 0L)
  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0

  testthat::expect_equal(as.numeric(unclass(b)),
    unclass(0.83),
    tolerance = tol, scale = 1L,
    label = "half-normal prior"
  )


  l <- likelihood(family = "normal", mean = 15L, sd = 13L)
  p1 <- prior(family = "normal", mean = 50L, sd = 14L)
  p0 <- prior(family = "point", point = 0L)
  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0

  testthat::expect_equal(as.numeric(unclass(b)),
    unclass(0.25),
    tolerance = tol, scale = 1L,
    label = "normal prior"
  )


})


test_that("noncentral_d likelihoods", {

  l <- likelihood(family = "noncentral_d", d = 1.49188227940238959945, n = 10L)
  p1 <- prior(family = "cauchy", 0L, 1L)
  p0 <- prior(family = "point", 0L)
  m1 <- integral(l * p1)
  m0 <- integral(l * p0)
  b <- m1 / m0
  testthat::expect_equal(as.numeric(unclass(b)),
    42.44814,
    tolerance = tol, scale = 1L,
    label = "default bayes t-test (noncentral_t likelihood)"
  )


})
