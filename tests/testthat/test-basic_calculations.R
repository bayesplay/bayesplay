context("Basic calculations")

test_that("basic BF calculations", {
  tol <- 0.005

  data_model <- likelihood(family = "normal", mean = 5, sd = 10)
  h1_model <- prior(family = "uniform", 0, 20)
  h0_model <- prior(family = "point", point = 0)
  m1 <- (data_model * h1_model)
  m0 <- (data_model * h0_model)
  b <- m1 / m0

  testthat::expect_equal(unclass(b),
    unclass(0.89),
    tolerance = tol, scale = 1,
    label = "uniform prior"
  )

  data_model <- likelihood(family = "normal", mean = 5.5, sd = 32.35)
  h0_model <- prior(family = "point", point = 0)
  h1_model <- prior(
    family = "normal", mean = 0, sd = 13.3,
    range = c(0, Inf)
  )
  m1 <- integral(data_model * h1_model)
  m0 <- integral(data_model * h0_model)
  b <- m1 / m0

  testthat::expect_equal(unclass(b),
    unclass(0.97),
    tolerance = tol, scale = 1,
    label = "normal prior"
  )



  data_model <- likelihood(family = "normal", mean = 0.63, sd = 0.43)
  h1_model <- prior(
    family = "normal", mean = 0, sd = 2.69,
    range = c(0, Inf)
  )
  h0_model <- prior(family = "point", point = 0)
  m1 <- integral(data_model * h1_model)
  m0 <- integral(data_model * h0_model)
  b <- m1 / m0

  testthat::expect_equal(unclass(b),
    unclass(0.83),
    tolerance = tol, scale = 1,
    label = "half-normal prior"
  )


  data_model <- likelihood(family = "normal", mean = 15, sd = 13)
  h1_model <- prior(family = "normal", mean = 50, sd = 14)
  h0_model <- prior(family = "point", point = 0)
  m1 <- integral(data_model * h1_model)
  m0 <- integral(data_model * h0_model)
  b <- m1 / m0

  testthat::expect_equal(unclass(b),
    unclass(0.25),
    tolerance = tol, scale = 1,
    label = "normal prior"
  )


  data_model <- likelihood("student_t", mean = 5.47, sd = 32.2, df = 119)
  h1_model <- prior("student_t", mean = 13.3, sd = 4.93, df = 72)
  h0_model <- prior("point", 0)
  m1 <- integral(data_model * h1_model)
  m0 <- integral(data_model * h0_model)
  b <- m1 / m0

  testthat::expect_equal(unclass(b),
    unclass(0.97),
    tolerance = tol, scale = 1,
    label = "student_t prior (student_t likelihood)"
  )





  # default bayes t-test
  # BayesFactor will be temporarily installed for the initial tests
  # paired
  n <- 10
  m <- 0.8880938
  s <- 0.5952841
  d <- m / s
  t <- d * sqrt(n)
  df <- n - 1
  data_model <- likelihood("noncentral_t", t, df)
  prior_model <- prior("cauchy", 0, 1 * sqrt(df + 1))
  bf <- sd_ratio(data_model * prior_model, 0)
  bf_bayesfactor <- 42.44814

  testthat::expect_equal(bf_bayesfactor,
    unclass(bf),
    tolerance = tol, scale = 1
  )

  data_model <- likelihood(family = "noncentral_d", d, n = n)
  prior_model <- prior(family = "cauchy", 0, 1)
  bf <- sd_ratio(data_model * prior_model, 0)
  testthat::expect_equal(bf_bayesfactor,
    unclass(bf),
    tolerance = tol, scale = 1
  )


  # independent samples
  n1 <- 17
  n2 <- 18
  m1 <- 18.04028
  m2 <- 21.00756
  s1 <- 17.56803
  s2 <- 17.61612
  md_diff <- m1 - m2
  sd_pooled <- sqrt((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2))
  d <- md_diff / sd_pooled

  t <- d / sqrt(1 / n1 + 1 / n2)
  df <- n1 + n2 - 2

  bf_bayesfactor <- 0.274111
  data_model <- likelihood(family = "noncentral_t", t, df)
  prior_model <- prior(family = "cauchy", 0, 1 * sqrt((n1 * n2) / (n1 + n2)))
  bf <- bayesplay::sd_ratio(data_model * prior_model, 0)

  testthat::expect_equal(bf_bayesfactor,
    unclass(bf),
    tolerance = tol, scale = 1
  )


  data_model <- likelihood("noncentral_d2", d = d, n1 = n1, n2 = n2)
  prior_model <- prior("cauchy", 0, 1)
  bf <- bayesplay::sd_ratio(data_model * prior_model, 0)

  testthat::expect_equal(bf_bayesfactor,
    unclass(bf),
    tolerance = tol, scale = 1
  )

  t <- 2.03
  n <- 80
  d <- t / sqrt(n)
  data_model <- likelihood(family = "noncentral_d", d = d, n = n)

  h1_model <- prior("cauchy", scale = 1)
  h0_model <- prior("point", 0)
  m1 <- integral(data_model * h1_model)
  m0 <- integral(data_model * h0_model)

  b1 <- m1 / m0
  b2 <- 1 / 1.557447

  testthat::expect_equal(unclass(b1),
    unclass(unname(b2)),
    label = "default bayes t (orginal)",
    tolerance = tol, scale = 1
  )

  # now do it with a one-sided prior
  data_model <- likelihood(family = "noncentral_d", d = d, n = n)

  h1_model <- prior("cauchy", scale = 1, range = c(0, Inf))
  h0_model <- prior("point", 0)
  m1 <- integral(data_model * h1_model)
  m0 <- integral(data_model * h0_model)

  b1 <- m0 / m1
  b2 <- 0.79745

  testthat::expect_equal(unclass(b1),
    unclass(unname(b2)),
    label = "default bayes t (orginal) one-sided",
    tolerance = tol, scale = 1
  )





  data_model <- likelihood(
    family = "noncentral_t",
    t = t, df = n - 1
  )

  h1_model <- prior("cauchy", scale = 1 * sqrt(n))
  h0_model <- prior("point", 0)
  m1 <- integral(data_model * h1_model)
  m0 <- integral(data_model * h0_model)

  b1 <- m1 / m0
  b2 <- 1 / 1.557447
  testthat::expect_equal(unclass(b1),
    unclass(unname(b2)),
    label = "default bayes t (t version)",
    tolerance = tol, scale = 1
  )



  data_model <- likelihood(family = "binomial", 3, 12)
  alt_prior <- prior(family = "beta", alpha = 1, beta = 2)
  null_prior <- prior(family = "point", point = 0.5)
  m1 <- data_model * alt_prior
  m0 <- data_model * null_prior
  b1 <- integral(m1) / integral(m0)
  b2 <- 1 / 0.4887695
  testthat::expect_equal(unclass(b1),
    unclass(unname(b2)),
    label = "binomial likelihood, beta prior",
    tolerance = tol, scale = 1
  )


  data_model <- likelihood(family = "binomial", 3, 12)
  alt_prior <- prior(family = "uniform", min = 0, max = 1)
  null_prior <- prior(family = "point", point = 0.5)
  m1 <- data_model * alt_prior
  m0 <- data_model * null_prior
  b1 <- integral(m1) / integral(m0)
  b2 <- 1 / 0.6982422
  testthat::expect_equal(unclass(b1),
    unclass(unname(b2)),
    label = "binomial likelihood, uniform prior",
    tolerance = tol, scale = 1
  )


  data_model <- likelihood(family = "binomial", 3, trials = 12)
  alt_prior <- prior(family = "beta", 1, 1)
  null_prior <- prior(family = "point", point = 0.5)
  m1 <- data_model * alt_prior
  m0 <- data_model * null_prior
  b1 <- integral(m1) / integral(m0)
  b2 <- 1 / 0.6982422
  testthat::expect_equal(unclass(b1),
    unclass(unname(b2)),
    label = "binomial likelihood, beta uniform",
    tolerance = tol, scale = 1
  )


  data_model <- likelihood(family = "binomial", successes = 2, trials = 10)
  alt_prior <- prior(family = "beta", alpha = 1, beta = 2.5)
  null_prior <- prior(family = "point", point = 0.5)
  m1 <- data_model * alt_prior
  m0 <- data_model * null_prior
  b1 <- integral(m1) / integral(m0)
  b2 <- 3.3921325
  testthat::expect_equal(unclass(b1),
    unclass(unname(b2)),
    label = "binomial likelihood, beta prior",
    tolerance = tol, scale = 1
  )
})
