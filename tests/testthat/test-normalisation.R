context("Normalising priors")
test_that("Normalising warnings", {
  require(bayesplay)
  testthat::expect_warning(
    prior(
      family = "normal",
      mean = -2,
      sd = 0.05,
      range = c(0, Inf)
    ), "mean"
  )


  testthat::expect_warning(
    prior(
      family = "student_t",
      mean = -100,
      sd = 0.05,
      df = 100,
      range = c(1000, Inf)
    ), "mean"
  )

})

test_that("Results with normalising errors", {
  suppressWarnings({
    shifted_half_norm <- prior(
      family = "normal",
      mean = -2,
      sd = 0.05,
      range = c(0, Inf)
    )
  })

  data_model <- likelihood(
    family = "normal",
    mean = -0.09129544,
    sd = 0.1225285
  )

  null_prior <- prior("point", 0)
  bf <- integral(data_model * shifted_half_norm) /
    integral(data_model * null_prior)
  testthat::expect_equal(unclass(bf), 0)


  suppressWarnings({
    shifted_half_t <- prior(
      family = "student_t",
      mean = -10,
      sd = 0.05,
      df = 100,
      range = c(1000, Inf)
    )
  })
  data_model <- likelihood(
    family = "normal",
    mean = -0.09129544,
    sd = 0.1225285
  )


  null_prior <- prior("point", 0)
  bf <- integral(data_model * shifted_half_norm) /
    integral(data_model * null_prior)
  testthat::expect_equal(unclass(bf), 0)
})

test_that("Normalisation method", {
  require(bayesplay)
  mean <- 1
  sd <- 10
  ll <- -2
  ul <- 2
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- prior(
    family = "normal",
    mean = mean,
    sd = sd
  )@func
  f <- \(x) dnorm(x = x, mean = mean, sd = sd)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - pnorm(mean = mean, sd = sd, ll, lower.tail = TRUE)) -
    (1 - pnorm(mean = mean, sd = sd, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_normal(mean, sd, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)



  mean <- 0
  sd <- 10
  ll <- 0
  ul <- Inf
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- prior(
    family = "normal",
    mean = mean,
    sd = sd
  )@func
  f <- \(x) dnorm(x = x, mean = mean, sd = sd)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - pnorm(mean = mean, sd = sd, ll, lower.tail = TRUE)) -
    (1 - pnorm(mean = mean, sd = sd, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_normal(mean, sd, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)



  mean <- 1
  sd <- 10
  ll <- 2
  ul <- 3
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- prior(
    family = "normal",
    mean = mean,
    sd = sd
  )@func
  f <- \(x) dnorm(x = x, mean = mean, sd = sd)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - pnorm(mean = mean, sd = sd, ll, lower.tail = TRUE)) -
    (1 - pnorm(mean = mean, sd = sd, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_normal(mean, sd, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)


  mean <- 1
  sd <- 10
  ul <- -3
  ll <- -2
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- prior(
    family = "normal",
    mean = mean,
    sd = sd
  )@func
  f <- \(x) dnorm(x = x, mean = mean, sd = sd)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - pnorm(mean = mean, sd = sd, ll, lower.tail = TRUE)) -
    (1 - pnorm(mean = mean, sd = sd, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_normal(mean, sd, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)


  mean <- 1
  sd <- 10
  ul <- -3
  ll <- Inf
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- prior(
    family = "normal",
    mean = mean,
    sd = sd
  )@func
  f <- \(x) dnorm(x = x, mean = mean, sd = sd)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - pnorm(mean = mean, sd = sd, ll, lower.tail = TRUE)) -
    (1 - pnorm(mean = mean, sd = sd, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_normal(mean, sd, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)


  ## Student t

  mean <- 1
  sd <- 10
  ll <- -2
  ul <- 2
  df <- 49
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) bayesplay:::dt_scaled(x = x, mean = mean, sd = sd, df = df)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - bayesplay:::pt_scaled(mean = mean, sd = sd, df = df, ll, lower.tail = TRUE)) -
    (1 - bayesplay:::pt_scaled(mean = mean, sd = sd, df = df, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_student(mean, sd, df, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)



  mean <- 0
  sd <- 10
  ll <- 0
  ul <- Inf
  df <- 49
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) bayesplay:::dt_scaled(x = x, mean = mean, sd = sd, df = df)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - bayesplay:::pt_scaled(mean = mean, sd = sd, df = df, ll, lower.tail = TRUE)) -
    (1 - bayesplay:::pt_scaled(mean = mean, sd = sd, df = df, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_student(mean, sd, df, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)



  mean <- 1
  sd <- 10
  ll <- 2
  ul <- 3
  df <- 49
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) bayesplay:::dt_scaled(x = x, mean = mean, sd = sd, df = df)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - bayesplay:::pt_scaled(mean = mean, sd = sd, df = df, ll, lower.tail = TRUE)) -
    (1 - bayesplay:::pt_scaled(mean = mean, sd = sd, df = df, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_student(mean, sd, df, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)


  mean <- 1
  sd <- 10
  ul <- -3
  ll <- -2
  df <- 49
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) bayesplay:::dt_scaled(x = x, mean = mean, sd = sd, df = df)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - bayesplay:::pt_scaled(mean = mean, sd = sd, df = df, ll, lower.tail = TRUE)) -
    (1 - bayesplay:::pt_scaled(mean = mean, sd = sd, df = df, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_student(mean, sd, df, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)



  mean <- 1
  sd <- 10
  ul <- -3
  ll <- Inf
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  df <- 49
  f <- \(x) bayesplay:::dt_scaled(x = x, mean = mean, sd = sd, df = df)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - bayesplay:::pt_scaled(mean = mean, sd = sd, df = df, ll, lower.tail = TRUE)) -
    (1 - bayesplay:::pt_scaled(mean = mean, sd = sd, df = df, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_student(mean, sd, df, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)

  # Cauchy distribution

  location <- 1
  scale <- 10
  ul <- -3
  ll <- Inf
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) dcauchy(x = x, location = location, scale = scale)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - pcauchy(location = location, scale = scale, ll, lower.tail = TRUE)) -
    (1 - pcauchy(location = location, scale = scale, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_cauchy(location, scale, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)


  location <- 0
  scale <- 10
  ll <- 0
  ul <- Inf
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) dcauchy(x = x, location = location, scale = scale)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - pcauchy(location = location, scale = scale, ll, lower.tail = TRUE)) -
    (1 - pcauchy(location = location, scale = scale, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_cauchy(location, scale, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)


  location <- 1
  scale <- 10
  ll <- 2
  ul <- 3
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) dcauchy(x = x, location = location, scale = scale)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - pcauchy(location = location, scale = scale, ll, lower.tail = TRUE)) -
    (1 - pcauchy(location = location, scale = scale, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_cauchy(location, scale, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)


  location <- 1
  scale <- 10
  ul <- -3
  ll <- -2
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) dcauchy(x = x, location = location, scale = scale)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - pcauchy(location = location, scale = scale, ll, lower.tail = TRUE)) -
    (1 - pcauchy(location = location, scale = scale, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_cauchy(location, scale, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)



  location <- 1
  scale <- 10
  ul <- -3
  ll <- Inf
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) dcauchy(x = x, location = location, scale = scale)
  k1 <- stats::integrate(Vectorize(f), ll, ul)$value
  k2 <- (1 - pcauchy(location = location, scale = scale, ll, lower.tail = TRUE)) -
    (1 - pcauchy(location = location, scale = scale, ul, lower.tail = TRUE))
  testthat::expect_equal(k1, k2, tolerance = 0.001)
  k3 <- bayesplay:::range_area_cauchy(location, scale, ll, ul)
  testthat::expect_equal(k1, k3, tolerance = 0.001)
})
