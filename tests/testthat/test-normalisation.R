context("Normalising priors")
test_that("Normalising warnings", {

  expect_warning(
    prior(
      family = "normal",
      mean = -2L,
      sd = 0.05,
      range = c(0L, Inf)
    ), "mean"
  )


  expect_warning(
    prior(
      family = "student_t",
      mean = -100L,
      sd = 0.05,
      df = 100L,
      range = c(1000L, Inf)
    ), "mean"
  )

# })
#
# test_that("Results with normalising errors", {
  suppressWarnings({
    shifted_half_norm <- prior(
      family = "normal",
      mean = -2L,
      sd = 0.05,
      range = c(0L, Inf)
    )
  })

  data_model <- likelihood(
    family = "normal",
    mean = -0.09129544,
    sd = 0.1225285
  )

  null_prior <- prior("point", 0L)
  bf <- integral(data_model * shifted_half_norm) /
    integral(data_model * null_prior)
  expect_equal(unclass(bf), 0L, tolerance = 0L)


  suppressWarnings({
    shifted_half_t <- prior(
      family = "student_t",
      mean = -10L,
      sd = 0.05,
      df = 100L,
      range = c(1000L, Inf)
    )
  })

  data_model <- likelihood(
    family = "normal",
    mean = -0.09129544,
    sd = 0.1225285
  )


  null_prior <- prior("point", 0L)
  bf <- integral(data_model * shifted_half_norm) /
    integral(data_model * null_prior)
  expect_equal(unclass(bf), 0L, tolerance = 0L)
# })
#
# test_that("Normalisation method", {
  mean <- 1L
  sd <- 10L
  ll <- -2L
  ul <- 2L
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- prior(
    family = "normal",
    mean = mean,
    sd = sd
  )@func
  f <- \(x) dnorm(x = x, mean = mean, sd = sd)
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pnorm(mean = mean, sd = sd, ll, lower.tail = TRUE)) -
    (1L - pnorm(mean = mean, sd = sd, ul, lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-09)
  k3 <- range_area_normal(mean, sd, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-09)



  mean <- 0L
  sd <- 10L
  ll <- 0L
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
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pnorm(mean = mean, sd = sd, ll, lower.tail = TRUE)) -
    (1L - pnorm(mean = mean, sd = sd, ul, lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-9)

  k3 <- range_area_normal(mean, sd, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-9)



  mean <- 1L
  sd <- 10L
  ll <- 2L
  ul <- 3L
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- prior(
    family = "normal",
    mean = mean,
    sd = sd
  )@func
  f <- \(x) dnorm(x = x, mean = mean, sd = sd)
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pnorm(mean = mean, sd = sd, ll, lower.tail = TRUE)) -
    (1L - pnorm(mean = mean, sd = sd, ul, lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-09)

  k3 <- range_area_normal(mean, sd, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-09)


  mean <- 1L
  sd <- 10L
  ul <- -3L
  ll <- -2L
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- prior(
    family = "normal",
    mean = mean,
    sd = sd
  )@func
  f <- \(x) dnorm(x = x, mean = mean, sd = sd)
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pnorm(mean = mean, sd = sd, ll, lower.tail = TRUE)) -
    (1L - pnorm(mean = mean, sd = sd, ul, lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-09)

  k3 <- range_area_normal(mean, sd, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-09)


  mean <- 1L
  sd <- 10L
  ul <- -3L
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
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pnorm(mean = mean, sd = sd, ll, lower.tail = TRUE)) -
    (1L - pnorm(mean = mean, sd = sd, ul, lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-09)
  k3 <- range_area_normal(mean, sd, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-09)


  ## Student t

  mean <- 1L
  sd <- 10L
  ll <- -2L
  ul <- 2L
  df <- 49L
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) dt_scaled(x = x, mean = mean, sd = sd, df = df)
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pt_scaled(mean = mean, sd = sd, df = df, ll, lower.tail = TRUE)) -
    (1L - pt_scaled(mean = mean, sd = sd, df = df, ul, lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-09)

  k3 <- range_area_student(mean, sd, df, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-09)



  mean <- 0L
  sd <- 10L
  ll <- 0L
  ul <- Inf
  df <- 49L
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) dt_scaled(x = x, mean = mean, sd = sd, df = df)
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pt_scaled(mean = mean, sd = sd, df = df, ll, lower.tail = TRUE)) -
    (1L - pt_scaled(mean = mean, sd = sd, df = df, ul, lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-06)

  k3 <- range_area_student(mean, sd, df, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-06)



  mean <- 1L
  sd <- 10L
  ll <- 2L
  ul <- 3L
  df <- 49L
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) dt_scaled(x = x, mean = mean, sd = sd, df = df)
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pt_scaled(mean = mean, sd = sd, df = df, ll, lower.tail = TRUE)) -
    (1L - pt_scaled(mean = mean, sd = sd, df = df, ul, lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-09)
  k3 <- range_area_student(mean, sd, df, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-09)


  mean <- 1L
  sd <- 10L
  ul <- -3L
  ll <- -2L
  df <- 49L
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) dt_scaled(x = x, mean = mean, sd = sd, df = df)
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pt_scaled(mean = mean, sd = sd, df = df, ll, lower.tail = TRUE)) -
    (1L - pt_scaled(mean = mean, sd = sd, df = df, ul, lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-09)
  k3 <- range_area_student(mean, sd, df, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-09)



  mean <- 1L
  sd <- 10L
  ul <- -3L
  ll <- Inf
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  df <- 49L
  f <- \(x) dt_scaled(x = x, mean = mean, sd = sd, df = df)
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pt_scaled(mean = mean, sd = sd, df = df, ll, lower.tail = TRUE)) -
    (1L - pt_scaled(mean = mean, sd = sd, df = df, ul, lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-09)
  k3 <- range_area_student(mean, sd, df, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-09)

  # Cauchy distribution

  location <- 1L
  scale <- 10L
  ul <- -3L
  ll <- Inf
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) dcauchy(x = x, location = location, scale = scale)
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pcauchy(ll, location, scale, lower.tail = TRUE)) -
    (1L - pcauchy(ul, location, scale, lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-09)
  k3 <- range_area_cauchy(location, scale, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-09)


  location <- 0L
  scale <- 10L
  ll <- 0L
  ul <- Inf
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) dcauchy(x = x, location = location, scale = scale)
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pcauchy(ll, location, scale, lower.tail = TRUE)) -
    (1L - pcauchy(ul, location, scale, lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-09)
  k3 <- range_area_cauchy(location, scale, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-09)


  location <- 1L
  scale <- 10L
  ll <- 2L
  ul <- 3L
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) dcauchy(x = x, location = location, scale = scale)
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pcauchy(ll, location, scale, lower.tail = TRUE)) -
    (1L - pcauchy(ul, location, scale, lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-09)
  k3 <- range_area_cauchy(location, scale, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-09)


  location <- 1L
  scale <- 10L
  ul <- -3L
  ll <- -2L
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) dcauchy(x = x, location = location, scale = scale)
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pcauchy(ll, location,  scale, lower.tail = TRUE)) -
    (1L - pcauchy(ul, location, scale, lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-09)
  k3 <- range_area_cauchy(location, scale, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-09)



  location <- 1L
  scale <- 10L
  ul <- -3L
  ll <- Inf
  tmp <- c(ul, ll)
  ll <- min(tmp)
  ul <- max(tmp)
  f <- \(x) dcauchy(x = x, location = location, scale = scale)
  k1 <- stats::integrate(Vectorize(f), ll, ul)[["value"]]
  k2 <- (1L - pcauchy(ll, location, scale, lower.tail = TRUE)) -
    (1L - pcauchy(ul, location, scale,  lower.tail = TRUE))
  expect_equal(k1, k2, tolerance = 1e-09)
  k3 <- range_area_cauchy(location, scale, ll, ul)
  expect_equal(k1, k3, tolerance = 1e-09)
})
