# convert a t value and df to a d value and n


# convert a t value and df to a d value and n1 and n2
t_to_d2 <- function(t, df) {
  total_n <- df + 2L
  n1 <- floor(total_n / 2L)
  n2 <- total_n - n1
  n <- n1 * n2 / (n1 + n2)
  d <- t / sqrt(n)
  list(d = d, n1 = n1, n2 = n2)
}


## FIXME: This needs approximations for one-sample tests too! # nolint
test_that("Approximation works", {
  # note, that approximations are invoked when the t-value is very large
  # or when the prior is truncated and the t-value is in the truncated region

  t_value <- 4L
  df <- 10L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L)
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 16.8045514199822
  testthat::expect_equal(bp, bf, tolerance = 1e-6)


  t_value <- 5L
  df <- 10L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L)
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 57.9851200108064
  testthat::expect_equal(bp, bf, tolerance = 1e-6)


  t_value <- 7L
  df <- 10L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L)
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 538.691408434198
  testthat::expect_equal(bp, bf, tolerance = 1e-6)


  t_value <- 12L
  df <- 23L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L)
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 302479507.020976
  testthat::expect_equal(bp, bf, tolerance = 1e-6)


  # approximations are invokved at this point

  t_value <- 15.1
  df <- 23L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L)
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 25655644599.9737
  testthat::expect_equal(bp, bf, tolerance = 1e-6)



  t_value <- 16L
  df <- 23L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L)
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 81407502464.5156
  testthat::expect_equal(bp, bf, tolerance = 1e-6)


  t_value <- 20L
  df <- 23L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L)
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 7727336038836.84
  testthat::expect_equal(bp, bf, tolerance = 1e-6)



  # And now with truncated priors

  t_value <- 4L
  df <- 23L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L, c(-Inf, 0L))
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 0.0748129841998264
  testthat::expect_equal(bp, bf, tolerance = 1e-6)



  t_value <- 5L
  df <- 23L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L, c(-Inf, 0L))
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 0.0677364571619135
  testthat::expect_equal(bp, bf, tolerance = 1e-6)

  ## approximation is invoked at this point



  t_value <- 5.1
  df <- 23L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L, c(-Inf, 0L))
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 0.0216306847663303
  testthat::expect_equal(bp, bf, tolerance = 1e-6)


  t_value <- 15L
  df <- 23L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L, c(-Inf, 0L))
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 0.00514506391415391
  testthat::expect_equal(bp, bf, tolerance = 1e-6)


  t_value <- 35L
  df <- 40L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L, c(-Inf, 0L))
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 0.00154498566424772
  testthat::expect_equal(bp, bf, tolerance = 1e-5)


  t_value <- 17.8
  df <- 24L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L, c(-Inf, 0L))
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 0.00415484006186442
  testthat::expect_equal(bp, bf, tolerance = 1e-6)


  # and now truncate the prior on the other side

  t_value <- -4L
  df <- 24L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L, c(0L, Inf))
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 0.0728303285761128
  testthat::expect_equal(bp, bf, tolerance = 1e-6)


  t_value <- -5L
  df <- 45L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L, c(0L, Inf))
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 0.0436725327106835
  testthat::expect_equal(bp, bf, tolerance = 1e-6)


  # approximations are invokved at this point
  t_value <- -5.5
  df <- 45L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L, c(0L, Inf))
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 0.0160479382778689
  testthat::expect_equal(bp, bf, tolerance = 1e-6)


  t_value <- -7L
  df <- 110L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L, c(0L, Inf))
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 0.00903123730304949
  testthat::expect_equal(bp, bf, tolerance = 1e-6)


  t_value <- -90L
  df <- 110L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 1L, c(0L, Inf))
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 0.00034618370689802
  testthat::expect_equal(bp, bf, tolerance = 1e-6)




  t_value <- -90L
  df <- 110L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 0.707, c(0L, Inf))
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 0.000245186701573274
  testthat::expect_equal(bp, bf, tolerance = 1e-6)



  t_value <- -94L
  df <- 20L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 0.707, c(0L, Inf))
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 0.000588357441164805
  testthat::expect_equal(bp, bf, tolerance = 1e-5)


  t_value <- -94L
  df <- 20L
  bp <- t_to_d2(t_value, df) |>
    (function(x) {
      suppressWarnings(
        likelihood("noncentral_d2", x[["d"]], x[["n1"]], x[["n2"]]) *
          prior("cauchy", 0L, 0.707, c(0L, Inf))
      )
    })() |>
    sd_ratio(point = 0L) |>
    as.numeric()
  bf <- 0.000588357441164805
  testthat::expect_equal(bp, bf, tolerance = 1e-5)
})
