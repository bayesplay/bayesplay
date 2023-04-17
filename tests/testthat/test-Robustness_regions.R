test_that("make bf func", {

  mean_diff <- 0.3526918
  tvalue <- 2.9009
  se <- mean_diff / tvalue
  std_dev <- se * sqrt(50L)
  simdat <- mean_diff + as.numeric(scale(rnorm(50L, 0L, 1L))) * std_dev

  data_model <- likelihood("student_t", mean(simdat),
    sd = sd(simdat) / sqrt(length(simdat)), df = length(simdat) - 1L
  )

  alternative_prior <- prior("normal", mean = 0L, sd = 0.25, range = c(0L, Inf))
  null_prior <- prior("point", 0L)

  parameters <- list(mean = c(-2L, 2L), sd = c(0L, 2L))
  bf_func <- make_bf_rr_func(
    data_model,
    alternative_prior,
    null_prior,
    parameters
  )


  bf_base1 <- integral(data_model * alternative_prior) /
    integral(data_model * null_prior)

  expect_identical(
    bf_base1,
    bf_func(list(mean = 0L, sd = 0.25))
  )

  precision <- 0.05

  values_df <- makes_values_list(parameters, precision)
  values_list <- unname(split(values_df, ~ row.names(values_df)))
  bfs <- suppressWarnings(map(values_list, \(x) 
    cbind(x, data.frame(bf = bf_func(x)))))

  expect_identical(
    values_list[[200L]][["mean"]],
    bfs[[200L]][["mean"]]
  )


  expect_identical(
    values_list[[200L]][["sd"]],
    bfs[[200L]][["sd"]]
  )


  expect_identical(
    bfs[[200L]][c("mean", "sd")] |> bf_func(),
    bfs[[200L]][["bf"]]
  )

})



test_that("Robustness regions", {
 
  strip <- function(x) {
    as.numeric(unclass(x))
  }


  mean_diff <- 0.3526918
  tvalue <- 2.9009
  se <- mean_diff / tvalue
  std_dev <- se * sqrt(50L)
  simdat <- mean_diff + as.numeric(scale(rnorm(50L, 0L, 1L))) * std_dev

  data_model <- likelihood("student_t", mean(simdat),
    sd = sd(simdat) / sqrt(length(simdat)), df = length(simdat) - 1L
  )

  alternative_prior <- prior("normal", mean = 0L, sd = 0.25, range = c(0L, Inf))
  null_prior <- prior("point", 0L)
  parameters <- list(mean = c(-2L, 2L), sd = c(0L, 2L))
  precision <- 0.05

  bf_base1 <- integral(data_model * alternative_prior) /
    integral(data_model * null_prior)

  suppressWarnings({
    rr <- bfrr(
      data_model = data_model,
      alternative_prior = alternative_prior,
      null_prior = null_prior,
      parameters = parameters,
      precision = precision,
      cutoff = 6L,
      multicore = FALSE
    )
  })

  item <- rr[["data"]][200L, ]
  prior_mean <- item[["mean"]]
  prior_sd <- item[["sd"]]
  prior_range <- item[["range"]][[1L]] |> unlist()


  data_model <- likelihood("student_t", mean(simdat),
    sd = sd(simdat) / sqrt(length(simdat)), df = length(simdat) - 1L
  )

  alternative_prior <- prior("normal",
    mean = prior_mean,
    sd = prior_sd, range = c(0L, Inf)
    # sd = prior_sd, range = prior_range
  )

  null_prior <- prior("point", 0L)

  bf_value <- integral(data_model * alternative_prior) /
    integral(data_model * null_prior)

  testthat::expect_equal(
    strip(item[["bf"]]),
    strip(bf_value),
    tolerance = 0L
  )




  testthat::expect_equal(
    strip(bf_base1),
    strip(Filter(f = \(x) x[["mean"]] == 0L && x[["sd"]] == 0.25,
    unname(split(rr[["data"]], ~row.names(rr[["data"]]))))[[1L]][["bf"]]),
    tolerance = 0L
  )

  # single threaded
  mean_diff <- 0.3526918
  tvalue <- 2.9009
  se <- mean_diff / tvalue
  sd <- se * sqrt(50L)
  simdat <- mean_diff + as.numeric(scale(rnorm(50L, 0L, 1L))) * sd

  data_model <- likelihood("student_t", mean(simdat),
    sd = sd(simdat) / sqrt(length(simdat)), df = length(simdat) - 1L
  )
  alternative_prior <- prior("normal", mean = 0L, sd = 0.25, range = c(0L, Inf))
  null_prior <- prior("point", 0L)
  parameters <- list(mean = c(-2L, 2L), sd = c(0L, 2L))
  precision <- 0.05

  bf_base1 <- integral(data_model * alternative_prior) /
    integral(data_model * null_prior)

  suppressWarnings({
    rr <- bfrr(
      data_model = data_model,
      alternative_prior = alternative_prior,
      null_prior = null_prior,
      parameters = parameters,
      precision = precision,
      cutoff = 6L,
      multicore = FALSE
    )
  })

  item <- rr[["data"]][100L, ]
  prior_mean <- item[["mean"]]
  prior_sd <- item[["sd"]]
  prior_range <- item[["range"]] |> unlist()


  data_model <- likelihood("student_t", mean(simdat),
    sd = sd(simdat) / sqrt(length(simdat)), df = length(simdat) - 1L
  )
  alternative_prior <- prior("normal",
    mean = prior_mean,
    sd = prior_sd, range = prior_range
  )
  null_prior <- prior("point", 0L)

  bf_value <- integral(data_model * alternative_prior) /
    integral(data_model * null_prior)

  testthat::expect_equal(
    strip(item[["bf"]]),
    strip(bf_value),
    tolerance = 0L
  )

  testthat::expect_equal(
    strip(bf_base1),
    strip(Filter(f = \(x) x[["mean"]] == 0L && x[["sd"]] == 0.25,
      unname(split(rr[["data"]], ~row.names(rr[["data"]]))))[[1L]][["bf"]]),
    tolerance = 0L
  )

})
