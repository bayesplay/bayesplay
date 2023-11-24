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

  orginal_values <- alternative_prior |>
    slot("parameters")
  values_df <- makes_values_list(parameters, precision, orginal_values)
  values_list <- unname(split(values_df, ~ row.names(values_df)))
  bfs <- suppressWarnings(map(values_list, function(x) {
    cbind(x, data.frame(bf = bf_func(x)))
  }))

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

test_that("make values list", {
  mean_values <- c(-2L, -1L, 0L, 1L, 2L)
  sd_values <- c(0L, 0.25, 1L, 2L)

  expect_df_s <- expand.grid(list(mean = mean_values, sd = sd_values)) |>
    filter(sd > 0L)

  expect_df_s <- expect_df_s[
    order(expect_df_s[["mean"]], expect_df_s[["sd"]]),
  ]

  row.names(expect_df_s) <- NULL


  data_model <- likelihood("student_t", 0.3526918, sd = 0.1215801, df = 49L)
  alternative_prior <- prior("normal", mean = 0L, sd = 0.25, range = c(0L, Inf))
  null_prior <- prior("point", 0L)
  parameters <- list(mean = c(-2L, 2L), sd = c(0L, 2L))
  precision <- 1L
  orginal_values <- alternative_prior |>
    slot("parameters")
  values_df <- makes_values_list(parameters, precision, orginal_values)


  # strip the attributes
  attr(values_df, "out.attrs") <- NULL # nolint
  attr(expect_df_s, "out.attrs") <- NULL # nolint

  values_df <- sorter(values_df)
  expect_df_s <- sorter(expect_df_s)


  expect_identical(
    expect_df_s,
    values_df
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
      likelihood = data_model,
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
    strip(rr[["data"]] |>
            filter(mean == 0L, sd == 0.25) |>
            pull(bf)),
    tolerance = 0L
  )


  testthat::expect_equal(
    strip(bf_base1),
    strip(rr[["data"]] |>
            filter(mean == 0L, sd == 0.25) |>
            pull(bf)),
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
      likelihood = data_model,
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
    strip(rr[["data"]] |>
            filter(mean == 0L, sd == 0.25) |>
            pull(bf)),
    tolerance = 0L
  )
})

test_that("Robustness regions display", {
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
  precision <- 0.5
  bf_base1 <- integral(data_model * alternative_prior) /
    integral(data_model * null_prior)

  suppressWarnings({
    rr <- bfrr(
      likelihood = data_model,
      alternative_prior = alternative_prior,
      null_prior = null_prior,
      parameters = parameters,
      precision = precision,
      cutoff = 6L,
      multicore = FALSE
    )
  })

  testthat::expect_snapshot(
    print(rr)
  )


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
  parameters <- list(mean = c(-2L, 2L))
  precision <- 0.05
  bf_base1 <- integral(data_model * alternative_prior) /
    integral(data_model * null_prior)

  suppressWarnings({
    rr <- bfrr(
      likelihood = data_model,
      alternative_prior = alternative_prior,
      null_prior = null_prior,
      parameters = parameters,
      precision = precision,
      cutoff = 6L,
      multicore = FALSE
    )
  })



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
  parameters <- list(sd = c(0L, 4L))
  precision <- 0.05
  bf_base1 <- integral(data_model * alternative_prior) /
    integral(data_model * null_prior)

  suppressWarnings({
    rr <- bfrr(
      likelihood = data_model,
      alternative_prior = alternative_prior,
      null_prior = null_prior,
      parameters = parameters,
      precision = precision,
      cutoff = 6L,
      multicore = FALSE
    )
  })

  # nolint start
  #
  # # H1: Normal(NormalPrior { mean: 0.0, sd: 0.25, range: (Some(0.0), Some(inf)) })
  #
  # # H0: Point(PointPrior { point: 0.0 })
  #
  # # Likelihood: Normal(NormalLikelihood { mean: 0.3526918, sd: 0.121580130304388 })
  #   data_model <- likelihood("normal", mean = 0.3526918, sd = 0.121580130304388)
  #   h1 <- prior("normal", mean = 0L, sd = 0.25, c(-Inf, Inf))
  #   h0 <- prior("point", 0L)
  #   bf <- integral(data_model * h1) / integral(data_model * h0)
  #
  #   precision <- 0.05
  #   parameters <- list(mean = c(-2L, 2L), sd = c(0L, 2L))
  #   suppressWarnings({
  #     rr <- bfrr(
  #       data_model = data_model,
  #       alternative_prior = h1,
  #       null_prior = h0,
  #       parameters = parameters,
  #       precision = precision,
  #       cutoff = 6L,
  #       multicore = TRUE
  #     )
  #   })
  #
  #   bp <- rr[["data"]] |> dplyr::select(mean, sd, bf) |>
  #     tibble::as_tibble() |>
  #     dplyr::arrange(mean, sd) |>
  #     dplyr::mutate(bf = as.numeric(bf))
  #
  #
  #   rs <- readr::read_csv("~/Github/bayesplay-rs/robustness.csv") |>
  #     dplyr::select(mean, sd, bf) |>
  #     dplyr::filter(sd > 0L) |>
  #     dplyr::arrange(mean, sd)
  # #   testthat::expect_equal(bp, rs, tolerance = 1e-6)
  #
  # rs <- rs |> mutate(bf = log(bf)) |>
  #     dplyr::mutate(bounds = case_when(bf < log(1/6) ~ "H0", bf > log(6) ~ "H1", TRUE ~ "Inc"))
  # ggplot(rs, aes(x = mean, y = sd, fill = bounds, color = bounds)) + geom_point()
  #
  # nolint end
})
