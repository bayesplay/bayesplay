test_that("makes_values_list creates a parameter grid", {
  parameters <- list(mean = c(-2L, 2L), sd = c(0L, 2L))

  values_df <- makes_values_list(parameters, steps = 4L)

  expect_named(values_df, c("mean", "sd"))
  expect_identical(nrow(values_df), 16L)
  expect_true(all(values_df[["sd"]] > 0L))
  expect_identical(sort(unique(values_df[["mean"]])), c(-2L, -1L, 0L, 1L))
  expect_equal(sort(unique(values_df[["sd"]])), c(0.5, 1.0, 1.5, 2.0))
})

test_that("make_bf_rr_func rejects invalid prior parameters", {
  data_model <- likelihood("normal", mean = 1L, sd = 5L)
  alternative_prior <- prior("normal", mean = 0L, sd = 1L)
  null_prior <- prior("point", point = 0L)

  expect_error(
    make_bf_rr_func(
      data_model,
      alternative_prior,
      null_prior,
      parameters = list(location = c(-1L, 1L))
    ),
    "location is not a valid parameter for a normal prior"
  )
})

test_that("bfrr computes expected Bayes factors on a small grid", {
  data_model <- likelihood("normal", mean = 1L, sd = 5L)
  alternative_prior <- prior("normal", mean = 0L, sd = 1L)
  null_prior <- prior("point", point = 0L)

  rr <- bfrr(
    likelihood = data_model,
    alternative_prior = alternative_prior,
    null_prior = null_prior,
    parameters = list(mean = c(-1L, 1L), sd = c(0L, 2L)),
    steps = 2L,
    multicore = FALSE
  )

  expect_s4_class(rr, "robustness")
  expect_named(
    rr[["data"]],
    c("mean", "sd", "bf", "range", "support"),
    ignore.order = TRUE
  )

  original_row <- rr[["data"]][
    rr[["data"]][["mean"]] == 0L & rr[["data"]][["sd"]] == 1L,
  ]
  expected_bf <- integral(data_model * alternative_prior) /
    integral(data_model * null_prior)

  expect_equal(
    as.numeric(unclass(original_row[["bf"]])),
    as.numeric(unclass(expected_bf)),
    tolerance = 1e-12
  )
})

test_that("bfrr reports original values outside varied range", {
  data_model <- likelihood("normal", mean = 1L, sd = 5L)
  alternative_prior <- prior("normal", mean = 3L, sd = 1L)
  null_prior <- prior("point", point = 0L)

  expect_error(
    bfrr(
      likelihood = data_model,
      alternative_prior = alternative_prior,
      null_prior = null_prior,
      parameters = list(mean = c(-1L, 1L)),
      multicore = FALSE
    ),
    "greater than the maximum value"
  )
})
