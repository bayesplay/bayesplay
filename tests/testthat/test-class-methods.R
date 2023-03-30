test_that("class methods", {
  expect_named(
    bayesplay::likelihood("normal", 0L, 1L),
    c("family", "parameters", "likelihood_function"),
    label = "likelihood function"
  )

  expect_named(
    bayesplay::prior("normal", 0.0, 1.0),
    c("family", "parameters", "prior_function"),
    label = "prior function"
  )

  expect_named(
    likelihood("normal", 0.0, 1.0) * prior("normal", 0.0, 1.0),
    c(
      "integral",
      "marginal_function",
      "evidence_function",
      "posterior_function",
      "conditional_function",
      "weighted_likelihood_function",
      "prediction_function"
      # "approximation"
    )
  )

  expect_equivalent(
    likelihood("normal", 0.0, 1.0) * prior("normal", 0.0, 1.0),
    prior("normal", 0.0, 1.0) * likelihood("normal", 0.0, 1.0),
    "multiplication commutes"
  )

  expect_equivalent(
    prior("cauchy", 0.0, 1.0, c(-Inf, Inf)),
    make_prior(new("cauchy"), 0.0, 1.0, c(-Inf, Inf)),
    "contructor works"
  )

  expect_equivalent(
    prior("cauchy", 0.0, 1.0, c(-Inf, Inf))[["family"]],
    "cauchy",
    "accesing with [[]] works"
  )


  expect_equivalent(
    prior("cauchy", 0.0, 1.0, c(-Inf, Inf))$family, # nolint
    "cauchy",
    "accesing with $ works"
  )




})
