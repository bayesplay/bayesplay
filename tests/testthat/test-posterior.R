context("Posterior families")
test_that("posterior", {

  likelihood_obj <- likelihood(family = "noncentral_d", 0.7, n = 15L)
  prior_obj <- prior(family = "cauchy", 0L, 10L)
  posterior_obj <- likelihood_obj * prior_obj

  h1 <- integral(likelihood_obj * prior_obj)
  h0 <- integral(likelihood_obj * prior("point", 0L))
  m1 <- likelihood_obj * prior_obj
  bf <- h1 / h0
  sd <- prior_obj[["prior_function"]](0L) /
    posterior_obj[["posterior_function"]](0L)
  expect_equal(as.numeric(unclass(bf)),
    unclass(sd),
    label = "BF does match Savage-Dicky Ratio"
  )

  expect_equal(as.numeric(bf),
    as.numeric(sd_ratio(m1, 0L)),
    label = "BF does match Savage-Dicky Ratio (2)"
  )



  testthat::expect_equal(as.numeric(unclass(h1)),
    unclass(posterior_obj[["prediction_function"]](0.7)),
    label = "Prediction function"
  )

  # test with a half cauchy prior
  likelihood_obj <- likelihood(family = "noncentral_d", 0.7, n = 15L)
  prior_obj <- prior(family = "cauchy", 0L, 10L, range = c(0L, Inf))
  posterior_obj <- likelihood_obj * prior_obj

  h1 <- integral(likelihood_obj * prior_obj)

  testthat::expect_equal(as.numeric(unclass(h1)),
    unclass(posterior_obj[["prediction_function"]](0.7)),
    label = "Prediction function"
  )



  likelihood_obj <- likelihood(family = "noncentral_d", 0.7, n = 15L)
  prior_obj <- prior(family = "cauchy", 0L, 10L, range = c(0L, Inf))
  posterior_obj_half <- likelihood_obj * prior_obj


  likelihood_obj <- likelihood(family = "noncentral_d", 0.7, n = 15L)
  prior_obj <- prior(family = "cauchy", 0L, 10L, range = c(-Inf, Inf))
  posterior_obj_full <- likelihood_obj * prior_obj

  full <- posterior_obj_full[["prediction_function"]](0.7)

  half <- posterior_obj_half[["prediction_function"]](0.7)

  testthat::expect_gt(unclass(half),
    unclass(full),
    label = "Prediction function"
  )
})
