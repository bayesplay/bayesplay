context("Posterior families")
test_that("posterior", {

  # skip("skip")
  likelihood_obj <- likelihood(family = "noncentral_d", 0.7, n = 15)
  prior_obj <- prior(family = "cauchy", 0, 10)
  posterior_obj <- likelihood_obj * prior_obj

  h1 <- integral(likelihood_obj * prior_obj)
  h0 <- integral(likelihood_obj * prior("point", 0))
  m1 <- likelihood_obj * prior_obj
  bf <- h1 / h0
  sd <- prior_obj$prior_function(0) / posterior_obj$posterior_function(0)
  expect_equal(unclass(bf),
    unclass(sd),
    label = "BF does match Savage-Dicky Ratio"
  )

  expect_equal(bf,
    sd_ratio(m1, 0),
    label = "BF does match Savage-Dicky Ratio (2)"
  )



  testthat::expect_equal(unclass(h1),
    unclass(posterior_obj$prediction_function(0.7)),
    label = "Prediction function"
  )

  # test with a half cauchy prior
  likelihood_obj <- likelihood(family = "noncentral_d", 0.7, n = 15)
  prior_obj <- prior(family = "cauchy", 0, 10, range = c(0, Inf))
  posterior_obj <- likelihood_obj * prior_obj

  h1 <- integral(likelihood_obj * prior_obj)

  testthat::expect_equal(unclass(h1),
    unclass(posterior_obj$prediction_function(0.7)),
    label = "Prediction function"
  )



  likelihood_obj <- likelihood(family = "noncentral_d", 0.7, n = 15)
  prior_obj <- prior(family = "cauchy", 0, 10, range = c(0, Inf))
  posterior_obj_half <- likelihood_obj * prior_obj


  likelihood_obj <- likelihood(family = "noncentral_d", 0.7, n = 15)
  prior_obj <- prior(family = "cauchy", 0, 10, range = c(-Inf, Inf))
  posterior_obj_full <- likelihood_obj * prior_obj

  full <- posterior_obj_full$prediction_function(0.7)

  half <- posterior_obj_half$prediction_function(0.7)

  testthat::expect_gt(unclass(half),
    unclass(full),
    label = "Prediction function"
  )
})
