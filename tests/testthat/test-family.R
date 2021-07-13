context("Family attribute")
test_that("family attribute for object", {

  # first we'll test the priors
  p0_mod <- prior(family = "normal", mean = 0, sd = 1)
  p1_mod <- prior(family = "student_t", mean = 0, sd = 1, df = 10)
  p2_mod <- prior(family = "cauchy", location = 0, scale = 1)
  p3_mod <- prior(family = "beta", alpha = 1, beta = 1)
  p4_mod <- prior(family = "uniform", min = 0, max = 1)
  p5_mod <- prior(family = "point", point = 0)

  testthat::expect_identical(
    p0_mod$family,
    "normal",
    label = "normal prior"
  )

  testthat::expect_identical(
    p1_mod$family,
    "student_t",
    label = "student_t prior"
  )

  testthat::expect_identical(
    p2_mod$family,
    "cauchy",
    label = "cauchy prior"
  )

  testthat::expect_identical(
    p3_mod$family,
    "beta",
    label = "beta prior"
  )

  testthat::expect_identical(
    p4_mod$family,
    "uniform",
    label = "uniform prior"
  )

  testthat::expect_identical(
    p5_mod$family,
    "point",
    label = "point prior"
  )


  # next we'll test the likelihoods

  l0_mod <- likelihood(family = "normal", mean = 2, sd = 1)
  l1_mod <- likelihood(family = "student_t", mean = 2, sd = 1, df = 16)
  l2_mod <- likelihood(family = "noncentral_d", d = .7, n = 12)
  l3_mod <- likelihood(family = "noncentral_d2", d = .7, n1 = 12, n2 = 13)
  l4_mod <- likelihood(family = "noncentral_t", t = 2.5, df = 17)
  l5_mod <- likelihood(family = "binomial", successes = 4, trials = 10)

  testthat::expect_identical(
    l0_mod$family,
    "normal",
    label = "normal likelihood"
  )

  testthat::expect_identical(
    l1_mod$family,
    "student_t",
    label = "student_t likelihood"
  )

  testthat::expect_identical(
    l2_mod$family,
    "noncentral_d",
    label = "noncentral_d prior"
  )

  testthat::expect_identical(
    l3_mod$family,
    "noncentral_d2",
    label = "noncentral_d2 prior"
  )

  testthat::expect_identical(
    l4_mod$family,
    "noncentral_t",
    label = "noncentral_t prior"
  )

  testthat::expect_identical(
    l5_mod$family,
    "binomial",
    label = "binomial prior"
  )
})
