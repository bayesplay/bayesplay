test_that("family attribute for object", {
  # first we'll test the priors
  p0_mod <- prior(family = "normal", mean = 0L, sd = 1L)
  p1_mod <- prior(family = "student_t", mean = 0L, sd = 1L, df = 10L)
  p2_mod <- prior(family = "cauchy", location = 0L, scale = 1L)
  p3_mod <- prior(family = "beta", alpha = 1L, beta = 1L)
  p4_mod <- prior(family = "uniform", min = 0L, max = 1L)
  p5_mod <- prior(family = "point", point = 0L)

  testthat::expect_identical(
    p0_mod[["family"]],
    "normal",
    label = "normal prior"
  )

  testthat::expect_identical(
    p1_mod[["family"]],
    "student_t",
    label = "student_t prior"
  )

  testthat::expect_identical(
    p2_mod[["family"]],
    "cauchy",
    label = "cauchy prior"
  )

  testthat::expect_identical(
    p3_mod[["family"]],
    "beta",
    label = "beta prior"
  )

  testthat::expect_identical(
    p4_mod[["family"]],
    "uniform",
    label = "uniform prior"
  )

  testthat::expect_identical(
    p5_mod[["family"]],
    "point",
    label = "point prior"
  )


  # next we'll test the likelihoods

  l0_mod <- likelihood(family = "normal", mean = 2L, sd = 1L)
  l1_mod <- likelihood(family = "student_t", mean = 2L, sd = 1L, df = 16L)
  l2_mod <- likelihood(family = "noncentral_d", d = 0.7, n = 12L)
  l3_mod <- likelihood(family = "noncentral_d2", d = 0.7, n1 = 12L, n2 = 13L)
  l4_mod <- likelihood(family = "noncentral_t", t = 2.5, df = 17L)
  l5_mod <- likelihood(family = "binomial", successes = 4L, trials = 10L)

  testthat::expect_identical(
    l0_mod[["family"]],
    "normal",
    label = "normal likelihood"
  )

  testthat::expect_identical(
    l1_mod[["family"]],
    "student_t",
    label = "student_t likelihood"
  )

  testthat::expect_identical(
    l2_mod[["family"]],
    "noncentral_d",
    label = "noncentral_d prior"
  )

  testthat::expect_identical(
    l3_mod[["family"]],
    "noncentral_d2",
    label = "noncentral_d2 prior"
  )

  testthat::expect_identical(
    l4_mod[["family"]],
    "noncentral_t",
    label = "noncentral_t prior"
  )

  testthat::expect_identical(
    l5_mod[["family"]],
    "binomial",
    label = "binomial prior"
  )
})
