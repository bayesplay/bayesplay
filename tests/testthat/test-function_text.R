context("Function text attribute")
test_that("function_text slot for prior", {
  p0_mod <- prior(family = "normal", mean = 0, sd = 1)
  p1_mod <- prior(family = "student_t", mean = 0, sd = 1, df = 10)
  p2_mod <- prior(family = "cauchy", location = 0, scale = 1)
  p3_mod <- prior(family = "beta", alpha = 1, beta = 1)
  p4_mod <- prior(family = "uniform", min = 0, max = 1)
  p5_mod <- prior(family = "point", point = 0)


  testthat::expect_identical(
    p0_mod@function_text,
    "prior(\"normal\", mean = 0, sd = 1)",
    label = "normal prior"
  )

  testthat::expect_identical(
    p1_mod@function_text,
    "prior(\"student_t\", mean = 0, sd = 1, df = 10)",
    label = "student_t prior"
  )

  testthat::expect_identical(
    p2_mod@function_text,
    "prior(\"cauchy\", location = 0, scale = 1)",
    label = "cauchy prior"
  )

  testthat::expect_identical(
    p3_mod@function_text,
    "prior(\"beta\", alpha = 1, beta = 1)",
    label = "beta prior"
  )

  testthat::expect_identical(
    p4_mod@function_text,
    "prior(\"uniform\", min = 0, max = 1)",
    label = "uniform prior"
  )

  testthat::expect_identical(
    p5_mod@function_text,
    "prior(\"point\", point = 0)",
    label = "point prior"
  )
})
