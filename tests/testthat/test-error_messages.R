context("Error messages")
test_that("error messages", {
  expect_error(
    prior("uniform", 0),
    "You must specify `min` and `max` for a uniform  prior"
  )

  expect_error(
    prior("normal", 0),
    "You must specify `mean` and `sd` for a normal prior"
  )


  expect_warning(
    prior("point"),
    "Point value is missing. Assuming 0"
  )

  expect_error(
    prior("student_t"),
    "You must specify `mean`, `sd`, and `df` for a student_t prior"
  )


  expect_error(
    prior("beta"),
    "You must specify `alpha` and `beta` for a beta  prior"
  )

  l <- likelihood("normal", 0, 1)
  p <- prior("normal", 0, .1)

  expect_error(
    extract_predictions(l),
    "Object not of class product"
  )

  expect_error(
    extract_posterior(l),
    "Object not of class product"
  )

  expect_error(
    extract_predictions(p),
    "Object not of class product"
  )

  expect_error(
    extract_posterior(p),
    "Object not of class product"
  )

  expect_error(
    prior("t"),
    "t is not a valid distribution family"
  )

  expect_error(
    prior("normal", 0, 0),
    "`sd` must be greater than 0"
  )

  expect_error(
    likelihood("t"),
    "t is not a valid distribution family"
  )

  expect_error(
    likelihood("normal", mean = 10),
    "You must specify a `mean` and `sd` for a normal likelihood"
  )

  expect_error(
    likelihood("normal", sd = 10),
    "You must specify a `mean` and `sd` for a normal likelihood"
  )

  expect_error(
    likelihood("normal", mean = 10, sd = 0),
    "`sd` must be greater than 0"
  )

  expect_error(
    likelihood("student_t", 10),
    "You must specify a `mean`, `sd`, and `df` for a student t likelihood"
  )

  expect_error(
    likelihood("student_t", 0, 0, 0),
    "`sd` must be greater than 0"
  )

  expect_error(
    likelihood("student_t", 0, 1, 0),
    "`df` must be greater than 0"
  )

  expect_error(
    likelihood("noncentral_d", 10),
    "You must specify a `d` and `n` for a noncentral d likelihood"
  )

  expect_error(
    likelihood("noncentral_d", 10, 0),
    "`n` must be greater than zero"
  )

  expect_error(
    likelihood("noncentral_t", 10, 0),
    "`df` must be greater than 0"
  )

  expect_error(
    likelihood("noncentral_t", 10),
    "You must specify a `t` and `df` for a noncentral t likelihood"
  )

  expect_error(
    likelihood("binomial"),
    "You must specify `successes` and `trials` for a binomial likelihood"
  )

  expect_error(
    likelihood("binomial", 10, 9),
    "`trials` must be greater than or equal to `successes`"
  )

  expect_error(
    likelihood("binomial", 10, 0),
    "`trials` must be greater than or equal to 1"
  )

  expect_error(
    likelihood("binomial", -1, 1),
    "`successes` must be greater than or equal to 0"
  )


  expect_error(
    likelihood("noncentral_d2", 10),
    "You must specify `d`, `n1`, and `n2` for a noncentral d2 likelihood"
  )

  expect_error(
    likelihood("noncentral_d2", 10, 1, 0),
    "`n1` and `n2` must be greater than or equal to 1"
  )


  expect_error(
    integral(prior("normal", 0, 1)),
    "obj must be of class product"
  )
})
