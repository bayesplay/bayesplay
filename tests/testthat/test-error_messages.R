context("Error messages")
test_that("error messages", {
  expect_error(
    prior("uniform", 0.0),
    "You must specify `min` and `max` for a uniform  prior"
  )

  expect_error(
    prior("normal", 0.0),
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

  l <- likelihood("normal", 0.0, 1.0)
  p <- prior("normal", 0.0, 0.1)

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
    prior("normal", 0.0, 0.0),
    "`sd` must be greater than 0"
  )

  expect_error(
    likelihood("t"),
    "t is not a valid distribution family"
  )

  expect_error(
    likelihood("normal", mean = 10.0),
    "You must specify a `mean` and `sd` for a normal likelihood"
  )

  expect_error(
    likelihood("normal", sd = 10.0),
    "You must specify a `mean` and `sd` for a normal likelihood"
  )

  expect_error(
    likelihood("normal", mean = 10.0, sd = 0.0),
    "`sd` must be greater than 0"
  )

  expect_error(
    likelihood("student_t", 10.0),
    "You must specify a `mean`, `sd`, and `df` for a student t likelihood"
  )

  expect_error(
    likelihood("student_t", 0.0, 0.0, 0.0),
    "`sd` must be greater than 0"
  )

  expect_error(
    likelihood("student_t", 0.0, 1.0, 0.0),
    "`df` must be greater than 0"
  )

  expect_error(
    likelihood("noncentral_d", 10.0),
    "You must specify a `d` and `n` for a noncentral d likelihood"
  )

  expect_error(
    likelihood("noncentral_d", 10.0, 0.0),
    "`n` must be greater than zero"
  )

  expect_error(
    likelihood("noncentral_t", 10.0, 0.0),
    "`df` must be greater than 0"
  )

  expect_error(
    likelihood("noncentral_t", 10.0),
    "You must specify a `t` and `df` for a noncentral t likelihood"
  )

  expect_error(
    likelihood("binomial"),
    "You must specify `successes` and `trials` for a binomial likelihood"
  )

  expect_error(
    likelihood("binomial", 10.0, 9.0),
    "`trials` must be greater than or equal to `successes`"
  )

  expect_error(
    likelihood("binomial", 10.0, 0.0),
    "`trials` must be greater than or equal to 1"
  )

  expect_error(
    likelihood("binomial", -1.0, 1.0),
    "`successes` must be greater than or equal to 0"
  )


  expect_error(
    likelihood("noncentral_d2", 10.0),
    "You must specify `d`, `n1`, and `n2` for a noncentral d2 likelihood"
  )

  expect_error(
    likelihood("noncentral_d2", 10.0, 1.0, 0.0),
    "`n1` and `n2` must be greater than or equal to 1"
  )


  expect_error(
    integral(prior("normal", 0.0, 1.0)),
    "obj must be of class product"
  )

  expect_error(
    suppressWarnings(
    plot(likelihood("noncentral_d", 20.0, 20L) * prior("cauchy", 0L, 1L))
      ),
    "Marginal likelihood has been approximated; Can't reliably output a plot."
  )

  expect_error(
    suppressWarnings(
    extract_posterior(likelihood("noncentral_d", 20.0, 20L) *
        prior("cauchy", 0L, 1L))
      ),
    paste0("Marginal likelihood has been approximated; ",
    "Can't reliably output a posterior function")
  )

  # TODO: Move these to a someting called approximation helper
  mod1 <- suppressWarnings(
      likelihood("noncentral_d", 20.0, 20L) * prior("cauchy", 0L, 1L)
  )
  mod2 <- suppressWarnings(
      likelihood("noncentral_d", 20.0, 20L) * prior("cauchy", 1L, 2L)
  )

  e1 <- integral(mod1)
  e2 <- integral(mod2)

  e4 <- integral(likelihood("noncentral_d", 20.0, 20L) * prior("point", 0L))
  e5 <- integral(likelihood("noncentral_d", 20.0, 20L) * prior("point", 1L))

  expect_false(is_point(e1, 0L))
  expect_true(is_point(e4, 0L))
  expect_false(is_point(e5, 0L))


  expect_error(
    e1 / e2,
    paste0("Marginal likelihood is a approximation. ",
      "One prior must be a point prior at 0")
  )



  expect_error(
    suppressWarnings(
      likelihood("noncentral_d", 20.0, 20L) * prior("uniform", 1L, 2L)
    ),
    "Approximations are only supported with cauchy priors"
  )

  expect_error(
    sd_ratio(mod2, 1L),
    "point must be 0 if the marginal likelihood is an approximation"
  )

  expect_error(
    suppressWarnings(
      likelihood("noncentral_t", 20.0, 20L) * prior("uniform", 1L, 2L)
    ),
    "t value is large; approximation needed
     Reparameterize using a `noncentral_d` of `noncentral_d2` likelihood."
  )


  bf <- integral(likelihood("binomial", 2L, 10L) * prior("uniform", 0L, 1L)) /
      integral(likelihood("binomial", 2L, 10L) * prior("point", 0.5))

  expect_output(
    show(summary(bf)),
    paste0(r"(Bayes factor\n)",
      r"( Using the levels from Wagenmakers et al \(2017\)\n)", #nolint
      r"( A BF of 2\.0687 indicates:\n)",
      r"( Anecdotal evidence)")
  )


})
