tol <- 0.005

strip_bf <- function(x) {
  as.numeric(unclass(x))
}

bayes_factor <- function(likelihood_obj, alt_prior, null_prior, quiet = FALSE) {
  if (quiet) {
    return(suppressWarnings(
      integral(likelihood_obj * alt_prior) / integral(likelihood_obj * null_prior)
    ))
  }

  integral(likelihood_obj * alt_prior) / integral(likelihood_obj * null_prior)
}

expect_bf_equal <- function(actual, expected, label, tolerance = tol) {
  testthat::expect_equal(
    strip_bf(actual),
    strip_bf(expected),
    label = label,
    tolerance = tolerance
  )
}
