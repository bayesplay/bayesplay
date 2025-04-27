testthat::skip(message = "Skipping during debugging")
test_that("helper functions", {
  expect_true(is_empty(NULL), label = "is empty")
  expect_false(is_empty(""), label = "is empty")


  expect_identical(NULL %||% "A", "A", "default")

  expect_identical("B" %||% "A", "B", "default")


  expect_identical(get_ev_level(1.0), "No evidence")
  expect_identical(get_ev_level(1.5), "Anecdotal evidence")
  expect_identical(get_ev_level(3.0), "Anecdotal evidence")
  expect_identical(get_ev_level(3.1), "Moderate evidence")
  expect_identical(get_ev_level(10.0), "Moderate evidence")
  expect_identical(get_ev_level(10.1), "Strong evidence")
  expect_identical(get_ev_level(30.0), "Strong evidence")
  expect_identical(get_ev_level(30.1), "Very strong evidence")
  expect_identical(get_ev_level(100.0), "Very strong evidence")
  expect_identical(get_ev_level(100.1), "Extreme evidence")

  expect_identical(bfsay(1.0), c(
    "Using the levels from Wagenmakers et al (2017)\n",
    "A BF of 1 indicates:\n", "No evidence"
  ))

  expect_identical(bfsay(0.5), c(
    "Using the levels from Wagenmakers et al (2017)\n",
    "A BF of 0.5 indicates:\n", "Anecdotal evidence"
  ))


  expect_identical(bfsay(1L / 100L), c(
    "Using the levels from Wagenmakers et al (2017)\n",
    "A BF of 0.01 indicates:\n", "Very strong evidence"
  ))

  m1 <- integral(likelihood("noncentral_t", 2.8, 19L) * prior("cauchy", 0L, 1L))
  m0 <- integral(likelihood("noncentral_t", 2.8, 19L) * prior("point", 0L))
  b <- m1 / m0

  expect_equal(unclass(summary(b)),
    c("Bayes factor\n", bfsay(b), "\n"),
    ignore_attr = FALSE
  )

  expect_output(show(b), "4.108891 ")
  expect_output(show(m1), "0.05112506 ")


  l <- likelihood("noncentral_t", 2L, 10L)
  p <- prior("cauchy", 0L, 1L)
  prod <- l * p
  post <- extract_posterior(l * p)
  pred <- extract_predictions(l * p)

  # test general messages
  expect_output(show(l), l@desc)
  expect_output(show(p), p@desc)

  expect_output(show(prod), prod@desc)
  expect_output(show(post), post@desc)
  expect_output(show(pred), pred@desc)
})
