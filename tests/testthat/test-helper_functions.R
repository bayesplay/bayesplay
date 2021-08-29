test_that("helper functions", {
  expect_true(is_empty(NULL), label = "is empty")
  expect_false(is_empty(""), label = "is empty")


  expect_equal(NULL %||% "A", "A", "default")

  expect_equal("B" %||% "A", "B", "default")


  expect_equal(get_ev_level(1), "No evidence")
  expect_equal(get_ev_level(1.5), "Anecdotal evidence")
  expect_equal(get_ev_level(3), "Anecdotal evidence")
  expect_equal(get_ev_level(3.1), "Moderate evidence")
  expect_equal(get_ev_level(10), "Moderate evidence")
  expect_equal(get_ev_level(10.1), "Strong evidence")
  expect_equal(get_ev_level(30), "Strong evidence")
  expect_equal(get_ev_level(30.1), "Very strong evidence")
  expect_equal(get_ev_level(100), "Very strong evidence")
  expect_equal(get_ev_level(100.1), "Extreme evidence")

  expect_equal(bfsay(1), c(
    "Using the levels from Wagenmakers et al (2017)\n",
    "A BF of 1 indicates:\n", "No evidence"
  ))

  expect_equal(bfsay(0.5), c(
    "Using the levels from Wagenmakers et al (2017)\n",
    "A BF of 0.5 indicates:\n", "Anecdotal evidence"
  ))


  expect_equal(bfsay(1 / 100), c(
    "Using the levels from Wagenmakers et al (2017)\n",
    "A BF of 0.01 indicates:\n", "Very strong evidence"
  ))

  m1 <- integral(likelihood("noncentral_t", 2.8, 19) * prior("cauchy", 0, 1))
  m0 <- integral(likelihood("noncentral_t", 2.8, 19) * prior("point", 0))
  b <- m1 / m0

  expect_output(summary(b), paste0("Bayes factor\\n", cat(bfsay(b))))

  expect_output(show(b), "4\\.108891 ")
  expect_output(show(m1), "0\\.05112506 ")


  l <- likelihood("noncentral_t", 2, 10)
  p <- prior("cauchy", 0, 1)
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
