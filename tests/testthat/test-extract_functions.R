test_that("multiplication works", {
  l <- likelihood("normal", 0.0, 0.1)
  p <- prior("normal", 0.0, 10.0)
  prod <- l * p
  expect_s4_class(
    extract_predictions(prod),
    "prediction"
  )

  expect_s4_class(
    extract_posterior(prod),
    "posterior"
  )

  expect_s4_class(
    prod,
    "product"
  )
})
