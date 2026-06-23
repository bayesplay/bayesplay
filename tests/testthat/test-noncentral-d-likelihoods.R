test_that("noncentral_d likelihood", {
  l <- likelihood(family = "noncentral_d", d = 0.22696089971622862569, n = 80L)
  p1 <- prior("cauchy", location = 0L, scale = 1L)
  p0 <- prior("point", 0L)
  expect_bf_equal(bayes_factor(l, p1, p0),
    0.64207642378841778275,
    tolerance = tol,
    label = "default bayes t-test (noncentral_d likelihood)"
  )

  l <- likelihood(family = "noncentral_d", d = 0.22696089971622862569, n = 80L)
  p1 <- prior("cauchy", location = 0L, scale = 1L, range = c(0L, Inf))
  p0 <- prior("point", 0L)

  expect_bf_equal(bayes_factor(l, p1, p0),
    1.25399711580663364430,
    tolerance = tol,
    label = "default bayes t-test (noncentral_d likelihood)"
  )

  l <- likelihood("noncentral_d", 0.624524917476492, 51L)
  p1 <- prior("cauchy", 0L, 0.707)
  p0 <- prior("point", 0L)
  expect_bf_equal(bayes_factor(l, p1, p0),
    460.2497,
    label = "previously anomalous t test v2 (as d)",
    tolerance = tol
  )

  l <- likelihood("noncentral_d", -2.24, 34L)
  p1 <- prior("cauchy", 0L, 0.707, c(0L, Inf))
  p0 <- prior("point", 0L)
  expect_bf_equal(bayes_factor(l, p1, p0, quiet = TRUE),
    0.006772853,
    label = "previously anomalous t test v2 (as t)",
    tolerance = tol
  )
})

test_that("noncentral_d likelihoods", {
  l <- likelihood(family = "noncentral_d", d = 1.49188227940238959945, n = 10L)
  p1 <- prior(family = "cauchy", 0L, 1L)
  p0 <- prior(family = "point", 0L)
  expect_bf_equal(bayes_factor(l, p1, p0),
    42.44814,
    tolerance = tol,
    label = "default bayes t-test (noncentral_d likelihood)"
  )
})
