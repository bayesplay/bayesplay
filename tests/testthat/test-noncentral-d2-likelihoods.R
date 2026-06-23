test_that("noncentral_d2 likelihoods", {
  l <- likelihood("noncentral_d2", -0.16866426138921944422, 17L, 18L)
  p1 <- prior("cauchy", location = 0L, scale = 1L)
  p0 <- prior("point", 0L)

  expect_bf_equal(bayes_factor(l, p1, p0),
    0.274111,
    tolerance = tol,
    label = "default bayes t-test (noncentral_t likelihood)"
  )
})
