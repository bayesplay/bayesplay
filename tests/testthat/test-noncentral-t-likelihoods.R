test_that("noncentral_t likelihoods", {
  l <- likelihood("noncentral_t", t = -0.49871193286623344276, df = 33L)
  p1 <- prior("cauchy", location = 0L, scale = 2.95683228182748658597)
  p0 <- prior("point", 0L)
  expect_bf_equal(bayes_factor(l, p1, p0),
    0.274111,
    tolerance = tol,
    label = "default bayes t-test (noncentral_t likelihood)"
  )

  l <- likelihood("noncentral_t", t = 4.71774600375525743345, df = 9L)
  p1 <- prior("cauchy", location = 0L, scale = 3.16227766016837952279)
  p0 <- prior("point", 0L)
  expect_bf_equal(bayes_factor(l, p1, p0),
    42.44814,
    tolerance = tol,
    label = "default bayes t-test (noncentral_t likelihood)"
  )

  l <- likelihood(family = "noncentral_t", t = 2.03, df = 79L)
  p1 <- prior(family = "cauchy", location = 0L, scale = 8.94427190999915922021)
  p0 <- prior("point", 0L)
  expect_bf_equal(bayes_factor(l, p1, p0),
    0.64207642378841778275,
    label = "default bayes t (t version)",
    tolerance = tol
  )

  l <- likelihood(family = "noncentral_t", t = 4.46, df = 49L)
  p1 <- prior("cauchy", location = 0L, scale = sqrt(50L))
  p0 <- prior("point", 0L)
  expect_bf_equal(bayes_factor(l, p1, p0),
    403.35222779080226018777,
    label = "anomalous t test v1 (as t)",
    tolerance = tol
  )

  l <- likelihood(family = "noncentral_t", t = 4.46, df = 50L)
  p1 <- prior("cauchy", 0L, 5.0489898989798)
  p0 <- prior("point", 0L)
  expect_bf_equal(bayes_factor(l, p1, p0),
    460.2497,
    label = "previously anomalous t test v2 (as t)",
    tolerance = tol
  )
})
