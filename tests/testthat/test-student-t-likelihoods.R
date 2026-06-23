test_that("student_t likelihoods", {
  l <- likelihood("student_t", mean = 5.47, sd = 32.2, df = 119L)
  p1 <- prior("student_t", mean = 13.3, sd = 4.93, df = 72L)

  p0 <- prior("point", 0L)
  expect_bf_equal(bayes_factor(l, p1, p0),
    unclass(0.97),
    tolerance = tol,
    label = "student_t prior (student_t likelihood)"
  )
})
