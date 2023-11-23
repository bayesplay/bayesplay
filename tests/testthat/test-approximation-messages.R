check_field_names <- function(approx, label) {
  fields <- c(
    "approximation",
    "supported_prior",
    "is_large",
    "not_in_prior",
    "n",
    "t",
    "df"
  )

  testthat::expect_named(approx, fields, label = label)
}

check_all_valid <- function(approx, label) {
  Map(function(x) {
    testthat::expect_false(is.na(x), label = label)
  }, approx)
}


test_that("Needs approximation", {
  l <- likelihood("noncentral_d", d = 20L, n = 10L)
  p1 <- prior("cauchy", 0L, 1L, c(-Inf, Inf))
  approx <- check_approximation(l, p1)
  check_field_names(approx, "noncentral d 1")
  check_all_valid(approx, "noncentral d 1")


  l <- likelihood("noncentral_d", d = 3L, n = 10L)
  p1 <- prior("cauchy", 0L, 1L, c(-Inf, 0L))
  approx <- check_approximation(l, p1)
  check_field_names(approx, "noncentral d 1")
  check_all_valid(approx, "noncentral d 1")


  l <- likelihood("noncentral_d", d = 3L, n = 10L)
  p1 <- prior("cauchy", 0L, 1L, c(-Inf, Inf))
  approx <- check_approximation(l, p1)
  check_field_names(approx, "noncentral d 1")
  check_all_valid(approx, "noncentral d 1")



  l <- likelihood("noncentral_d2", d = 20L, n1 = 10L, n2 = 20L)
  p1 <- prior("cauchy", 0L, 1L, c(-Inf, Inf))
  approx <- check_approximation(l, p1)
  check_field_names(approx, "noncentral d2")
  check_all_valid(approx, "noncentral d2")

  l <- likelihood("noncentral_d2", d = 5L, n1 = 10L, n2 = 20L)
  p1 <- prior("cauchy", 0L, 1L, c(-Inf, 0L))
  approx <- check_approximation(l, p1)
  check_field_names(approx, "noncentral d2")
  check_all_valid(approx, "noncentral d2")

  l <- likelihood("noncentral_d2", d = 5L, n1 = 10L, n2 = 20L)
  p1 <- prior("cauchy", 0L, 1L, c(-Inf, Inf))
  approx <- check_approximation(l, p1)
  check_field_names(approx, "noncentral d2")
  check_all_valid(approx, "noncentral d2")



  l <- likelihood("noncentral_t", t = 23L, df = 10L)
  p1 <- prior("cauchy", 0L, 1L, c(-Inf, Inf))
  approx <- check_approximation(l, p1)
  check_field_names(approx, "noncentral t")

  approx_without_n <- approx[which(!(names(approx) %in% "n"))]
  check_all_valid(approx_without_n, "noncentral t")

  testthat::expect_true(is.na(approx[["n"]]), label = "noncentral t (n)")


  l <- likelihood("noncentral_t", t = 6L, df = 10L)
  p1 <- prior("cauchy", 0L, 1L, c(-Inf, Inf))
  approx <- check_approximation(l, p1)
  check_field_names(approx, "noncentral t")

  approx_without_n <- approx[which(!(names(approx) %in% "n"))]
  check_all_valid(approx_without_n, "noncentral t")

  testthat::expect_true(is.na(approx[["n"]]), label = "noncentral t (n)")


  l <- likelihood("noncentral_t", t = 6L, df = 10L)
  p1 <- prior("cauchy", 0L, 1L, c(-Inf, 0L))
  approx <- check_approximation(l, p1)
  check_field_names(approx, "noncentral t")

  approx_without_n <- approx[which(!(names(approx) %in% "n"))]
  check_all_valid(approx_without_n, "noncentral t")

  testthat::expect_true(is.na(approx[["n"]]), label = "noncentral t (n)")
})
