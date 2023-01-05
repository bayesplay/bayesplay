context("Plots")
test_that("Prior plots", {


  # Half normal prior
  half_normal <- prior("normal", 0L, 45L, range = c(0L, Inf))
  half_normal_plot <- plot(half_normal)
  vdiffr::expect_doppelganger("half normal prior", half_normal_plot)

  normal <- prior("normal", 0L, 45L)
  normal_plot <- plot(normal)
  vdiffr::expect_doppelganger("normal prior", normal_plot)



  uniform <- prior("uniform", -10L, 10L)
  uniform_plot <- plot(uniform)
  vdiffr::expect_doppelganger("uniform prior", uniform_plot)

  point <- prior("point", 2L)
  point_plot <- plot(point)
  vdiffr::expect_doppelganger("point prior", point_plot)


  # likelihoods
  binomial_likelihood <- plot(likelihood("binomial", 2L, 10L))
  vdiffr::expect_doppelganger("binomial likelihood", binomial_likelihood)
  normal_likelihood <- plot(likelihood("normal", 0L, 1L))
  vdiffr::expect_doppelganger("normal likelihood", normal_likelihood)



  # visual comparison discrete
  prior_model <- prior(family = "beta", 10L, 10L)
  data_model <- likelihood(family = "binomial", 2L, 10L)
  null_prior <- prior("point", 0.5)

  m0 <- data_model * null_prior
  m1 <- data_model * prior_model
  comparison_plot1 <- visual_compare(m0, m1)
  vdiffr::expect_doppelganger("discrete visual compare", comparison_plot1)


  comparison_plot1a <- visual_compare(m0, m1, ratio = TRUE)
  vdiffr::expect_doppelganger("discrete visual compare", comparison_plot1)



  # visual comparison continuous
  prior_model <- prior(family = "normal", 0L, 13.13)
  data_model <- likelihood(family = "normal", 5.5, 32.35)
  null_prior <- prior("point", 0L)
  m0 <- data_model * null_prior
  m1 <- data_model * prior_model
  comparison_plot2 <- visual_compare(m0, m1)
  vdiffr::expect_doppelganger("continuous visual compare", comparison_plot2)


  # ratio plot continuous
  prior_model <- prior("normal", 0L, 10L)
  data_model <- likelihood("normal", 5L, 5L)
  null_prior <- prior("point", 0L)
  m0 <- data_model * null_prior
  m1 <- data_model * prior_model
  ratio_plot1 <- visual_compare(m0, m1, ratio = TRUE)
  vdiffr::expect_doppelganger("continuous ratio plot", ratio_plot1)

  # ratio plot discrete

  prior_model <- prior(family = "beta", 10L, 10L)
  data_model <- likelihood(family = "binomial", 2L, 10L)
  null_prior <- prior("point", 0.5)

  m0 <- data_model * null_prior
  m1 <- data_model * prior_model
  ratio_plot2 <- visual_compare(m0, m1, ratio = TRUE)
  vdiffr::expect_doppelganger("discrete ratio plot", ratio_plot2)

  # beta-binom
  prior_model <- prior(family = "beta", 10L, 10L)
  data_model <- likelihood(family = "binomial", 2L, 10L)
  m1 <- data_model * prior_model
  prior_posterior1 <- plot(extract_posterior(m1), add_prior = TRUE)
  posterior1 <- plot(extract_posterior(m1))
  prod_plot <- plot(m1)
  vdiffr::expect_doppelganger("beta-binom pp", prior_posterior1)
  vdiffr::expect_doppelganger("beta-binom posterior", posterior1)
  vdiffr::expect_doppelganger("beta-binom weighted likelihood", prod_plot)


  # normal-normal
  prior_model <- prior(family = "normal", 0L, 10L)
  data_model <- likelihood(family = "normal", 5L, 5L)
  m1 <- data_model * prior_model
  prior_posterior2 <- plot(extract_posterior(m1), add_prior = TRUE)
  posterior1 <- plot(extract_posterior(m1))
  prod_plot <- plot(m1)
  vdiffr::expect_doppelganger("normal-normal pp", prior_posterior2)
  vdiffr::expect_doppelganger("normal posterior", posterior1)
  vdiffr::expect_doppelganger("normal weighted likelihood", prod_plot)

  # # point posterior plot
  data_mod <- likelihood(family = "normal", mean = 0L, sd = 1L)
  h0_mod <- prior(family = "point", point = 0L)
  point_posterior <- plot(extract_posterior(data_mod * h0_mod))
  point_posteriora <- plot(extract_posterior(data_mod * h0_mod),
    add_prior = TRUE)
  vdiffr::expect_doppelganger("point_posterior", point_posterior)

  vdiffr::expect_doppelganger("point_posterior_prior", point_posteriora)

  # prediction plot
  continuous_prediction <- extract_predictions(
    prior("normal", 0L, 10L) * likelihood("normal", 2L, 10L)
  )
  continuous_prediction_plot <- plot(continuous_prediction)
  continuous_prediction_plot

  discrete_prediction <- extract_predictions(
    prior("beta", 1L, 1L) * likelihood("binomial", 2L, 10L)
  )
  discrete_prediction_plot <- plot(discrete_prediction)

  vdiffr::expect_doppelganger(
    "continuous prediction",
    continuous_prediction_plot
  )
  vdiffr::expect_doppelganger(
    "discrete prediction",
    discrete_prediction_plot
  )

  # cauchy-normal-d2 prediction plot
  data_model <- likelihood(family = "noncentral_d2", d = 0.5,
    n1 = 10L, n2 = 10L)

  # define alternative prior
  alt_prior <- prior(family = "normal", mean = 0L, sd = 1L)

  # define null prior
  null_prior <- prior(family = "cauchy", location = 0L, scale = 1L)

  # weight likelihood by prior
  m1 <- data_model * alt_prior
  m0 <- data_model * null_prior
  m1_predictions <- extract_predictions(m1)
  m0_predictions <- extract_predictions(m0)
  cauchy_normal_plot <- visual_compare(m1_predictions, m0_predictions)

  vdiffr::expect_doppelganger(
    "cauchy-normal-d2 prediction",
    cauchy_normal_plot
  )
})
