context("Plots")
test_that("Prior plots", {

  # TODO: RE-WRITE ALL THE PLOT TESTS!
  # Half normal prior
  half_normal <- prior("normal", 0, 45, range = c(0, Inf))
  half_normal_plot <- plot(half_normal)
  half_normal_plot
  vdiffr::expect_doppelganger("half normal prior", half_normal_plot)

  normal <- prior("normal", 0, 45)
  normal_plot <- plot(normal)
  normal_plot
  vdiffr::expect_doppelganger("normal prior", normal_plot)



  uniform <- prior("uniform", -10, 10)
  uniform_plot <- plot(uniform)
  uniform_plot
  vdiffr::expect_doppelganger("uniform prior", uniform_plot)

  point <- prior("point", 2)
  point_plot <- plot(point)
  point_plot
  vdiffr::expect_doppelganger("point prior", point_plot)


  # likelihoods
  binomial_likelihood <- plot(likelihood("binomial", 2, 10))
  binomial_likelihood
  vdiffr::expect_doppelganger("binomial likelihood", binomial_likelihood)
  normal_likelihood <- plot(likelihood("normal", 0, 1))
  normal_likelihood
  vdiffr::expect_doppelganger("normal likelihood", normal_likelihood)



  # visual comparison discrete
  prior_model <- prior(family = "beta", 10, 10)
  data_model <- likelihood(family = "binomial", 2, 10)
  null_prior <- prior("point", 0.5)
  plot(null_prior)

  m0 <- data_model * null_prior
  m1 <- data_model * prior_model
  comparison_plot1 <- visual_compare(m0, m1)
  comparison_plot1
  vdiffr::expect_doppelganger("discrete visual compare", comparison_plot1)


  # visual comparison continuous
  prior_model <- prior(family = "normal", 0, 13.13)
  data_model <- likelihood(family = "normal", 5.5, 32.35)
  null_prior <- prior("point", 0)
  m0 <- data_model * null_prior
  m1 <- data_model * prior_model
  comparison_plot2 <- visual_compare(m0, m1)
  comparison_plot2
  vdiffr::expect_doppelganger("continuous visual compare", comparison_plot2)


  # ratio plot continuous
  prior_model <- prior("normal", 0, 10)
  data_model <- likelihood("normal", 5, 5)
  null_prior <- prior("point", 0)
  m0 <- data_model * null_prior
  m1 <- data_model * prior_model
  ratio_plot1 <- visual_compare(m0, m1, ratio = TRUE)
  vdiffr::expect_doppelganger("continuous ratio plot", ratio_plot1)

  # ratio plot discrete

  prior_model <- prior(family = "beta", 10, 10)
  data_model <- likelihood(family = "binomial", 2, 10)
  null_prior <- prior("point", 0.5)

  m0 <- data_model * null_prior
  m1 <- data_model * prior_model
  ratio_plot2 <- visual_compare(m0, m1, ratio = TRUE)
  ratio_plot2
  vdiffr::expect_doppelganger("discrete ratio plot", ratio_plot2)

  # beta-binom
  prior_model <- prior(family = "beta", 10, 10)
  data_model <- likelihood(family = "binomial", 2, 10)
  m1 <- data_model * prior_model
  prior_posterior1 <- plot(extract_posterior(m1), add_prior = TRUE)
  prior_posterior1
  posterior1 <- plot(extract_posterior(m1))
  posterior1
  prod_plot <- plot(m1)
  prod_plot
  vdiffr::expect_doppelganger("beta-binom pp", prior_posterior1)
  vdiffr::expect_doppelganger("beta-binom posterior", posterior1)
  vdiffr::expect_doppelganger("beta-binom weighted likelihood", prod_plot)


  # normal-normal
  prior_model <- prior(family = "normal", 0, 10)
  data_model <- likelihood(family = "normal", 5, 5)
  m1 <- data_model * prior_model
  prior_posterior2 <- plot(extract_posterior(m1), add_prior = TRUE)
  prior_posterior2
  posterior1 <- plot(extract_posterior(m1))
  prod_plot <- plot(m1)
  prod_plot
  vdiffr::expect_doppelganger("normal-normal pp", prior_posterior2)
  vdiffr::expect_doppelganger("normal posterior", posterior1)
  vdiffr::expect_doppelganger("normal weighted likelihood", prod_plot)


  # prediction plot
  continuous_prediction <- extract_predictions(
    prior("normal", 0, 10) * likelihood("normal", 2, 10)
  )
  continuous_prediction_plot <- plot(continuous_prediction)
  continuous_prediction_plot

  discrete_prediction <- extract_predictions(
    prior("beta", 1, 1) * likelihood("binomial", 2, 10)
  )
  discrete_prediction_plot <- plot(discrete_prediction)

  vdiffr::expect_doppelganger("continuous prediction", continuous_prediction_plot)
  vdiffr::expect_doppelganger("discrete prediction", discrete_prediction_plot)
})
