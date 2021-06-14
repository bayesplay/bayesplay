#' Plot a bayesplay object
#' @description Plots an object created by bayesplay
#' @param x a \code{likelihood}, \code{prior}, \code{posterior}, \code{product} or \code{predictive} object #nolint
#' @param ... arguments passed to methods
#' @return a \code{ggplo2} object
setGeneric("plot",
  function(x, ...) standardGeneric("plot"),
  signature = c("x")
)

setMethod("plot", "prior", function(x) {
  plot.prior(x)
})

setMethod("plot", "posterior", function(x, add_prior = FALSE) {
  plot.posterior(x, add_prior)
})

setMethod("plot", "likelihood", function(x) {
  plot.likelihood(x)
})

setMethod("plot", "product", function(x) {
  plot.product(x)
})

setMethod("plot", "prediction", function(x) {
  plot.prediction(x)
})

#' @method plot prior
#' @rdname plot
#' @export
plot.prior <- function(x, ...) {
  return(handle_prior_likelihood(x, n = 101))
}

#' @method plot likelihood
#' @rdname plot
#' @export
plot.likelihood <- function(x, ...) {
  return(handle_prior_likelihood(x, n = 101))
}

#' @method plot posterior
#' @rdname plot
#' @param add_prior set to TRUE to add prior to the posterior plot
#' @export
plot.posterior <- function(x, add_prior = FALSE, ...) {
  if (!add_prior) {
    return(plot_posterior(x, n = 101))
  }
  return(plot_pp(x, n = 101))
}

#' @method plot product
#' @rdname plot
#' @export
plot.product <- function(x, ...) {
  return(plot_weighted_likelihood(x, n = 101))
}

#' @method plot prediction
#' @rdname plot
#' @export
plot.prediction <- function(x, ...) {
  return(plot_prediction(x, n = 101))
}



plot_prediction <- function(x, n, model_name = "model") {
  likelihood_obj <- x@likelihood_obj
  likelihood_family <- likelihood_obj$family

  # now call the functions for different families
  if (likelihood_family == "binomial") {
    return(handle_binomial_marginal(x, n, model_name))
  }

  return(handle_other_marginal(x, n, model_name))
}



handle_binomial_marginal <- function(x, n, model_name) {
  model_func <- x$prediction_function
  plot_range <- c(0, get_binomial_trials(x))
  observation <- x@likelihood_obj@observation

  observation_df <- data.frame(
    observation = observation,
    auc = model_func(observation),
    color = model_name
  )

  observation_range <- seq(plot_range[1], plot_range[2], 1)

  counterfactual <- data.frame(
    observation = observation_range,
    auc = unlist(lapply(observation_range, FUN = model_func))
  )


  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = counterfactual,
      ggplot2::aes(x = observation, y = auc, colour = model_name)
    ) +
    ggplot2::geom_point(
      data = counterfactual,
      ggplot2::aes(
        x = observation,
        y = auc,
        colour = model_name
      ),
      size = 2, shape = 21, fill = "white"
    ) +
    ggplot2::geom_point(
      data = observation_df,
      ggplot2::aes(
        x = observation,
        y = auc,
        colour = model_name
      ),
      size = 2, shape = 16
    ) +
    ggplot2::labs(x = "Outcome", y = "Marginal probability") +
    ggplot2::scale_color_manual(
      values = "black",
      name = NULL,
      labels = NULL,
      guide = FALSE
    ) +
    ggplot2::scale_x_continuous(
      limits = plot_range,
      breaks = integer_breaks()
    ) +
    NULL
}

is_binomial <- function(x) {
  x@likelihood_obj$family == "binomial"
}

get_binomial_trials <- function(x) {
  x@likelihood_obj$parameters$trials
}


get_max_range <- function(x) {
  pr <- x@prior_obj@plot$range
  lk <- x@likelihood_obj@plot$range

  if (is_binomial(x)) {
    return(c(0, get_binomial_trials(x)))
  }

  min_value <- min(c(pr, lk))
  max_value <- max(c(pr, lk))
  return(c(min_value, max_value))
}



handle_other_marginal <- function(x, n, model_name) {
  model_func <- x$prediction_function
  plot_range <- get_max_range(x)
  observation <- x@likelihood_obj@observation

  x <- observation
  y <- model_func(observation)
  color <- model_name
  observation_df <- data.frame(
    x = x, y = y, color = color
  )


  ggplot2::ggplot() +
    ggplot2::geom_function(
      fun = model_func,
      ggplot2::aes(colour = model_name)
    ) +
    ggplot2::geom_point(
      data = observation_df,
      ggplot2::aes(
        x = x,
        y = y,
        colour = model_name
      ),
      size = 2, shape = 16,
    ) +
    ggplot2::labs(x = "Outcome", y = "Marginal probability") +
    ggplot2::scale_color_manual(
      values = "black",
      name = NULL,
      labels = NULL,
      guide = FALSE,
    ) +
    ggplot2::scale_x_continuous(
      limits = plot_range,
    ) +
    NULL
}



handle_prior_likelihood <- function(x, n) {
  if (x@dist_type == "point") {
    return(plot_point(x, n))
  }
  if (x@dist_type == "continuous") {
    return(plot_continuous(x, n))
  }
}

plot_point <- function(x, n) {
  dens <- 1
  theta <- x@parameters$point

  df <- data.frame(
    theta = theta,
    dens = dens
  )

  return(ggplot2::ggplot(
    df,
    ggplot2::aes(x = theta, y = dens)
  ) +
    ggplot2::geom_point(size = 3, shape = 16) +
    ggplot2::geom_linerange(ggplot2::aes(
      x = theta,
      y = NULL,
      ymax = 1,
      ymin = 0
    )) +
    ggplot2::xlim(x@plot$range) +
    ggplot2::labs(x = x@plot$labs$x, y = x@plot$labs$y) +
    ggplot2::expand_limits(y = 0) +
    NULL)
}

plot_continuous <- function(x, n) {
  return(ggplot2::ggplot() +
    ggplot2::geom_function(
      fun = Vectorize(x@func),
      colour = "black",
      na.rm = TRUE,
      n = n
    ) +
    ggplot2::xlim(x@plot$range) +
    ggplot2::labs(x = x@plot$labs$x, y = x@plot$labs$y) +
    ggplot2::expand_limits(y = 0) +
    NULL)
}


plot_weighted_likelihood <- function(x, n) {
  func <- x$weighted_likelihood_function
  plot_range <- x@prior_obj@plot$range

  if (x@likelihood_obj$family == "binomial") {
    plot_range <- c(0, 1)
  }

  return(ggplot2::ggplot() +
    ggplot2::geom_function(
      fun = Vectorize(func),
      colour = "black",
      na.rm = TRUE,
      n = n
    ) +
    ggplot2::labs(
      x = x@prior_obj@plot$labs$x,
      y = "Pr(Outcome) \u00D7 Pr(\u03F4)"
    ) +
    ggplot2::xlim(plot_range) +
    NULL)
}



plot_posterior <- function(x, n) {
  ggplot2::ggplot() +
    ggplot2::geom_function(
      fun = Vectorize(x$posterior_function),
      n = n
    ) +
    ggplot2::xlim(x@prior_obj@plot$range) +
    ggplot2::labs(x = posterior_labs$x, y = posterior_labs$y) +
    ggplot2::expand_limits(y = 0) +
    NULL
}

plot_pp <- function(x, n) {
  ggplot2::ggplot() +
    ggplot2::geom_function(
      fun = Vectorize(x$posterior_function),
      ggplot2::aes(color = "blue")
    ) +
    ggplot2::geom_function(
      fun = Vectorize(x@prior_obj@func),
      ggplot2::aes(color = "red")
    ) +
    ggplot2::xlim(x@prior_obj@plot$range) +
    ggplot2::labs(x = posterior_labs$x, y = posterior_labs$y) +
    ggplot2::scale_colour_manual(
      values = c("blue", "red"),
      labels = c("posterior", "prior"),
      name = NULL
    ) +
    ggplot2::expand_limits(y = 0) +
    # ggplot2::theme_minimal(base_size = 16) +
    NULL
}

#' Visually compare two models
#'
#' @param model1 a \code{predictive} object
#' @param model2 a \code{predictive} object
#' @param ratio show ratio rather than comparison (default: FALSE)
#' @return A \code{ggplot2} object
#'
#' @examples
#' # define two models
#' data_model <- likelihood(family = "noncentral_d", .8, 79)
#' h0_mod <- prior(family = "point", point = 0)
#' h1_mod <- prior(family = "normal", mean = 0, sd = 1)
#' m0 <- extract_predictions(data_model * h0_mod)
#' m1 <- extract_predictions(data_model * h1_mod)
#'
#' # visually compare the model
#' visual_compare(m0, m1)
#' # plot the ratio of the two model predictions
#' visual_compare(m0, m1, ratio = TRUE)
#' @export
visual_compare <- function(model1, model2, ratio = FALSE) {
  model_name1 <- paste0(substitute(model1))
  model_name2 <- paste0(substitute(model2))

  model1_layer <- plot_prediction(model1, n = 101, model_name1)
  model2_layer <- plot_prediction(model2, n = 101, model_name2)

  if (!ratio) {
    return(suppressMessages(
      gginnards::append_layers(
        model1_layer,
        model2_layer$layers
      ) + ggplot2::scale_colour_manual(
        values = c("red", "blue"),
        name = "Model"
      )
    ))
  }

  if (ratio) {
    ratio_function <- function(x) {
      model1$prediction_function(x) /
        model2$prediction_function(x)
    }

    if (model1@likelihood_obj@data$family == "binomial") {
      x <- seq(
        get_max_range(model1)[1],
        get_max_range(model1)[2]
      )
      y <- unlist(lapply(
        FUN = ratio_function,
        seq(
          get_max_range(model1)[1],
          get_max_range(model1)[2], 1
        )
      ))
      df <- data.frame(x = x, y = y)

      return(ggplot2::ggplot(data = df) +
        ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line(ggplot2::aes(x = x, y = y)) +
    ggplot2::scale_x_continuous(
        c(min(get_max_range(model1), get_max_range(model2)),
        max(get_max_range(model1), get_max_range(model2))),
      breaks = integer_breaks()
    ) +
      ggplot2::geom_hline(yintercept = 1, linetype = 2) +
        ggplot2::scale_y_log10() +
        ggplot2::labs(
          x = "Outcome",
          y = paste0("Log 10 BF ", model_name1, " / ", model_name2)
        ))
    }
    if (model1@likelihood_obj@data$family != "binomial") {
    return(ggplot2::ggplot() +
      ggplot2::geom_function(fun = ratio_function, n = 101) +
      ggplot2::xlim(c(
        min(
          get_max_range(model1),
          get_max_range(model2)
        ),
        max(
          get_max_range(model1),
          get_max_range(model2)
        )
      )) +
      ggplot2::geom_hline(yintercept = 1, linetype = 2) +
      ggplot2::scale_y_log10() +
      ggplot2::labs(
        x = "Outcome",
        y = paste0("Log 10 BF ", model_name1, " / ", model_name2)
      ))
    }
  }
}
