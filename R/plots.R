#' Plot a bayesplay object
#' @description Plots an object created by bayesplay
#' @param x a \code{likelihood}, \code{prior}, \code{posterior},
#' \code{product} or \code{predictive} object
#' @param ... arguments passed to methods
#' @return a \code{ggplot2} object
#' @name plot
NULL


#' @name plot
#' @method plot prior
#' @param x a \code{prior} object
#' @param ... arguments passed to methods
#' @rdname plot
#' @exportS3Method plot prior
plot.prior <- function(x, ...) {
  return(handle_prior_likelihood(x, n = 101L))
}


#' @name plot
#' @method plot posterior
#' @param x a \code{posterior} object
#' @param add_prior set to TRUE to add prior to the posterior plot
#' @param ... arguments passed to methods
#' @rdname plot
#' @exportS3Method plot posterior
plot.posterior <- function(x, add_prior = FALSE, ...) {
  if (!add_prior) {
    return(plot_posterior(x, n = 101L))
  }
  return(plot_pp(x, n = 101L))
}


#' @name plot
#' @method plot likelihood
#' @param x a \code{likelihood} object
#' @param ... arguments passed to methods
#' @rdname plot
#' @exportS3Method plot likelihood
plot.likelihood <- function(x, ...) {
  return(handle_prior_likelihood(x, n = 101L))
}


#' @name plot
#' @method plot product
#' @param x a \code{product} object
#' @param ... arguments passed to methods
#' @rdname plot
#' @exportS3Method plot product
plot.product <- function(x, ...) {
  return(plot_weighted_likelihood(x, n = 101L))
}


#' @title plot
#' @method plot prediction
#' @param x a \code{prediction} object
#' @param model_name name of the model
#' @param ... arguments passed to methods
#' @rdname plot
#' @exportS3Method plot prediction
plot.prediction <- function(x, model_name = "model", ...) {
  return(plot_prediction(x, model_name, ...))
}

plot_prediction <- function(x, model_name = "model", ...) {
  likelihood_obj <- x@likelihood_obj
  likelihood_family <- likelihood_obj[["family"]]

  # now call the functions for different families
  if (likelihood_family == "binomial") {
    return(handle_binomial_marginal(x, model_name, ...))
  }

  return(handle_other_marginal(x, model_name, ...))
}



handle_binomial_marginal <- function(x, model_name = "model", ...) {
  model_func <- suppressWarnings(x[["prediction_function"]])
  plot_range <- c(0L, get_binomial_trials(x))
  observation <- x@likelihood_obj@observation

  observation_df <- data.frame(
    observation = observation,
    auc = model_func(observation),
    color = model_name,
    linetype = model_name
  )

  observation_range <- seq(plot_range[[1L]], plot_range[[2L]], 1L)

  counterfactual <- data.frame(
    observation = observation_range,
    auc = unlist(lapply(observation_range, FUN = model_func))
  )


  ggplot() +
    geom_line(
      data = counterfactual,
      aes(x = observation, y = auc, colour = model_name)
    ) +
    geom_point(
      data = counterfactual,
      aes(
        x = observation,
        y = auc,
        colour = model_name
      ),
      size = 2L, shape = 21L, fill = "white"
    ) +
    geom_point(
      data = observation_df,
      aes(
        x = observation,
        y = auc,
        colour = model_name
      ),
      size = 2L, shape = 16L
    ) +
    labs(x = "Outcome", y = "Marginal probability") +
    scale_colour_manual(
      values = "black",
      name = NULL,
      labels = NULL,
      guide = "none"
    ) +
    scale_linetype_manual(
      values = 1L,
      name = NULL,
      labels = NULL,
      guide = "none"
    ) +
    scale_x_continuous( # nolint
      limits = plot_range,
      breaks = integer_breaks()
    ) +
    NULL
}

is_binomial <- function(x) {
  x@likelihood_obj[["family"]] == "binomial"
}

get_binomial_trials <- function(x) {
  x@likelihood_obj[["parameters"]][["trials"]]
}


get_max_range <- function(x) {
  pr <- x@prior_obj@plot[["range"]]
  lk <- x@likelihood_obj@plot[["range"]]

  if (is_binomial(x)) {
    return(c(0L, get_binomial_trials(x)))
  }

  min_value <- min(c(pr, lk))
  max_value <- max(c(pr, lk))
  return(c(min_value, max_value))
}



handle_other_marginal <- function(x, model_name = "model", ...) {
  model_func <- suppressWarnings(x[["prediction_function"]])
  plot_range <- get_max_range(x)
  observation <- x@likelihood_obj@observation

  x <- observation
  y <- model_func(observation)
  color <- model_name
  linetype <- model_name
  observation_df <- data.frame(
    x = x, y = y, color = color, linetype = linetype
  )

  ggplot() +
    geom_function(
      fun = suppressWarnings(model_func),
      aes(colour = model_name, linetype = model_name)
    ) +
    geom_point(
      data = observation_df,
      aes(
        x = x,
        y = y,
        colour = model_name
      ),
      size = 2L, shape = 16L
    ) +
    labs(x = "Outcome", y = "Marginal probability") +
    scale_colour_manual(
      values = "black",
      name = NULL,
      labels = NULL,
      guide = "none"
    ) +
    scale_linetype_manual(
      values = 1L,
      name = NULL,
      labels = NULL,
      guide = "none"
    ) +
    scale_x_continuous( # nolint
      limits = plot_range
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
  dens <- 1L
  theta <- x@parameters[["point"]]

  df <- data.frame(
    theta = theta,
    dens = dens
  )

  return(ggplot(
    df,
    aes(x = theta, y = dens)
  ) +
    geom_point(size = 3L, shape = 16L) +
    geom_linerange(aes( # nolint
      x = theta,
      y = NULL,
      ymax = 1L,
      ymin = 0L
    )) +
    xlim(x@plot[["range"]]) +
    labs(x = x@plot[["labs"]][["x"]], y = x@plot[["labs"]][["y"]]) +
    expand_limits(y = 0L) +
    NULL)
}

plot_continuous <- function(x, n) {
  return(ggplot() +
    geom_function(
      fun = Vectorize(x@func),
      colour = "black",
      na.rm = TRUE,
      n = n
    ) +
    xlim(x@plot[["range"]]) +
    labs(x = x@plot[["labs"]][["x"]], y = x@plot[["labs"]][["y"]]) +
    expand_limits(y = 0L) +
    NULL)
}


plot_weighted_likelihood <- function(x, n) {
  if (x@approximation) {
    stop("Marginal likelihood has been approximated; ",
      "Can't reliably output a plot.",
      call. = FALSE
    )
  }

  func <- x[["weighted_likelihood_function"]]
  plot_range <- x@prior_obj@plot[["range"]]

  if (x@likelihood_obj[["family"]] == "binomial") {
    plot_range <- c(0L, 1L)
  }

  return(ggplot() +
    geom_function(
      fun = Vectorize(func),
      colour = "black",
      na.rm = TRUE,
      n = n
    ) +
    labs(
      x = x@prior_obj@plot[["labs"]][["x"]],
      y = "Pr(Outcome) \u00D7 Pr(\u03F4)"
    ) +
    xlim(plot_range) +
    NULL)
}



plot_posterior <- function(x, n) {
  if (x@prior_obj@dist_type == "point") {
    return(
      plot_point(x@prior_obj, n) +
        labs(y = "Density")
    )
  }
  ggplot() +
    geom_function(
      fun = Vectorize(x[["posterior_function"]]),
      n = n
    ) +
    xlim(x@prior_obj@plot[["range"]]) +
    labs(x = posterior_labs[["x"]], y = posterior_labs[["y"]]) +
    expand_limits(y = 0L) +
    NULL
}

plot_pp <- function(x, n) {
  if (x@prior_obj@dist_type == "point") {
    theta <- unique(x@prior_obj@theta_range)
    return(
      ggplot() +
        geom_point(
          aes(
            x = theta,
            y = x@prior_obj[["prior_function"]](0L),
            color = "prior"
          ),
          size = 3L, shape = 16L
        ) +
        geom_linerange(aes( # nolint
          x = unique(x@prior_obj@theta_range),
          y = NULL,
          ymax = x@prior_obj[["prior_function"]](0L),
          ymin = 0L,
          color = "prior"
        )) +
        geom_point(
          aes(
            x = theta,
            y = x@prior_obj[["prior_function"]](0L),
            color = "posterior"
          ),
          size = 3L, shape = 16L
        ) +
        geom_linerange(aes(
          x = unique(x@prior_obj@theta_range),
          y = NULL,
          ymax = x[["posterior_function"]](0L),
          ymin = 0L,
          color = "posterior"
        )) +
        xlim(x@prior_obj@plot[["range"]]) +
        labs(x = posterior_labs[["x"]], y = posterior_labs[["y"]]) +
        scale_colour_manual(
          values = c("blue", "red"),
          labels = c("posterior", "prior"),
          name = NULL
        ) +
        NULL
    )
  }

  ggplot() +
    geom_function(
      fun = Vectorize(x[["posterior_function"]]),
      aes(color = "blue", linetype = "blue")
    ) +
    geom_function(
      fun = Vectorize(x@prior_obj@func),
      aes(color = "red", linetype = "red")
    ) +
    xlim(x@prior_obj@plot[["range"]]) +
    labs(x = posterior_labs[["x"]], y = posterior_labs[["y"]]) +
    scale_colour_manual(
      values = c("blue", "red"),
      labels = c("posterior", "prior"),
      name = NULL
    ) +
    scale_linetype_manual(
      values = c(1L, 1L),
      labels = c("posterior", "prior"),
      name = NULL
    ) +
    expand_limits(y = 0L) +
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
#' data_model <- likelihood(family = "normal", .5, 1)
#' h0_mod <- prior(family = "point", point = 0)
#' h1_mod <- prior(family = "normal", mean = 0, sd = 10)
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

  suppressWarnings({
    model1_layer <- plot_prediction(model1, n = 101L, model_name1)
  })

  suppressWarnings({
    model2_layer <- plot_prediction(model2, n = 101L, model_name2)
  })

  if (!ratio) {
    return(suppressMessages(
      append_layers(
        model1_layer,
        model2_layer[["layers"]]
      ) +
        scale_colour_manual(
          values = c("red", "blue"),
          name = "Model"
        ) +
        scale_linetype_manual(
          values = c(1L, 1L),
          name = "Model"
        )
    ))
  }

  if (ratio) {
    ratio_function <- function(x) {
      suppressWarnings({
        model1[["prediction_function"]](x) /
          model2[["prediction_function"]](x)
      })
    }

    if (model1@likelihood_obj@data[["family"]] == "binomial") {
      x <- seq(
        get_max_range(model1)[[1L]],
        get_max_range(model1)[[2L]]
      )
      y <- unlist(lapply(
        FUN = ratio_function,
        seq(
          get_max_range(model1)[[1L]],
          get_max_range(model1)[[2L]], 1L
        )
      ))
      df <- data.frame(x = x, y = y)

      return(ggplot(data = df) +
        geom_point(aes(x = x, y = y)) +
        geom_line(aes(x = x, y = y)) +
        scale_x_continuous( # nolint
          c(
            min(get_max_range(model1), get_max_range(model2)),
            max(get_max_range(model1), get_max_range(model2))
          ),
          breaks = integer_breaks()
        ) +
        geom_hline(yintercept = 1L, linetype = 2L) +
        scale_y_log10() +
        labs(
          x = "Outcome",
          y = paste0("Log 10 BF ", model_name1, " / ", model_name2)
        ))
    }
    if (model1@likelihood_obj@data[["family"]] != "binomial") {
      return(ggplot() +
        geom_function(fun = ratio_function, n = 101L) +
        xlim(c(
          min(
            get_max_range(model1),
            get_max_range(model2)
          ),
          max(
            get_max_range(model1),
            get_max_range(model2)
          )
        )) +
        geom_hline(yintercept = 1L, linetype = 2L) +
        scale_y_log10() +
        labs(
          x = "Outcome",
          y = paste0("Log 10 BF ", model_name1, " / ", model_name2)
        ))
    }
  }
}
