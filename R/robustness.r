#' @include utils.r

make_bf_rr_func <- function(data_model,
                            alternative_prior,
                            null_prior, parameters) {
  base_prior_family <- alternative_prior[["family"]]
  base_defined <- names(alternative_prior[["parameters"]])
  instance_defined <- names(parameters)

  allowed <- allowed_parameters[[base_prior_family]]
  if (!all(instance_defined %in% allowed)) {
    illegal_params <- instance_defined[!(instance_defined %in% allowed)]
    warning_msg <- paste(illegal_params, "is not a valid parameter for a",
      base_prior_family, "prior",
      collapse = "\n"
    )

    stop(
      warning_msg,
      "\nValid parameters are: ", toString(allowed)
    )
  }


  base_defined <- alternative_prior[["parameters"]][
    !(names(alternative_prior[["parameters"]]) %in% instance_defined)
  ]
  null_value <- integral(data_model * null_prior)
  bf_func <- function(params) {
    prior <- do.call(
      "prior",
      c(list(family = base_prior_family), c(base_defined, params))
    )
    integral(data_model * prior) / null_value
  }
  bf_func
}




makes_values_list <- function(parameters, steps = 100L) {
  values <- lapply(parameters, function(x) {
    do.call(
      what = "seq",
      args = list(from = x[[1L]], to = x[[2L]], length.out = steps + 1L)
    )
  })





  if ("sd" %in% names(parameters)) {
    values[["sd"]] <- values[["sd"]][values[["sd"]] > 0L]
  }


  if ("scale" %in% names(parameters)) {
    values[["scale"]] <- values[["scale"]][values[["scale"]] > 0L]
  }

  if ("df" %in% names(parameters)) {
    values[["df"]] <- values[["df"]][values[["df"]] > 0L]
  }


  values <- lapply(values, function(x) {
    x[1L:steps]
  })

  n_values <- prod(unlist(map(values, length)))
  if (n_values > (101L * 101L)) {
    warning("Number of values to evaluate is large. Consider using a smaller
range.")
  }


  expand.grid(values, KEEP.OUT.ATTRS = FALSE)
}



allowed_parameters <- list(
  normal = c("mean", "sd", "range"),
  student_t = c("mean", "sd", "df", "range"),
  cauchy = c("location", "scale", "range"),
  uniform = c("min", "max"),
  point = "point",
  beta = c("alpha", "beta")
)


#' Perform a robustness analysis
#' @description Perform a robustness analysis by systematically varying the
#' values of the alternative prior
#'
#' @param likelihood \code{likelihood} object representing the data
#' @param alternative_prior \code{prior} object for the alternative prior
#' @param null_prior \code{prior} object for the null prior
#' @param parameters \code{list} of min and max values for each parameter to
#' vary over.
#' @param steps \code{numeric} the number of steps each parameter is varied
#' over. (Default: 100 steps)
#' @param cutoff Minimum Bayes factor value for evidence for the hypothesis
#' (Default: 3)
#' @param multicore Run robustness analysis across multiple cores
#' (Default: TRUE if available)
#' @return A \code{robustness} object
#'
#' @examples
#'
#' # define a likelihood
#' data_model <- likelihood(family = "normal", mean = 1, sd = 5)
#'
#' # define the alternative prior
#' alternative_prior <- prior(family = "normal", mean = 5, sd = 4)
#'
#' # define the null prior
#' null_prior <- prior(family = "point", point = 0L)
#'
#' # set the parameters to vary and the range to vary them across
#' # vary the mean from -10 to 5
#' # vary the sd from 1 to 5
#' parameters <- list(mean = c(-10, 5), sd = c(1, 5))
#'
#' # mark all Bayes factors larger/smaller than 3/.3 as evidence for the
#' # alternative / null
#' cutoff <- 3
#' bfrr(data_model, alternative_prior, null_prior, parameters, steps = 10,
#'   cutoff,
#'   multicore = FALSE
#' )
#' @export
bfrr <- function(likelihood,
                 alternative_prior,
                 null_prior,
                 parameters,
                 steps = 100L,
                 cutoff = 3L,
                 multicore = TRUE) {
  # check whether multicore is available
  if (multicore && (parallel::detectCores() == 1L)) {
    multicore <- FALSE
    message("Multicore is not available, running on a single core")
  }


  bf_func <- make_bf_rr_func(
    likelihood,
    alternative_prior,
    null_prior,
    parameters
  )

  if (cutoff < 1L) {
    cutoff <- 1L / cutoff
  }

  input_values <- list(
    likelihood = likelihood,
    alternative_prior = alternative_prior,
    null_prior = null_prior,
    parameters = parameters,
    steps = steps,
    cutoff = cutoff
  )

  bf_value <- integral(likelihood * alternative_prior) /
    integral(likelihood * null_prior)


  original_values <- alternative_prior |>
    slot("parameters")


  # check if the original values are in the range of the parameters
  lapply(names(parameters), FUN = function(x) {
    if (original_values[[x]] < parameters[[x]][[1L]]) {
      stop(
        "The original value for ", x, " (", original_values[[x]],
        ") is less than the minimum value for ",
        x, " (", parameters[[x]][[1L]], ")"
      )
    }
    if (original_values[[x]] > parameters[[x]][[2L]]) {
      stop(
        "The original value for ", x, " (", original_values[[x]],
        ") is greater than the maximum value for ",
        x, " (", parameters[[x]][[2L]], ")"
      )
    }
  })

  values_df <- makes_values_list(parameters, steps)


  if (multicore) {
    suppressMessages({
      bfs <- par_pmap(values_df, \(x) cbind(x, data.frame(bf = bf_func(x))))
    })
  } else {
    suppressMessages({
      bfs <- pmap(values_df, \(x) cbind(x, data.frame(bf = bf_func(x))))
    })
  }


  base_params <- alternative_prior[["parameters"]]
  instance_defined <- names(parameters)
  output_values <- cbind(
    bfs,
    list_to_df(base_params[!(names(base_params) %in% names(parameters))])
  )


  output_values[["support"]] <- get_support(bfs[["bf"]], cutoff)


  data <- list(
    data = output_values,
    required_parameters = instance_defined,
    cutoff = cutoff,
    input_values = input_values,
    bf_value = bf_value
  )

  desc <- describe_robustness(data)

  new(
    Class = "robustness",
    data = data,
    desc = desc
  )
}


describe_robustness <- function(data) {
  step_size <- get_precision(data)
  alternative_prior <- describe_prior(
    new(data[["input_values"]][["alternative_prior"]][["family"]]),
    data[["input_values"]][["alternative_prior"]][["parameters"]]
  )

  alternative_prior_desc <- strsplit(alternative_prior, "\n",
    fixed = TRUE
  )[[1L]]
  alternative_prior_desc[[1L]] <- "Alternative prior\n  -----------------"
  alternative_prior_desc <- paste0(alternative_prior_desc, collapse = "\n  ")


  null_prior <- describe_prior(
    new(data[["input_values"]][["null_prior"]][["family"]]),
    data[["input_values"]][["null_prior"]][["parameters"]]
  )
  null_prior_desc <- strsplit(null_prior, "\n", fixed = TRUE)[[1L]]
  null_prior_desc[[1L]] <- "Null prior\n  -----------------"
  null_prior_desc <- paste0(null_prior_desc, collapse = "\n  ")

  likelihood <- describe_likelihood(
    new(data[["input_values"]][["likelihood"]][["family"]]),
    data[["input_values"]][["likelihood"]][["parameters"]]
  )
  likelihood_desc <- strsplit(likelihood, "\n", fixed = TRUE)[[1L]]
  likelihood_desc[[1L]] <- "Likelihood\n  -----------------"
  likelihood_desc <- paste0(likelihood_desc, collapse = "\n  ")
  parameters <- data[["input_values"]][["parameters"]]
  n_varied <- length(parameters)
  n_varied_text <- ifelse(n_varied == 1L,
    " parameter of the alternative prior was ",
    " parameters of the alternative prior were "
  )

  varied_text <- names(parameters) |>
    map(function(x) {
      paste0(
        "  The ", x, " was varied from ", parameters[[x]][[1L]],
        " to ", parameters[[x]][[2L]],
        " (step size: ", signif(step_size[[x]], 3L), ")"
      )
    }) |>
    paste0(collapse = "\n")


  bf_base <- data[["bf_value"]]
  output_values <- data[["data"]]
  cutoff <- data[["cutoff"]]
  base_support <- get_support(bf_base, cutoff)

  support_values <- table(output_values[["support"]])

  h1_text <- paste0(
    support_values[["Evidence for H1"]], " of ", sum(support_values),
    " (", round(support_values[["Evidence for H1"]] / sum(support_values), 2L),
    ") tested priors provided evidence for H1 (BF > ",
    round(cutoff, 2L), ")"
  )

  h0_text <- paste0(
    support_values[["Evidence for H0"]], " of ", sum(support_values),
    " (", round(support_values[["Evidence for H0"]] / sum(support_values), 2L),
    ") tested priors provided evidence for H0 (BF < ",
    round(1L / cutoff, 2L), ")"
  )

  inc_text <- paste0(
    support_values[["Inconclusive"]], " of ", sum(support_values),
    " (", round(support_values[["Inconclusive"]] / sum(support_values), 2L),
    ") tested priors were inconclusive (",
    round(1L / cutoff, 2L), " < BF < ", round(cutoff, 2L), ")"
  )

  robust_regions <- round(mean(output_values[["support"]] == base_support), 2L)

  robust_text <- paste0(
    "The conclusion of '", base_support, "' was robust over ",
    robust_regions, " tested priors"
  )

  if (base_support != "Inconclusive") {
    reverse_region <- mean(
      output_values[["support"]] == invert_support(base_support)
    )

    if (reverse_region > 0L) {
      robust_text <- paste0(
        robust_text,
        "\n\n",
        round(reverse_region, 2L),
        " of tested priors gave the opposite conclusion of '",
        invert_support(base_support), "' "
      )
    } else {
      robust_text <- paste0(
        robust_text,
        "\n\n",
        "No tested priors gave the opposite conclusion"
      )
    }
  }



  base_text <- switch(as.character(base_support),
    Inconclusive = "is inconclusive.",
    `Evidence for H0` = "provides evidence for H0.",
    `Evidence for H1` = "provides evidence for H1."
  )

  could_not_compute <- "\n"
  if (support_values[["None"]]) {
    could_not_compute <- paste0(
      "\n\n",
      "BFs could not be computed for ", support_values[["None"]], " of ",
      sum(support_values), " (", round(support_values[["None"]] /
        sum(support_values), 2L),
      ") tested priors\n"
    )
  }

  paste0(
    "Prior robustness analysis:\n\n  ",
    alternative_prior_desc, "\n\n  ",
    null_prior_desc, "\n\n  ",
    likelihood_desc, "\n\n ",
    "The original parameters gave a Bayes factor of ", round(bf_base, 2L),
    ".\nUsing the cutoff value of ", cutoff, ", this result ", base_text,
    "\n\n", n_varied, n_varied_text, "varied for the robustness analysis:\n",
    varied_text, "\n\n",
    "Outcome\n-----------------\n",
    h0_text, "\n\n",
    h1_text, "\n\n",
    inc_text, "\n\n",
    robust_text,
    could_not_compute
  )
}



support_factor <- function(x) {
  factor(x,
    levels = c("Evidence for H0", "Inconclusive", "Evidence for H1", "None")
  )
}


get_support <- Vectorize(function(x, cutoff) {
  if (is.nan(x)) {
    return(support_factor("None"))
  }
  if (x <= 1L / cutoff) {
    return(support_factor("Evidence for H0"))
  }
  if (x >= cutoff) {
    return(support_factor("Evidence for H1"))
  }
  support_factor("Inconclusive")
})


invert_support <- function(x) {
  support_value <- switch(as.numeric(x),
    -1L,
    0L,
    1L,
    NaN
  ) * -1L
  switch(as.character(support_value),
    "-1" = "H0",
    "0" = "Inc",
    "1" = "H1",
    "NaN" = "None"
  ) |>
    factor(
      levels = c("H0", "Inc", "H1", "None"),
      labels = c("Evidence for H0", "Inconclusive", "Evidence for H1", "None")
    )
}

#' Summarise a Robustness analysis
#' @description Provide a summary of a robustness analysis
#' @param object a \code{robustness} object
#' @export
#' @return No return, called for side effects
setMethod(
  "summary",
  "robustness",
  function(object) {
    cat(slot(object, "desc"))
  }
)

get_l1 <- function(x, l1) slot(x, "data")[[l1]]
get_l2 <- function(x, l1, l2) slot(x, "data")[[l1]][[l2]]
get_l3 <- function(x, l1, l2, l3) slot(x, "data")[[l1]][[l2]][[l3]]

get_data <- function(x) get_l1(x, "data")
get_required_param <- function(x) get_l1(x, "required_parameters")

get_set_params <- function(x) {
  get_l3(
    x,
    "input_values",
    "alternative_prior",
    "parameters"
  )
}

get_precision <- function(x) {
  input_values <- x[["input_values"]]
  steps <- input_values[["steps"]]
  step_size <- unlist(lapply(names(input_values[["parameters"]]), function(x) {
    input_values[["parameters"]][[x]][[3L]] <- diff(
      input_values[["parameters"]][[x]][1L:2L]
    ) / steps
  }))
  names(step_size) <- names(input_values[["parameters"]])
  step_size
}

get_cutoff <- function(x) get_l2(x, "input_values", "cutoff")

color_palette <- c(
  H1 = "#2a9d8f",
  In = "#e9c46a",
  H0 = "#e76f51"
)

#' @name plot
#' @method plot robustness
#' @param x a \code{robustness} object
#' @param ... arguments passed to methods
#' @rdname plot
#' @exportS3Method plot robustness
plot.robustness <- function(x, ...) {
  # first work out how many were varied
  n_params <- slot(x, "data")[["required_parameters"]] |> length()
  if (n_params == 1L) {
    return(robustness_plot_one(x))
  }
  if (n_params == 2L) {
    return(robustness_plot_two(x))
  }
}


robustness_plot_one <- function(x) {
  data <- get_data(x)

  param <- get_required_param(x)

  set_params <- get_set_params(x)

  base_param <- if (length(param) > 1L) {
    set_params[param]
  } else {
    set_params[[param]]
  }

  base_bf <- x[["bf_value"]]

  precision <- get_precision(x)

  cutoff <- get_cutoff(x)

  height <- max(c(max(data[["bf"]]), cutoff))

  ggplot(
    data,
    aes(
      x = .data[[param]], # nolint: object_usage_linter.
      y = bf
    )
  ) +
    geom_rect(
      aes(
        xmin = .data[[param]] - (precision / 2L),
        xmax = .data[[param]] + (precision / 2L),
        ymin = 0L,
        ymax = height,
        fill = .data[["support"]]
      ),
      linewidth = NULL
    ) +
    geom_hline(yintercept = 1L / cutoff, alpha = 1L, linetype = 2L) +
    geom_hline(yintercept = cutoff, alpha = 1L, linetype = 2L) +
    geom_line(linewidth = 1L) +
    geom_point(
      aes(x = base_param, y = base_bf),
      show.legend = FALSE, size = 3L
    ) +
    scale_fill_manual(
      name = NULL,
      values = c(
        "Evidence for H1" = color_palette[["H1"]],
        Inconclusive = color_palette[["In"]],
        "Evidence for H0" = color_palette[["H0"]]
      )
    ) +
    expand_limits(y = 0L) +
    NULL
}

robustness_plot_two <- function(x) {
  data <- get_data(x)

  param <- get_required_param(x)

  set_params <- get_set_params(x)

  base_param <- if (length(param) > 1L) {
    set_params[param]
  } else {
    set_params[[param]]
  }


  param1 <- param[[1L]]
  param2 <- param[[2L]]

  ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = .data[[param1]], # nolint: object_usage_linter.
      y = .data[[param2]],
      fill = .data[["support"]]
    )
  ) +
    geom_raster(interpolate = FALSE) +
    geom_point(
      aes(x = base_param[[1L]], y = base_param[[2L]]),
      show.legend = FALSE
    ) +
    scale_fill_manual(
      name = NULL,
      values = c(
        "Evidence for H1" = color_palette[["H1"]],
        Inconclusive = color_palette[["In"]],
        "Evidence for H0" = color_palette[["H0"]]
      )
    ) +
    expand_limits(y = 0L) +
    NULL
}
