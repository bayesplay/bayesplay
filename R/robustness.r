## TODO: # nolint: todo_comment_linter.
## - define robust region object
## - define summary and display for this
## - define plot function for this


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

#
# TODO: I think when varying 2 or more params there should be a function that # nolint: todo_comment_linter, line_length_linter.
# generates values list for each param while the others are held constant
# for the RR it should also try and find the boundaries between changes of
# support
# Binary search is probably the best way to find these boundaries
#

makes_values_list <- function(parameters, precision, original_values) {
  raw_values <- lapply(parameters, function(x) {
    do.call("seq", as.list(c(x, precision)))
  })


  values <- lapply(names(raw_values), function(x) {
    unique(sort(c(raw_values[[x]], original_values[[x]])))
  }) |> setNames(names(raw_values))


  if ("sd" %in% names(parameters)) {
    values[["sd"]] <- values[["sd"]][values[["sd"]] > 0L]
  }

  if ("df" %in% names(parameters)) {
    values[["df"]] <- values[["df"]][values[["df"]] > 0L]
  }


  n_values <- prod(unlist(map(values, length)))
  if (n_values > 10000L) {
    warning("Number of values to evaluate is large. Consider using a smaller
range.")
  }

  return_values <- expand.grid(values, KEEP.OUT.ATTRS = FALSE)
  # NOTE: Is sorter necessary
  # sorter(return_values) # nolint: commented_code_linter.
  return_values
}

sorter <- function(df) {
  df[["dummy"]] <- 0L
  sorted_df <- df[do.call("order", df |> as.list()), ]
  sorted_df[["dummy"]] <- NULL
  row.names(sorted_df) <- NULL
  return(sorted_df)
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
#' @param parameters \code{list} of min and max values for the parameters to
#' vary
#' @param precision \code{numeric} step size for varied parameter range
#' (Default: precision is set so that each parameter is varied in 100 steps)
#' @param cutoff Minimum Bayes factor value for evidence for the hypothesis
#' @param multicore Run robustness analysis across multiple cores
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
#' # vary the parameters in steps of 0.5
#' precision <- 0.5
#'
#' # mark all Bayes factors larger/smaller than 3/.3 as evidence for the
#' # alternative / null
#' cutoff <- 3
#' bfrr(data_model, alternative_prior, null_prior, parameters, precision,
#'   cutoff,
#'   multicore = FALSE
#' )
#' @export
bfrr <- function(likelihood,
                 alternative_prior,
                 null_prior,
                 parameters,
                 precision = NULL,
                 cutoff = 3L,
                 multicore = FALSE) {
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
    precision = precision,
    cutoff = cutoff
  )

  bf_value <- integral(likelihood * alternative_prior) /
    integral(likelihood * null_prior)


  original_values <- alternative_prior |>
    slot("parameters")

  values_df <- makes_values_list(parameters, precision, original_values)


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

# TODO: This is not correct!  #nolint
describe_robustness <- function(data) {
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
    map(\(x)
    paste0(
      "  The ", x, " was varied from ", parameters[[x]][[1L]],
      " to ", parameters[[x]][[2L]],
      " (step size: ", data[["input_values"]][["precision"]], ")"
    )) |>
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


  # nolint start: commented_code_linter.
  # h0_text <- support_values[["Evidence for H1"]]
  # inc_text <-support_values[["inconclusive"]]
  # consistent <- support_values[[base_support]]
  # inconsistent <- support_values[[invert_support(base_support)]]
  # inconclusive <- sum(support_values) - consistent - inconsistent
  #
  #
  # consistent_text <- paste0(
  #   consistent, " of ", sum(support_values),
  #   # " (", round(consistent / sum(support_values), 2L), ")",
  #   " checked priors were consistent\nwith the original conclusion\n",
  #   "(drew the same conclusion)."
  # )
  #
  # inconsistent_text <- paste0(
  #   inconsistent, " of ", sum(support_values),
  #   # " (", round(inconsistent / sum(support_values), 2L), ")",
  #   " checked priors were inconsistent\nwith the original conclusion\n",
  #   "(drew the opposite conclusion)."
  # )
  #
  #
  #
  # inconclusive_text <- paste0(
  #   inconclusive, " of ", sum(support_values),
  #   # " (", round(inconclusive / sum(support_values), 2L), ")",
  #   " checked priors were inconclusive.\n",
  #   "(did not find support for H1 or H0)."
  # )

  # nolint end

  base_text <- switch(as.character(base_support),
    `Inconclusive` = "is inconclusive.",
    `Evidence for H0` = "provides evidence for H0.",
    `Evidence for H1` = "provides evidence for H1."
  )


  paste0(
    "Prior robustness analysis:\n\n  ",
    alternative_prior_desc, "\n\n  ",
    null_prior_desc, "\n\n  ",
    likelihood_desc, "\n\n ",
    "The original parameters gave a Bayes factor of ", round(bf_base, 2L),
    ".\nUsing the cutoff value of ", cutoff, ", this result ", base_text,
    "\n\n", n_varied, n_varied_text, "varied for the robutness analysis:\n",
    varied_text, "\n\n",
    "Outcome\n-----------------\n",
    h0_text, "\n\n",
    h1_text, "\n\n",
    inc_text, "\n\n"
  )
}



support_factor <- function(x) {
  factor(x,
    levels = c("Evidence for H0", "Inconclusive", "Evidence for H1")
  )
}


get_support <- Vectorize(function(x, cutoff) {
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
    1L
  ) * -1L
  switch(as.character(support_value),
    "-1" = "H0",
    "0" = "Inc",
    "1" = "H1"
  ) |>
    factor(
      levels = c("H0", "Inc", "H1"),
      labels = c("Evidence for H0", "Inconclusive", "Evidence for H1")
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
    cat(object[["desc"]])
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

get_precision <- function(x) get_l2(x, "input_values", "precision")
get_cutoff <- function(x) get_l2(x, "input_values", "cutoff")

color_palette <- c(
  "H1" = "#2a9d8f",
  "In" = "#e9c46a",
  "H0" = "#e76f51"
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
        ymax = max(bf),
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
        "Inconclusive" = color_palette[["In"]],
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


  ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = .data[[param[[1L]]]], # nolint: object_usage_linter.
      y = .data[[param[[2L]]]],
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
        "Inconclusive" = color_palette[["In"]],
        "Evidence for H0" = color_palette[["H0"]]
      )
    ) +
    expand_limits(y = 0L) +
    NULL
}
