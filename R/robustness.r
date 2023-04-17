make_bf_rr_func <- function(data_model,
                            alternative_prior,
                            null_prior, parameters) {
  base_prior_family <- alternative_prior[["family"]]
  base_defined <- names(alternative_prior[["parameters"]])
  instance_defined <- names(parameters)
  base_defined <- alternative_prior[["parameters"]][
    !(names(alternative_prior[["parameters"]]) %in% instance_defined)
  ]
  null_value <- integral(data_model * null_prior)
  bf_func <- function(params) {
    prior <- c(list(family = base_prior_family), c(base_defined, params)) |>
      do.call("prior", args = _)
    integral(data_model * prior) / null_value
  }
  bf_func
}



makes_values_list <- function(parameters, precision) {
  values <- lapply(parameters, function(x) {
    do.call("seq", as.list(c(x, precision)))
  })

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

  expand.grid(values)
}


bfrr <- function(data_model,
                 alternative_prior,
                 null_prior,
                 parameters,
                 precision,
                 cutoff,
                 multicore) {
  bf_func <- make_bf_rr_func(
    data_model,
    alternative_prior,
    null_prior,
    parameters
  )


  values_df <- makes_values_list(parameters, precision)

  values_list <- unname(split(values_df, ~ row.names(values_df)))

  if (multicore) {
    suppressMessages({
      bfs <- par_map(values_list, \(x) cbind(x, data.frame(bf = bf_func(x)))) |>
        Reduce(f = rbind)
    })
  } else {
    suppressMessages({
      bfs <- map(values_list, \(x) cbind(x, data.frame(bf = bf_func(x)))) |>
        Reduce(f = rbind)
    })
  }


  base_params <- alternative_prior[["parameters"]]
  instance_defined <- names(parameters)
  output_values <- cbind(
    bfs,
    list_to_df(base_params[!(names(base_params) %in% names(parameters))])
  )



  get_support <- Vectorize(\(x) {
    if (x <= 1L / cutoff) {
      return("H0")
    }
    if (x >= cutoff) {
      return("H1")
    }
    "Inc"
  })

  output_values[["support"]] <- get_support(bfs[["bf"]])

  output_values[["support"]] <- factor(output_values[["support"]],
    levels = c("H1", "Inc", "H0"),
    labels = c("Evidence for H1", "Inconclusive", "Evidence for H0")
  )
  list(
    data = output_values,
    required_parameters = instance_defined,
    cutoff = cutoff
  )
}


map <- function(x, f, ...) {
  f <- match.fun(f)
  lapply(x, f, ...)
}

par_map <- function(x, f, ...) {
  f <- match.fun(f)
  parallel::mclapply(x, f, mc.cores = parallel::detectCores(), ...)
}

list_to_df <- function(x) {
  as.data.frame(t(x))
}
