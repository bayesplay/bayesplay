
make_prior_data <- function(family, params, func) {
  list(
    family = get_family(family),
    parameters = as.data.frame(params),
    prior_function = func
  )
}

describe_prior <- function(family, parameters) {
  range <- parameters[["range"]]
  parameters <- parameters[!grepl("range", names(parameters))]
  range_text <- "\n"
  if (is.null(range)) {
    range_text <- paste0("\n    Range: ", range_as_text(range), "\n")
  }

  parameter_names <- names(parameters)
  parameter_values <- unname(parameters)
  return(paste0(
    "Prior\n",
    "  Family\n    ", class(family), "\n",
    "  Parameters\n",
    paste0("    ", parameter_names, ": ", parameter_values, collapse = "\n"),
    range_text
  ))
}

range_as_text <- function(range) {
  paste0(range[1], " to ", range[2])
}

prior_data_names <- c("family", "parameters", "prior_function")

#################################################################
##                 DEFINITIONS OF THE PRIORS                   ##
#################################################################

# class definition


#' Specify a prior
#' @description Define priors using different different distribution families
#' @param family the prior distribution (see details)
#' @param ... see details
#'
#' @details
#' ## Available distribution families
#' The following distributions families can be used for the prior
#' * \code{normal} a normal distribution
#' * \code{uniform} a uniform distribution
#' * \code{point} a point
#' The parameters that need to be specified will be dependent on the
#' family
#' ## normal distribution
#' When \code{family} is set to \code{normal} then the following
#' parameters must be set
#' * \code{mean} mean of the normal prior
#' * \code{sd} standard deviation of the normal prior
#' * \code{range} (optional) a vector specifying a paramter range
#' ## uniform distribution
#' When \code{family} is set to \code{uniform} then the following
#' parameters must be set
#' * \code{min} the lower bound
#' * \code{max} the upper bound
#' ## point
#' When \code{family} is set to \code{point} then the following
#' parameters may be set
#' * \code{point} the location of the point prior (default: 0)
#' @md
#' @return an object of class \code{prior}
#' @export
#'
#' @examples
#' # specify a half-normal (range 0 to Infinity) prior
#' prior(family = "normal", mean = 0, sd = 13.3, range = c(0, Inf))
#'
#' # specify a normal prior
#' prior(family = "normal", mean = 0, sd = 13.3)
#'
#' # specify a uniform prior
#' prior(family = "uniform", min = 0, max = 20)
#'
#' # specify a point prior
#' prior(family = "point", point = 0)
prior <- function(family, ...) {
  if (!methods::existsMethod(signature = family, f = "make_prior")) {
    stop(family, " is not a valid distribution family")
  }
  make_prior(family = new(family), ...)
}


setGeneric("make_prior",
  function(family, ...) standardGeneric("make_prior"),
  signature = "family",
)



setMethod(
  "make_prior",
  signature(family = "normal"),
  function(family, mean, sd, ...) {
    make_prior.normal(family, mean, sd, ...)
  }
)

setMethod(
  "make_prior",
  signature(family = "point"),
  function(family, point = 0) {
    if (missing(point)) {
      warning("Point value is missing. Assuming 0", call. = FALSE)
    }
    make_prior.point(family, point)
  }
)


setMethod(
  "make_prior",
  signature(family = "uniform"),
  function(family, min, max) {
    make_prior.uniform(family, min, max)
  }
)

setMethod(
  "make_prior",
  signature(family = "student_t"),
  function(family, mean, sd, df, ...) {
    make_prior.student_t(family, mean, sd, df, ...)
  }
)


setMethod(
  "make_prior",
  signature(family = "cauchy"),
  function(family, location = 0, scale, ...) {
    make_prior.cauchy(family, location, scale, ...)
  }
)


setMethod(
  "make_prior",
  signature(family = "beta"),
  function(family, alpha, beta, ...) {
    make_prior.beta(family, alpha, beta, ...)
  }
)

truncate_normalise <- function(family, range, ...) {
  unnormalised <- function(x) get_function(family)(x = x, ...) # nolint

  truncated_function <- function(x) {
    ifelse(in_range(x, range),
      unnormalised(x),
      0
    )
  }

  constant <- 1 / integrate(
    Vectorize(truncated_function),
    range[1],
    range[2]
  )$value

  normalised <- function(x) truncated_function(x) * constant
  return(normalised)
}


#' @method prior normal
#' @usage prior(family = "normal", mean, sd, range)
#' @noRd
make_prior.normal <- function(family, mean, sd, range = NULL) { # nolint
  if (missing(mean) | missing(sd)) {
    stop("You must specify `mean` and `sd` for a normal prior", call. = FALSE)
  }

  if (sd <= 0) {
    stop("`sd` must be greater than 0")
  }

  if (missing(range)) {
    range <- get_default_range(family)
  }


  params <- list(mean = mean, sd = sd, range = range)

  func <- truncate_normalise(
    family = family,
    range = range,
    mean = mean,
    sd = sd
  )

  desc <- describe_prior(family, params)
  data <- make_prior_data(family, params, func)

  new(
    Class = "prior",
    data = data,
    theta_range = params$range,
    type = "normal",
    func = func,
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = get_plot_range(family)(params),
      labs = list(x = "\u03F4", y = "P(\u03F4)")
    ),
    parameters = list(mean = mean, sd = sd),
    function_text = paste0("prior(\"normal\", mean = ", mean, ", sd =", sd, ")")
  )
}

#' @method prior point
#' @usage prior(family = "point", point)
#' @noRd
make_prior.point <- function(family, point = 0) {
  func <- function(x) get_function(family)(x = x, point = point)
  width <- 4
  range <- c(point - width, point + width)
  params <- list(point = point)
  # func <- make_distribution("point", list(point = point)) # nolint
  desc <- describe_prior(family, params)

  data <- make_prior_data(family, params, func)
  new(
    Class = "prior",
    data = data,
    theta_range = c(point, point),
    func = func,
    type = "point",
    dist_type = "point",
    plot = list(
      range = get_plot_range(family)(params),
      labs = list(x = "\u03F4", y = "P(\u03F4)")
    ),
    parameters = list(point = point),
    function_text = paste0("prior(\"point\", point = ", point, ")"),
    desc = desc
  )
}



#' @method prior uniform
#' @usage prior(family = "uniform", min, max)
#' @noRd
make_prior.uniform <- function(family, min, max) {
  if (missing(min) | missing(max)) {
    stop("You must specify `min` and `max` for a uniform  prior", call. = FALSE)
  }


  func <- function(x) get_function(family)(x = x, min = min, max = max)
  params <- list(min = min, max = max)

  desc <- describe_prior(family, params)

  data <- make_prior_data(family, params, func)

  new(
    Class = "prior",
    data = data,
    theta_range = c(min, max),
    func = func,
    type = "normal",
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = get_plot_range(family)(params),
      labs = list(x = "\u03F4", y = "P(\u03F4)")
    ),
    parameters = list(mean = mean, sd = sd),
    function_text = paste0(
      "prior(\"uniform\", min = ",
      min, ", max =", max, ")"
    )
  )
}



#' @method prior student_t
#' @usage prior(family = "student_t", mean, sd, df, range)
#' @noRd
make_prior.student_t <- function(family, mean, sd, df, range = NULL) {
  if (missing(mean) | missing(sd) | missing(df)) {
    stop("You must specify `mean`, `sd`, and `df` for a student_t prior",
      call. = FALSE
    )
  }

  if (missing(range)) {
    range <- get_default_range(family)
  }

  func <- truncate_normalise(family = family, range = range, mean = mean, sd = sd, df = df)




  params <- list(mean = mean, sd = sd, df = df, range = range)

  desc <- describe_prior(family, params)

  data <- make_prior_data(family, params, func)

  new(
    Class = "prior",
    data = data,
    theta_range = range,
    func = func,
    type = "normal",
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = get_plot_range(family)(params),
      labs = list(x = "\u03F4", y = "P(\u03F4)")
    ),
    parameters = list(mean = mean, sd = sd),
    function_text = paste0("prior(\"normal\", mean = ", mean, ", sd =", sd, ")")
  )
}

#' @method prior cauchy
#' @usage prior(family = "cauchy", location, scale, range)
#' @noRd
make_prior.cauchy <- function(family, location = 0, scale, range = NULL) {
  if (missing(range)) {
    range <- get_default_range(family)
  }

  func <- truncate_normalise(family = family, range = range, location = location, scale = scale)


  params <- list(location = location, scale = scale, range = range)
  desc <- describe_prior(family, params)

  data <- make_prior_data(family, params, func)



  new(
    Class = "prior",
    data = data,
    theta_range = params$range,
    func = func,
    type = "normal",
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = get_plot_range(family)(params),
      labs = list(x = "\u03F4", y = "P(\u03F4)")
    ),
    parameters = list(location = location, scale = scale),
    function_text = paste0(
      "prior(\"cauchy\", location = ",
      location, ", scale =",
      scale, ")"
    )
  )
}

#' @method prior beta
#' @usage prior(family = "beta", alpha, beta)
#' @noRd
make_prior.beta <- function(family, alpha, beta, range = NULL) {
  if (missing(alpha) | missing(beta)) {
    stop("You must specify `alpha` and `beta` for a beta  prior", call. = FALSE)
  }

  if (missing(range)) {
    range <- get_default_range(family)
  }


  func <- truncate_normalise(family = family, range = range, beta = beta, alpha = alpha)
  params <- list(alpha = alpha, beta = beta, range = range)

  desc <- describe_prior(family, params)

  data <- make_prior_data(family, params, func)

  new(
    Class = "prior",
    data = data,
    theta_range = params$range,
    func = func,
    type = "normal",
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = get_plot_range(family)(params),
      labs = list(x = "\u03F4", y = "P(\u03F4)")
    ),
    parameters = list(alpha = alpha, beta = beta),
    function_text = paste0(
      "prior(\"beta\", alpha = ",
      alpha, ", beta =", beta, ")"
    )
  )
}
