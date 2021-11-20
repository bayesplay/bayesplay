describe_likelihood <- function(family, parameters) {
  parameter_names <- names(parameters)
  parameter_values <- unname(parameters)
  return(paste0(
    "Likelihood\n",
    "  Family\n    ", class(family), "\n",
    "  Parameters\n",
    paste0("    ", parameter_names, ": ",
      parameter_values,
      collapse = "\n"
    ), "\n"
  ))
}

get_function <- function(x) x@fun
get_family <- function(x) x@family
get_default_range <- function(x) x@default_range

make_likelihood_data <- function(family, params, func) {
  list(
    family = get_family(family),
    parameters = as.data.frame(params),
    likelihood_function = func
  )
}



dt_scaled <- function(x, df, mean = 0, sd = 1, ncp = 0) {
  stats::dt((x - mean) / sd, df, ncp = ncp, log = FALSE) / sd
}

d_variance <- function(d, df) {
  (df + df + 2) / ((df + 1) * (df + 1)) + ((d * d) / (2 * (df + df + 2)))
}

d2_variance <- function(d, n1, n2) {
  (n1 + n2) / ((n1) * (n2)) + ((d * d) / (2 * (n1 + n2)))
}

get_plot_range <- function(family) {
  w <- 4 # width multipler


  if (class(family) %in% c("binomial", "beta")) {
    return(function(params) {
      c(0, 1)
    })
  }

  if (class(family) %in% c("normal", "student_t")) {
    return(function(params) {
      location <- params$mean
      width <- w * params$sd
      c(location - width, location + width)
    })
  }

  if (class(family) == "noncentral_d") {
    return(function(params) {
      location <- params$d
      width <- w * sqrt(d_variance(params$d, params$n - 1))
      c(location - width, location + width)
    })
  }

  if (class(family) == "noncentral_t") {
    return(function(params) {
      location <- params$t
      d <- location * sqrt(params$df + 1)
      width <- w * sqrt(d_variance(d, params$df))
      c(location - width, location + width)
    })
  }

  if (class(family) == "noncentral_d2") {
    return(function(params) {
      location <- params$d
      width <- w * sqrt(d2_variance(params$d, params$n1, params$n2))
      c(location - width, location + width)
    })
  }

  if (class(family) == "point") {
    return(function(params) {
      location <- params$point
      width <- w
      c(location - width, location + width)
    })
  }


  if (class(family) == "uniform") {
    return(function(params) {
      c(
        params$min - abs(params$min - params$max),
        params$max + abs(params$min - params$max)
      )
    })
  }

  if (class(family) == "cauchy") {
    return(function(params) {
      location <- params$location
      width <- params$scale * w
      c(location - width, location + width)
    })
  }
}


#################################################################
##                 DEFINITIONS OF THE LIKELIHOODS              ##
#################################################################


#' Specify a likelihood
#' @description Define likelihoods using different different distribution families #nolint
#' @param family the likelihood distribution (see details)
#' @param ... see details
#'
#' @details
#' ## Available distribution families
#' The following distribution families can be used for the likelihood
#' * \code{normal} a normal distribution
#' * \code{student_t} a scaled and shifted t-distribution
#' * \code{noncentral_t} a noncentral t (for t statistic)
#' * \code{noncentral_d} a noncentral t (for one sample d)
#' * \code{noncentral_d2} a noncentral t (for independent samples d)
#' * \code{binomial} a binomial distribution
#' The parameters that need to be specified will be dependent on the
#' family
#' ## normal distribution
#' When \code{family} is set to \code{normal} then the following
#' parameters must be set
#' * \code{mean} mean of the normal likelihood
#' * \code{sd} standard deviation of the normal likelihood
#'
#' ## student_t distribution
#' When \code{family} is set to \code{student_t} then the following
#' parameters may be set
#' * \code{mean} mean of the scaled and shifted t likelihood
#' * \code{sd} standard deviation of the scaled and shifted t likelihood
#' * \code{df} degrees of freedom
#'
#' ## noncentral_t distribution
#' When \code{family} is set to \code{noncentral_t} then the following
#' parameters may be set
#' * \code{t} the t value of the data
#' * \code{df} degrees of freedom
#'
#' ## noncentral_d distribution
#' When \code{family} is set to \code{noncentral_d} then the following
#' parameters may be set
#' * \code{d} the d (mean / sd) value of the data
#' * \code{n} the sample size
#'
#' ## noncentral_d2 distribution
#' When \code{family} is set to \code{noncentral_d2} then the following
#' parameters may be set
#' * \code{d} the d (mean / s_pooled) value of the data
#' * \code{n1} the sample size of group 1
#' * \code{n2} the sample size of group 2
#'
#' \eqn{s_{\mathrm{pooled}}}{s_pooled} is set as below:
#' \deqn{s_{\mathrm{pooled}} = \sqrt{\frac{(n_1 - 1)s^2_1 + (n_2 - 1)s^2_2 }{n_1 + n_2 - 2}}}{\sqrt(((n1 - 1) * s1^2 + (n2 - 1)*s2^2)/(n1 + n2 - 2))}
#'
#'
#' ## binomial distribution
#' When the \code{family} is set to \code{binomial} then the following
#' parameters may be set
#' * \code{successes} the number of successes
#' * \code{trials} the number of trials
#'
#'
#' @md
#' @return an object of class \code{likelihood}
#' @export
#'
#' @examples
#' # specify a normal likelihood
#' likelihood(family = "normal", mean = 5.5, sd = 32.35)
#'
#' # specify a scaled and shifted t likelihood
#' likelihood(family = "student_t", mean = 5.5, sd = 32.35, df = 10)
#'
#' # specify non-central t likelihood (t scaled)
#' likelihood(family = "noncentral_t", t = 10, df = 10)
#'
#' # specify non-central t likelihood (d scaled)
#' likelihood(family = "noncentral_d", d = 10, n = 10)
#'
#' # specify non-central t likelihood (independent samples d scaled)
#' likelihood(family = "noncentral_d2", d = 10, n1 = 10, n2 = 12)
#'
#' # specify a binomial likelihood
#' likelihood(family = "binomial", successes = 2, trials = 10)
likelihood <- function(family, ...) {
  if (!methods::existsMethod(signature = family, f = "make_likelihood")) {
    stop(family, " is not a valid distribution family")
  }
  make_likelihood(family = new(family), ...)
}




setGeneric("make_likelihood",
  function(family, ...) standardGeneric("make_likelihood"),
  signature = "family"
)




setMethod(
  "make_likelihood",
  signature(family = "normal"),
  function(family, mean, sd) {
    make_likelihood.normal(family, mean, sd)
  }
)


setMethod(
  "make_likelihood",
  signature(family = "student_t"),
  function(family, mean, sd, df) {
    make_likelihood.student_t(family, mean, sd, df)
  }
)

setMethod(
  "make_likelihood",
  signature(family = "noncentral_t"),
  function(family, t, df) {
    make_likelihood.noncentral_t(family, t, df)
  }
)


setMethod(
  "make_likelihood",
  signature(family = "noncentral_d"),
  function(family, d, n) {
    make_likelihood.noncentral_d(family, d, n)
  }
)

setMethod(
  "make_likelihood",
  signature(family = "noncentral_d2"),
  function(family, d, n1, n2) {
    make_likelihood.noncentral_d2(family, d, n1, n2)
  }
)

setMethod(
  "make_likelihood",
  signature(family = "binomial"),
  function(family, successes, trials) {
    make_likelihood.binomial(family, successes, trials)
  }
)



#' @method likelihood normal
#' @usage likelihood(family = "normal", mean, sd)
#' @param mean the sample mean
#' @param sd the standard error of the mean
#' @noRd
make_likelihood.normal <- function(family, mean, sd) { # nolint
  if (missing(mean) | missing(sd)) {
    stop("You must specify a `mean` and `sd` for a normal likelihood",
      call. = FALSE
    )
  }

  if (sd <= 0) {
    stop("`sd` must be greater than 0")
  }

  params <- list(mean = mean, sd = sd)


  func <- function(x) get_function(family)(x = x, mean = mean, sd = sd)



  data <- make_likelihood_data(family, params, func)

  desc <- describe_likelihood(family, params)
  new(
    Class = "likelihood",
    func = func,
    data = data,
    marginal = paste0(
      "likelihood(family = \"normal\", mean = x, sd = ",
      sd, ")"
    ),
    observation = params$mean,
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = get_plot_range(family)(params),
      labs = likelihood_labs
    )
  )
}


#' @method likelihood student_t
#' @usage likelihood(family = "student_t", mean, sd, df)
#' @param mean the sample mean
#' @param sd the standard error of the mean
#' @param df the degrees of freedom
#' @noRd
make_likelihood.student_t <- function(family, mean, sd, df) { # nolint

  if (missing(mean) | missing(sd) | missing(df)) {
    stop("You must specify a `mean`, `sd`, and `df` for a student t likelihood",
      call. = FALSE
    )
  }

  if (sd <= 0) {
    stop("`sd` must be greater than 0")
  }


  if (df <= 0) {
    stop("`df` must be greater than 0")
  }

  params <- list(mean = mean, sd = sd, df = df)


  # calculate the plot defaults


  func <- function(x) get_function(family)(x = x, mean = mean, sd = sd, df = df)

  data <- make_likelihood_data(family = family, params = params, func = func)
  desc <- describe_likelihood(family, params)

  new(
    Class = "likelihood",
    data = data,
    func = func,
    marginal = paste0(
      "likelihood(family = \"student_t\", mean = x, sd = ",
      sd, ", df = ", df, ")"
    ),
    observation = params$mean,
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = get_plot_range(family)(params),
      labs = likelihood_labs
    )
  )
}


#' @method likelihood noncentral_d
#' @usage likelihood(family = "noncentral_d", d, n)
#' @param d Cohen's d for a one sample difference
#' @param n sample size
#' @noRd
make_likelihood.noncentral_d <- function(family, d, n) { # nolint
  if (missing(d) | missing(n)) {
    stop("You must specify a `d` and `n` for a noncentral d likelihood",
      call. = FALSE
    )
  }

  if (n <= 0) {
    stop("`n` must be greater than zero",
      call. = FALSE
    )
  }


  params <- list(d = d, n = n)
  func <- function(x) get_function(family)(x = x, d = d, n = n)
  data <- make_likelihood_data(family = family, params = params, func = func)
  desc <- describe_likelihood(family, params)

  new(
    Class = "likelihood",
    data = data,
    func = func,
    marginal = paste0(
      "likelihood(family = \"noncentral_d\", d = x, n = ",
      n, ")"
    ),
    observation = params$d,
    desc = desc, dist_type = "continuous",
    plot = list(
      range = get_plot_range(family)(params),
      labs = likelihood_labs
    )
  )
}

#' @method likelihood noncentral_t
#' @usage likelihood(family = "noncentral_t", t, df)
#' @param t t statistic
#' @param df degrees of freedom
#' @noRd
make_likelihood.noncentral_t <- function(family, t, df) { # nolint

  if (missing(t) | missing(df)) {
    stop("You must specify a `t` and `df` for a noncentral t likelihood",
      call. = FALSE
    )
  }

  if (df <= 0) {
    stop("`df` must be greater than 0",
      call. = FALSE
    )
  }

  params <- list(t = t, df = df)
  func <- function(x) get_function(family)(x = x, t = t, df = df)
  data <- make_likelihood_data(family = family, params = params, func = func)
  desc <- describe_likelihood(family, params)

  new(
    Class = "likelihood",
    data = data,
    func = func,
    marginal = paste0(
      "likelihood(family = \"noncentral_t\", t = x, df = ",
      df, ")"
    ),
    observation = params$t,
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = get_plot_range(family)(params),
      labs = likelihood_labs
    )
  )
}


#' @method likelihood binomial
#' @usage likelihood(family = "binomial", successes, trials)
#' @param successes number of successes
#' @param trials number of trials
#'
#' @noRd
make_likelihood.binomial <- function(family, successes, trials) { # nolint

  if (missing(trials) | missing(successes)) {
    stop("You must specify `successes` and `trials` for a binomial likelihood",
      call. = FALSE
    )
  }


  if (trials <= 0) {
    stop("`trials` must be greater than or equal to 1", call. = FALSE)
  }

  if (successes > trials) {
    stop("`trials` must be greater than or equal to `successes`", call. = FALSE)
  }


  if (successes < 0) {
    stop("`successes` must be greater than or equal to 0", call. = FALSE)
  }

  params <- list(successes = successes, trials = trials)
  func <- function(p) {
    get_function(family)(p = p,
      successes = successes,
      trials = trials)
  }

  data <- make_likelihood_data(family = family, params = params, func = func)
  desc <- describe_likelihood(family, params)

  new(
    Class = "likelihood",
    func = func,
    data = data,
    marginal = paste0(
      "likelihood(family = \"binomial\", successes = x, trials = ", trials, ")"
    ),
    observation = params$successes,
    desc = desc,
    dist_type = "continuous",
    plot = list(
      range = get_plot_range(family)(params),
      labs = likelihood_labs
    )
  )
}

#' @method likelihood noncentral_d2
#' @usage likelihood(family = "noncentral_d2", d, n1, n2)
#' @param d Cohen's d for a independent samples design
#' @param n1 sample size of group 1
#' @param n2 sample size of group 2
#' @noRd
make_likelihood.noncentral_d2 <- function(family, d, n1, n2) { # nolint

  if (missing(d) | missing(n1) | missing(n2)) {
    stop("You must specify `d`, `n1`, and `n2` for a noncentral d2 likelihood",
      call. = FALSE
    )
  }


  if (n1 <= 0 | n2 <= 0) {
    stop("`n1` and `n2` must be greater than or equal to 1",
      call. = FALSE
    )
  }

  params <- list(d = d, n1 = n1, n2 = n2)
  func <- function(x) get_function(family)(x = x, d = d, n1 = n1, n2 = n2)
  data <- make_likelihood_data(family = family, params = params, func = func)
  desc <- describe_likelihood(family, params)

  new(
    Class = "likelihood",
    data = data,
    func = func,
    marginal = paste0(
      "likelihood(family = \"noncentral_d2\", d = x,  n1 = ",
      n1, ", n2 = ", n2, ")"
    ),
    observation = params$d,
    desc = desc, dist_type = "continuous",
    plot = list(
      range = get_plot_range(family)(params),
      labs = likelihood_labs
    )
  )
}
