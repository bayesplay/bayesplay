is_empty <- function(x) {
  length(x) == 0
}

`%||%` <- function(x, y) { # nolint
  if (is_empty(x)) y else x
}

in_range <- function(x, range) {
  min_value <- range[1]
  max_value <- range[2]

  if (all(c(x >= min_value, x <= max_value))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' @importFrom methods new
#' @importFrom stats qnorm sd integrate


#' @export
`*.bayesplay` <- function(e1, e2) {
  if ("likelihood" %in% class(e1)) {
    likelihood <- e1
    prior <- e2
  } else {
    likelihood <- e2
    prior <- e1
  }

  theta_range <- prior@theta_range


  likelihood_func <- likelihood@func
  prior_func <- prior@func

  product_function <- function(x) likelihood_func(x) * prior_func(x)

  # for priors that non-point points
  if (theta_range[1] != theta_range[2]) {
    marginal_likelihood <- suppressWarnings(stats::integrate(
      Vectorize(product_function),
      theta_range[1],
      theta_range[2]
    )$value)
  }


  # for point priors
  if (theta_range[1] == theta_range[2]) {
    marginal_likelihood <- product_function(theta_range[1])
  }

  make_predict <- function(data_model, prior_model) {
    marginal_func <- function(x) eval(parse(text = data_model@marginal))
    predictive_func <- function(x) integral(marginal_func(x) * prior_model)
    return(Vectorize(predictive_func))
  }

  posterior_func <- function(x) product_function(x) / marginal_likelihood

  evidence_func <- function(x) posterior_func(x) / prior_func(x)

  marginal_func <- function(x) likelihood_func(x) / evidence_func(x)

  conditional_func <- function(x) evidence_func(x) * product_function(x)

  prediction_function <- make_predict(likelihood, prior)


  data <- list(
    integral = marginal_likelihood,
    marginal_function = marginal_func,
    evidence_function = evidence_func,
    posterior_function = posterior_func,
    conditional_function = conditional_func,
    weighted_likelihood_function = product_function,
    prediction_function = prediction_function
  )

  desc <- paste0(
    "Product\n",
    "  Likelihood family: ", likelihood$family, "\n",
    "  Prior family: ", prior$family, "\n",
    "  Area under curve: ", round(data$integral, 4)
  )


  new(
    Class = "product",
    desc = desc,
    data = data,
    K = 1,
    lik = likelihood_func,
    prior = prior_func,
    theta_range = theta_range,
    likelihood_obj = likelihood,
    prior_obj = prior
  )
}


`/.product` <- function(e1, e2) {
  bf <- e1@data$integral / e2@data$integral
  return(bf)
}

setOldClass("numeric")

auc <- setClass("auc", contains = "numeric")
bf <- setClass("bf", contains = "numeric")



#' Compute integral
#'
#' Computes the definite integral of a \code{product} object over the range
#' of the parameter
#'
#' @param obj a \code{product} object
#'
#' @return A numeric of the marginal likelihood
#' @export
#'
#' @examples
#' # define a likelihood
#' data_model <- likelihood(family = "normal", mean = 5.5, sd = 32.35)
#'
#' # define a prior
#' prior_model <- prior(family = "normal", mean = 5.5, sd = 13.3)
#'
#' # multiply the likelihood by the prior
#' model <- data_model * prior_model
#'
#' # take the integral
#' integral(model)
integral <- function(obj) {
  if (class(obj) != "product") {
    stop("obj must be of class product", call. = FALSE)
  }
  new("auc", obj$integral)
}

#' @export
`/.auc` <- function(e1, e2) {
  new("bf", unclass(e1) / unclass(e2))
}

get_ev_level <- function(bf) {
  if (bf == 1) {
    return("No evidence")
  }
  if (bf > 1 & bf <= 3) {
    return("Anecdotal evidence")
  }
  if (bf > 3 & bf <= 10) {
    return("Moderate evidence")
  }
  if (bf > 10 & bf <= 30) {
    return("Strong evidence")
  }
  if (bf > 30 & bf <= 100) {
    return("Very strong evidence")
  }
  if (bf > 100) {
    return("Extreme evidence")
  }
}


bfsay <- function(bf) {
  bf <- unclass(bf)
  if (bf < 1) {
    bf_base <- bf
    bf <- 1 / bf
  } else {
    bf_base <- bf
  }

  ev_level <- get_ev_level(bf)

  ev_level_msg <- c(
  paste0("Using the levels from Wagenmakers et al (2017)\n"),
  paste0("A BF of ", round(bf_base, 4), " indicates:\n"),
  paste0(ev_level))

}

#' Summarise a Bayes factor
#' @description Provide a verbal summary of a Bayes factor and the level of evidence
#' @param object a \code{bf} object
#' @export
#' @return No return, called for side effects
setMethod(
  "summary",
  "bf",
  function(object) {
    cat("Bayes factor\n")
    cat(bfsay(object), "\n")
  }
)

setMethod(
  "show",
  "bf",
  function(object) {
    cat(object, "\n")
  }
)

setMethod(
  "show",
  "auc",
  function(object) {
    cat(object, "\n")
  }
)



integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


#' Compute the Savage-Dickey density ratio
#'
#' Computes the Saveage-Dickey density ratio from a \code{posterior} object
#' at a specified point
#'
#' @param x a \code{posterior} object
#' @param point the point at which to evaluate the Savage-Dickey ratio
#'
#' @return A numeric of the Savage-Dickey density ratio
#' @export
#'
#' @examples
#' # define a likelihood
#' data_model <- likelihood(family = "normal", mean = 5.5, sd = 32.35)
#'
#' # define a prior
#' prior_model <- prior(family = "normal", mean = 5.5, sd = 13.3)
#'
#' model <- extract_posterior(data_model * prior_model)
#'
#' # compute the Savage-Dickey density ratio at 0
#' sd_ratio(model, 0)
#' @export
sd_ratio <- function(x, point) {
  bf <- x@prior_obj@func(point) / x$posterior_function(point)
  new("bf", bf)
}
