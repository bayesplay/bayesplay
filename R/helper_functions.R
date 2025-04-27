is_empty <- function(x) {
  length(x) == 0L
}

`%||%` <- function(x, y) { # nolint
  if (is_empty(x)) y else x
}


in_range <- function(x, range) {
  min_value <- range[[1L]]
  max_value <- range[[2L]]

  if (all(c(x >= min_value, x <= max_value))) {
    TRUE
  } else {
    FALSE
  }
}


#' @importFrom methods new
#' @importFrom stats qnorm sd integrate





setOldClass("numeric")

auc <- setClass("auc", contains = "numeric")
bf <- setClass("bf", contains = "numeric")
summary.bf <- setClass("summary.bf", contains = "vector")


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
  if (!inherits(obj, "product")) {
    stop("obj must be of class product", call. = FALSE)
  }
  rv <- new("auc", obj[["integral"]])
  attr(rv, "approximate") <- slot(obj, "approximation")
  ## prior description
  p <- obj@prior_obj
  p_desc <- p[["parameters"]]
  p_desc[["family"]] <- p[["family"]]
  attr(rv, "prior") <- p_desc
  rv
}

#' @export
`/.auc` <- function(e1, e2) {
  is_e1_approx <- is_approx(e1)
  is_e2_approx <- is_approx(e2)

  has_approximation <- is_e1_approx | is_e2_approx

  is_e1_point <- is_point(e1, 0L)
  is_e2_point <- is_point(e2, 0L)


  one_is_point <- is_e1_point | is_e2_point


  # if one of the objects is an approximation
  # then one of the them must also be a point null

  if (has_approximation && !one_is_point) {
    stop("Marginal likelihood is a approximation. One prior must ",
      "be a point prior at 0",
      call. = FALSE
    )
  }

  new("bf", unclass(e1) / unclass(e2))
}

get_ev_level <- function(bf) { # nolint
  if (bf == 1L) {
    return("No evidence")
  }
  if (bf > 1L && bf <= 3L) {
    return("Anecdotal evidence")
  }
  if (bf > 3L && bf <= 10L) {
    return("Moderate evidence")
  }
  if (bf > 10L && bf <= 30L) {
    return("Strong evidence")
  }
  if (bf > 30L && bf <= 100L) {
    return("Very strong evidence")
  }
  if (bf > 100L) {
    "Extreme evidence"
  }
}


bfsay <- function(bf) {
  bf <- unclass(bf)
  if (bf < 1L) {
    bf_base <- bf
    bf <- 1L / bf
  } else {
    bf_base <- bf
  }

  ev_level <- get_ev_level(bf)

  c(
    paste0("Using the levels from Wagenmakers et al (2017)\n"),
    paste0("A BF of ", round(bf_base, 4L), " indicates:\n"),
    paste0(ev_level)
  )
}

#' Summarise a Bayes factor
#' @description Provide a verbal summary of a Bayes factor and
#' the level of evidence
#' @param object a \code{bf} object
#' @export
#' @return No return, called for side effects
setMethod(
  "summary",
  "bf",
  function(object) {
    out <- c(
      "Bayes factor\n",
      bfsay(object), "\n"
    )
    new("summary.bf", out)
  }
)

setMethod(
  "show",
  "summary.bf",
  function(object) {
    cat(object)
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



integer_breaks <- function(n = 5L, ...) {
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
  is_estimated <- x@approximation %||% FALSE
  if (is_estimated && point != 0L) {
    stop("point must be 0 if the marginal likelihood is an approximation",
      call. = FALSE
    )
  } else if (is_estimated && point == 0L) {
    bf <- x@data[["integral"]] / x@likelihood_obj@func(0L)
    return(new("bf", bf))
  }

  bf <- x@prior_obj@func(point) / x[["posterior_function"]](point)
  new("bf", bf)
}




#' @export
`*.bayesplay` <- function(e1, e2) {
  if (inherits(e1, "likelihood")) {
    likelihood <- e1
    prior <- e2
  } else {
    likelihood <- e2
    prior <- e1
  }


  likelihood_family <- likelihood[["family"]]


  bf_approx <- check_approximation(likelihood, prior)
  do_approximation <- bf_approx[["approximation"]]
  if (do_approximation) {
    supported_prior <- bf_approx[["supported_prior"]]
    approximation_params <- bf_approx[c("n", "t", "df")]
  }
  supported_prior <- bf_approx[["supported_prior"]]
  approximation_params <- bf_approx[c("n", "t", "df")]


  if (do_approximation && likelihood_family == "noncentral_t") {
    stop(
      "t value is large; approximation needed
     Reparameterize using a `noncentral_d` of `noncentral_d2` likelihood.",
      call. = FALSE
    )
  }


  if (do_approximation && !supported_prior) {
    warning("Observation is large; approximation needed.", call. = FALSE)
    stop("Approximations are only supported with cauchy priors.", call. = FALSE)
  }

  if (do_approximation) {
    n <- approximation_params[["n"]]
    t_value <- approximation_params[["t"]]
    df_value <- approximation_params[["df"]]
    approximation <- estimate_marginal(n, t_value, df_value, prior)
    marginal_likelihood_approx <- approximation[["marginal"]]





    warning("Observation is large; approximation needed.", call. = FALSE)
  }




  theta_range <- prior@theta_range


  likelihood_func <- likelihood@func
  prior_func <- prior@func

  product_function <- function(x) likelihood_func(x) * prior_func(x)

  # for priors that non-point points
  if (theta_range[[1L]] != theta_range[[2L]]) {
    marginal_likelihood <- suppressWarnings(stats::integrate(
      Vectorize(product_function),
      theta_range[[1L]],
      theta_range[[2L]],
      abs.tol = 1e-09
    )[["value"]])
  }


  # for point priors
  if (theta_range[[1L]] == theta_range[[2L]]) {
    marginal_likelihood <- product_function(theta_range[[1L]])
  }

  make_predict <- function(data_model, prior_model) {
    marginal_func <- function(x) eval(parse(text = data_model@marginal))
    predictive_func <- function(x) integral(marginal_func(x) * prior_model)
    Vectorize(predictive_func)
  }

  posterior_func <- function(x) product_function(x) / marginal_likelihood

  evidence_func <- function(x) posterior_func(x) / prior_func(x)

  marginal_func <- function(x) likelihood_func(x) / evidence_func(x)

  conditional_func <- function(x) evidence_func(x) * product_function(x)

  prediction_function <- make_predict(likelihood, prior)

  if (do_approximation) {
    bf_against_point <- approximation[["bf"]]
  } else {
    bf_against_point <- marginal_likelihood / likelihood_func(0L)
  }

  desc_text <- ""
  if (do_approximation) {
    auc <- marginal_likelihood_approx
    desc_text <- paste0(
      "  Area under curve: ", round(auc, 4L),
      " (approximation)"
    )
  } else {
    auc <- marginal_likelihood
    desc_text <- paste0("  Area under curve: ", round(auc, 4L))
  }

  obj_data <- list(
    integral = auc,
    marginal_function = marginal_func,
    evidence_function = evidence_func,
    posterior_function = posterior_func,
    conditional_function = conditional_func,
    weighted_likelihood_function = product_function,
    prediction_function = prediction_function
  )

  desc <- paste0(
    "Product\n",
    "  Likelihood family: ", likelihood[["family"]], "\n",
    "  Prior family: ", prior[["family"]], "\n",
    desc_text
    # "  Area under curve: ", round(data[["integral"]], 4L)
  )


  new(
    Class = "product",
    desc = desc,
    data = obj_data,
    K = 1L,
    lik = likelihood_func,
    prior = prior_func,
    theta_range = theta_range,
    likelihood_obj = likelihood,
    prior_obj = prior,
    approximation = do_approximation,
    bf_against_point = bf_against_point
  )
}



reparameterise_d_to_t <- function(likelihood_obj) {
  n <- likelihood_obj[["parameters"]][["n"]]
  d <- likelihood_obj[["parameters"]][["d"]]
  t_value <- d * sqrt(n)
  df_value <- n - 1L

  list(n = n, t = t_value, df = df_value)
}

reparameterise_d2_to_t <- function(likelihood_obj) {
  n1 <- likelihood_obj[["parameters"]][["n1"]]
  n2 <- likelihood_obj[["parameters"]][["n2"]]
  d <- likelihood_obj[["parameters"]][["d"]]
  n <- n1 * n2 / (n1 + n2)
  t_value <- d * sqrt(n)
  df_value <- n1 + n2 - 2L
  list(n = n, t = t_value, df = df_value)
}

prior_in_range <- function(prior_obj, likelihood_obj) {
  prior_limits <- prior_obj[["parameters"]][["range"]]
  observation <- likelihood_obj@observation
  observation >= prior_limits[[1L]] && observation <= prior_limits[[2L]]
}


check_approximation <- function(likelihood_obj, prior_obj) {
  prior_family <- prior_obj[["family"]]
  likelihood_family <- likelihood_obj[["family"]]

  likelihods_approx <- c("noncentral_t", "noncentral_d2", "noncentral_d")
  priors_approx <- "cauchy"

  # don't perform approximation if the likelihoods or priors are not supported
  if (!(likelihood_family %in% likelihods_approx)) {
    return(list(approximation = FALSE, is_large = FALSE, not_in_prior = FALSE))
  }

  # don't perform approximation when there is a point prior
  if (prior_family == "point") {
    return(list(approximation = FALSE, is_large = FALSE, not_in_prior = FALSE))
  }

  if (prior_family %in% priors_approx) {
    supported_prior <- TRUE
  } else {
    supported_prior <- FALSE
  }

  if (likelihood_family == "noncentral_t") { # nolint
    t_value <- likelihood_obj[["parameters"]][["t"]]
    df_value <- likelihood_obj[["parameters"]][["df"]]
    parameters <- list(n = NA, t = t_value, df = df_value)
  } else if (likelihood_family == "noncentral_d") {
    parameters <- reparameterise_d_to_t(likelihood_obj)
  } else if (likelihood_family == "noncentral_d2") {
    parameters <- reparameterise_d2_to_t(likelihood_obj)
  }

  more_than_15 <- abs(parameters[["t"]]) > 15L
  more_than_5 <- abs(parameters[["t"]]) > 5L
  in_range <- prior_in_range(prior_obj, likelihood_obj)

  if (more_than_15) {
    approximation <- TRUE
  } else if (more_than_5) {
    if (in_range) {
      approximation <- FALSE
    } else {
      approximation <- TRUE
    }
  } else {
    approximation <- FALSE
  }

  list(
    approximation = approximation,
    supported_prior = supported_prior,
    is_large = more_than_15 | more_than_5,
    not_in_prior = in_range,
    n = parameters[["n"]],
    t = parameters[["t"]],
    df = parameters[["df"]]
  )
}



estimate_marginal <- function(n, t, df, prior) {
  prior_limits <- prior[["parameters"]][["range"]]

  upper <- prior_limits[[2L]]
  lower <- prior_limits[[1L]]
  var_delta <- 1L / n
  delta_est <- t / sqrt(n)
  mean_delta <- (delta_est * n) / (n)
  log_post_probs <- c(
    pt((lower - mean_delta) / sqrt(var_delta), df, log.p = TRUE),
    pt((upper - mean_delta) / sqrt(var_delta), df, log.p = TRUE)
  )

  location <- prior[["parameters"]][["location"]]
  rscale <- prior[["parameters"]][["scale"]] * sqrt(n)
  log_prior_probs <- c(
    pcauchy(lower, location, rscale, TRUE, TRUE),
    pcauchy(upper, location, rscale, TRUE, TRUE)
  )

  prior_interval <- log_prior_probs[[2L]] + pexp(
    diff(log_prior_probs),
    1L, TRUE, TRUE
  )

  post_interval <- log_post_probs[[2L]] + pexp(
    diff(log_post_probs),
    1L, TRUE, TRUE
  )

  log_bf_interval <- post_interval - prior_interval


  new_prior <- prior("cauchy", location, rscale, c(-Inf, Inf))
  new_likelihood <- likelihood("noncentral_t", t, df)

  # The numerical integration can silently fail
  # This is extremely rare, but when it does occur we just need to
  # shift the observation by a small amount and try again








  auc_h1_pass1 <- integrate(
    Vectorize(function(x) new_likelihood@func(x) * new_prior@func(x)),
    -Inf, Inf,
    subdivisions = 1000L, abs.tol = 1e-14
  )


  auc_h1 <- auc_h1_pass1
  new_likelihood <- new_likelihood


  auc_h1 <- auc_h1[["value"]]
  auc_h0 <- new_likelihood@func(0L)
  log_bf_uncorrected <- log(auc_h1 / auc_h0)
  log_bf <- log_bf_interval + log_bf_uncorrected
  bf <- exp(log_bf)



  list(marginal = bf * auc_h0, bf = bf)
}


is_approx <- function(e1) {
  atr <- attributes(e1)
  atr[["approximate"]]
}

is_point <- function(e1, value) {
  if (attributes(e1)[["prior"]][["family"]] == "point") {
    return(attributes(e1)[["prior"]][["point"]] == value)
  }
  FALSE
}
