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
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' @importFrom methods new
#' @importFrom stats qnorm sd integrate




# `/.product` <- function(e1, e2) {
#   bf <- e1@data[["integral"]] / e2@data[["integral"]]
#   return(bf)
# }

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
  return(rv)
}

#' @export
`/.auc` <- function(e1, e2) {
  # FIXME: This is a hack... attribute should be set earlier
  is_e1_approx <- is_approx(e1)
  is_e2_approx <- is_approx(e2)

  has_approximation <- is_e1_approx | is_e2_approx

  is_e1_point <- is_point(e1, 0L)
  is_e2_point <- is_point(e2, 0L)

  # is_e1_point <- ifelse(is_e1_point,
  #   attributes(e1)[["prior"]][["point"]] == 0L, FALSE
  # )
  # is_e2_point <- ifelse(is_e2_point,
  #   attributes(e2)[["prior"]][["point"]] == 0L, FALSE
  # )

  one_is_point <- is_e1_point | is_e2_point


  # if one of the objects is an approximation
  # then one of the them must also be a point null
  # FIXME: This condition is not correct
  if (has_approximation == TRUE && one_is_point == FALSE) {
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
    return("Extreme evidence")
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
  # TODO: Move approximation it's own slot in class
  bf <- x@prior_obj@func(point) / x[["posterior_function"]](point)
  new("bf", bf)
}


### FIXME: This needs to be updated

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
  # prior_family <- prior[["family"]]

  approx <- check_approximation(likelihood, prior)
  do_approximation <- approx[["approximation"]]
  if (do_approximation == TRUE) {
    supported_prior <- approx[["supported_prior"]]
    approximation_params <- approx[c("n", "t", "df")]
  }
  supported_prior <- approx[["supported_prior"]]
  approximation_params <- approx[c("n", "t", "df")]


  if (do_approximation == TRUE && likelihood_family == "noncentral_t") {
    stop(
    "t value is large; approximation needed
     Reparameterize using a `noncentral_d` of `noncentral_d2` likelihood.",
      call. = FALSE
    )
  }


  if (do_approximation == TRUE && supported_prior == FALSE) {
    warning("Observation is large; approximation needed.")
    stop("Approximations are only supported with cauchy priors.", call. = FALSE)
  }

  if (do_approximation == TRUE) {
    n <- approximation_params[["n"]]
    t <- approximation_params[["t"]]
    df <- approximation_params[["df"]]
    approximation <- estimate_marginal(n, t, df, prior)
    marginal_likelihood_approx <- approximation[["marginal"]]
    observation_shift <- approximation[["observation_shift"]]
    if (observation_shift != 0L) {
      obs <- recompute_observation(likelihood, observation_shift)
      warning("Observation of d = ", obs[["d"]], " is unstable.\n",
        "Shifting observation to d = ", obs[["new_d"]],
        call. = FALSE
      )
    }

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
    return(Vectorize(predictive_func))
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


  if (do_approximation) {
    auc <- marginal_likelihood_approx
    text <- paste0(
      "  Area under curve: ", round(auc, 4L),
      " (approximation)"
    )
  } else {
    auc <- marginal_likelihood
    text <- paste0("  Area under curve: ", round(auc, 4L))
  }

  data <- list(
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
    text
    # "  Area under curve: ", round(data[["integral"]], 4L)
  )


  new(
    Class = "product",
    desc = desc,
    data = data,
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
  t <- d * sqrt(n)
  df <- n - 1L
  # observation_type <- "d"
  list(n = n, t = t, df = df)
}

reparameterise_d2_to_t <- function(likelihood_obj) {
  n1 <- likelihood_obj[["parameters"]][["n1"]]
  n2 <- likelihood_obj[["parameters"]][["n2"]]
  d <- likelihood_obj[["parameters"]][["d"]]
  n <- n1 * n2 / (n1 + n2)
  t <- d * sqrt(n)
  df <- n1 + n2 - 2L
  list(n = n, t = t, df = df)
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
  if (likelihood_family %in% likelihods_approx == FALSE) {
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


  if (likelihood_family == "noncentral_t") {
    t <- likelihood_obj[["parameters"]][["t"]]
    df <- likelihood_obj[["parameters"]][["df"]]
    parameters <- list(n = NA, t = t, df = df)
  } else if (likelihood_family == "noncentral_d") {
    parameters <- reparameterise_d_to_t(likelihood_obj)
  } else if (likelihood_family == "noncentral_d2") {
    parameters <- reparameterise_d2_to_t(likelihood_obj)
  }

  more_than_15 <- abs(parameters[["t"]]) > 15L
  more_than_5 <- abs(parameters[["t"]]) > 5L
  in_range <- prior_in_range(prior_obj, likelihood_obj)

  if (more_than_15 == TRUE) {
    approximation <- TRUE
  } else if (more_than_5 == TRUE) {
    if (in_range == FALSE) {
      approximation <- TRUE
    } else {
      approximation <- FALSE
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


shift_observation <- function(likelihood_obj, shift) {
  family <- likelihood_obj[["family"]]
  params <- likelihood_obj[["parameters"]]
  observation <- params[[1L]]
  # round_observation <- round(observation, 2L)
  round_observation <- observation
  # fallback for second pass
  # if (observation == round_observation) {
  round_observation <- round_observation + shift
  # }
  params[[1L]] <- round_observation
  params[["family"]] <- family
  do.call(likelihood, params)
}


total_n <- function(likelihood_obj) {
  if (likelihood_obj[["family"]] == "noncentral_d") {
    n <- likelihood_obj[["parameters"]][["n"]]
  } else if (likelihood_obj[["family"]] == "noncentral_d2") {
    n1 <- likelihood_obj[["parameters"]][["n1"]]
    n2 <- likelihood_obj[["parameters"]][["n2"]]
    n <- n1 * n2 / (n1 + n2)
  } else {
    stop("This should not happen")
  }
  n
}


recompute_observation <- function(likelihood_obj, observation_shift) {
  n <- total_n(likelihood_obj)
  d <- likelihood_obj[["parameters"]][["d"]]
  t <- d * sqrt(n)
  new_t <- t + observation_shift
  new_d <- new_t / sqrt(n)
  list(d = d, new_d = new_d)
}



estimate_marginal <- function(n, t, df, prior) {
  prior_limits <- prior[["parameters"]][["range"]]
  # prior_family <- prior[["family"]]
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
  scale <- prior[["parameters"]][["scale"]]
  rscale <- scale * sqrt(n)
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
  # TODO: Move up and down so that we can get a better estimate

  # FIXME: Step out in both directions at the same time
  # Then the one that stops first is the closest one
  pass_1_likelihood <- new_likelihood
  pass_2_likelihood <- new_likelihood
  original_observation <- new_likelihood@observation
  while (TRUE) {
    auc_h1_pass1 <- integrate(
      Vectorize(\(x)  pass_1_likelihood@func(x) * new_prior@func(x)),
      -Inf, Inf,
      subdivisions = 1000L, abs.tol = 1e-14
    )
    error1 <- auc_h1_pass1[["abs.error"]]

    auc_h1_pass2 <- integrate(
      Vectorize(\(x)  pass_2_likelihood@func(x) * new_prior@func(x)),
      -Inf, Inf,
      subdivisions = 1000L, abs.tol = 1e-14
     )
    error2 <- auc_h1_pass2[["abs.error"]]


    error <- error1 == 0L | error2 == 0L
    if (error) {
      pass_1_likelihood <- shift_observation(pass_1_likelihood, -0.01)
      pass_2_likelihood <- shift_observation(pass_2_likelihood, 0.01)
    } else {
      break
    }
  }

  # TODO: This should pick the most conservative one
  observation_shift_1 <- abs(
    pass_1_likelihood@observation - original_observation
  )
  observation_shift_2 <- abs(
    pass_2_likelihood@observation - original_observation
  )
  if (observation_shift_1 < observation_shift_2) {
    auc_h1 <- auc_h1_pass1
    new_likelihood <- pass_1_likelihood
  } else {
    auc_h1 <- auc_h1_pass2
    new_likelihood <- pass_2_likelihood
  }


  auc_h1 <- auc_h1[["value"]]
  auc_h0 <- new_likelihood@func(0L)
  log_bf_uncorrected <- log(auc_h1 / auc_h0)
  log_bf <- log_bf_interval + log_bf_uncorrected
  bf <- exp(log_bf)
  new_observation <- new_likelihood@observation

  if (new_observation != original_observation) {
    observation_shift <- new_observation - original_observation
  } else {
    observation_shift <- 0L
  }

  if (observation_shift != 0L) {
    stop(t, n, df, observation_shift)
  }

  list(marginal = bf * auc_h0, bf = bf, observation_shift = observation_shift)
}


is_approx <- function(e1) {
  atr <- attributes(e1)
  if(!"approximate" %in% names(atr)) {
    return(FALSE)
  }
  atr[["approximate"]]
}

is_point <- function(e1, value) {
  if(attributes(e1)[["prior"]][["family"]] == "point") {
   return(attributes(e1)[["prior"]][["point"]] == value)
  } 
  FALSE
}
