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




`/.product` <- function(e1, e2) {
  bf <- e1@data[["integral"]] / e2@data[["integral"]]
  return(bf)
}

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
  attr(rv, "approximate") <- obj[["approximate"]]
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
  is_e1_approx <- attributes(e1)[["approximate"]] %||% FALSE
  is_e2_approx <- attributes(e2)[["approximate"]] %||% FALSE  

  has_approximation <- is_e1_approx | is_e2_approx

  is_e1_point <- attributes(e1)[["prior"]][["family"]] == "point"
  is_e2_point <- attributes(e2)[["prior"]][["family"]] == "point"

  is_e1_point <- ifelse(is_e1_point,
    attributes(e1)[["prior"]][["point"]] == 0L, FALSE
  )
  is_e2_point <- ifelse(is_e2_point,
    attributes(e2)[["prior"]][["point"]] == 0L, FALSE
  )

  one_is_point <- is_e1_point | is_e2_point

  
  # if one of the objects is an approximation
  # then one of the them must also be a point null
  # FIXME: This condition is not correct
   if (has_approximation  == TRUE && one_is_point == FALSE) {
     stop("Marginal likelihood is a approximation. One prior must be a point prior at 0",
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
#' @description Provide a verbal summary of a Bayes factor and the level of evidence
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
  is_estimated <- x@approximation
  if (is_estimated && point != 0L) {
    stop("point must be 0 if the marginal likelihood is estimated",
      call. = FALSE)
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
  prior_family <- prior[["family"]]

  is_estimate <- FALSE
  if (likelihood_family %in% c("noncentral_t", "noncentral_d2", "noncentral_d")) {
    estimate <- check_estimate(likelihood, prior)
    is_estimate <- estimate[["estimate"]]
  } 
   

  if (is_estimate == TRUE && likelihood_family == "noncentral_t") {
    stop("t value is large; approximation needed. 
    Reparametrise using a `noncentral_d` or `noncentral_d2` likelihood.", call. = FALSE)
  }

  # TODO: If estimate needed and prior is not cauchy, then error
  if (is_estimate == TRUE && prior_family != "cauchy") {
    stop("Obseration is large; approximation needed.
    Approximations are only supported with cauchy priors.", call. = FALSE)
  }


  if (is_estimate == TRUE) {
    # WORK OUT ESTIMATE
    n <- estimate[["n"]]
    t <- estimate[["t"]]
    df <- estimate[["df"]]
    marginal_likelihood_estimate <- estimate_marginal(n, t, df, prior) 
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


  data <- list(
    integral = ifelse(is_estimate, marginal_likelihood_estimate, marginal_likelihood),
    marginal_function = marginal_func,
    evidence_function = evidence_func,
    posterior_function = posterior_func,
    conditional_function = conditional_func,
    weighted_likelihood_function = product_function,
    prediction_function = prediction_function#,
    # approximation = is_estimate
  )

  desc <- paste0(
    "Product\n",
    "  Likelihood family: ", likelihood[["family"]], "\n",
    "  Prior family: ", prior[["family"]], "\n",
    "  Area under curve: ", round(data[["integral"]], 4L)
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
    approximation = is_estimate
  )
}


check_estimate <- function(likelihood_obj, prior_obj) {
  prior_family <- prior_obj[["family"]]
  if(prior_family == "point") {
    return(list(estimate = FALSE, is_large = FALSE, not_in_prior = FALSE))
  }
  likelihood_family <- likelihood_obj[["family"]]
# if (likelihood_family %in% c("noncentral_t", "noncentral_d2", "noncentral_d")) {
  if (likelihood_family == "noncentral_d") {
    n <- likelihood_obj[["parameters"]][["n"]]
    d <- likelihood_obj[["parameters"]][["d"]]
    t <- d * sqrt(n)
    df <- n - 1L
    scaling <- sqrt(n)
    observation_type <- "d"
  }
  if (likelihood_family == "noncentral_d2") {
    n1 <- likelihood_obj[["parameters"]][["n1"]]
    n2 <- likelihood_obj[["parameters"]][["n2"]]
    d <- likelihood_obj[["parameters"]][["d"]]
    t <- d * sqrt(n1 * n2 / (n1 + n2))
    n <- n1 * n2 / (n1 + n2)
    df <- n1 + n2 - 2L
    scaling <- sqrt(n)
    observation_type <- "d"
  }
  if (likelihood_family == "noncentral_t") {
    df <- likelihood_obj[["parameters"]][["df"]]
    t <- likelihood_obj[["parameters"]][["t"]]
    scaling <- 1L
    n <- NA
    observation_type <- "t"
  }
  estimate <- FALSE
  is_large <- FALSE
  if (abs(t) > 15L) {
    estimate <- TRUE
    is_large <- TRUE
  }
  not_in_prior <- FALSE
  if ("range" %in% names(prior_obj[["parameters"]])) {
    prior_limits <- prior_obj[["parameters"]][["range"]]
    # upper <- prior_limits[[2L]]
    # lower <- prior_limits[[1L]]
    observation <- likelihood_obj@observation
    in_prior <- observation >= prior_limits[[1]] && observation <= prior_limits[[2]]
    if (!in_prior) {
      estimate <- TRUE
      not_in_prior <- TRUE
    }
  }
 # }
  list(
    estimate = estimate, 
    is_large = is_large, 
    not_in_prior = not_in_prior,
    n = n,
    t = t, 
    df = df
  )
}


estimate_marginal <- function(n, t, df, prior) {

  prior_limits <- prior[["parameters"]][["range"]]
  prior_family <- prior[["family"]]
  upper <- prior_limits[[2L]]
  lower <- prior_limits[[1L]]
  var_delta <- 1L / n
  delta_est <- t / sqrt(n)
  mean_delta <- (delta_est * n) / (n)
  log_post_probs <- c(
    pt((lower - mean_delta) / sqrt(var_delta), df, log.p = TRUE),
    pt((upper - mean_delta) / sqrt(var_delta), df, log.p = TRUE)
  )


  # TODO: Consider other priors
if(prior_family == "cauchy") {
  location <- prior[["parameters"]][["location"]]
  scale <- prior[["parameters"]][["scale"]]
  rscale <- scale * sqrt(n)
  log_prior_probs <- c(
    pcauchy(lower, location = location, scale = rscale, lower.tail = TRUE, log.p = TRUE),
    pcauchy(upper, location = location, scale = rscale, lower.tail = TRUE, log.p = TRUE)
  )
}


prior_interval <- log_prior_probs[[2L]] + pexp(diff(log_prior_probs), 1L, TRUE, TRUE)
post_interval <- log_post_probs[[2L]] + pexp(diff(log_post_probs), 1L, TRUE, TRUE)
log_bf_interval <- post_interval - prior_interval


  new_prior <- prior("cauchy", location, rscale, c(-Inf, Inf))
  new_likelihood <- likelihood("noncentral_t", t, df)

auc_h1 <- integrate(
    Vectorize(\(x)  new_likelihood@func(x) * new_prior@func(x)),
    -Inf, Inf, abs.tol = 1e-09 )[["value"]]

auc_h0 <- new_likelihood@func(0)

log_bf_uncorrected <- log(auc_h1 / auc_h0)
log_bf <- log_bf_interval + log_bf_uncorrected
bf <- exp(log_bf)



bf * auc_h0


}
