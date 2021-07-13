prior <- setClass(
  Class = "prior",
  slots = list(
    data = "list",
    theta_range = "numeric",
    func = "function",
    desc = "character",
    type = "character",
    dist_type = "character",
    plot = "list",
    parameters = "list",
    function_text = "character"
  )
)


likelihood <- setClass(
  Class = "likelihood",
  slots = list(
    data = "list",
    # theta_range = "",
    func = "function",
    desc = "character",
    observation = "numeric",
    # type = "character",
    dist_type = "character",
    plot = "list",
    # parameters = "",
    marginal = "character"
  )
)



product <- setClass(
  Class = "product",
  slots = list(
    data = "list",
    desc = "character",
    K = "numeric",
    lik = "function",
    prior = "function",
    theta_range = "numeric",
    likelihood_obj = "likelihood",
    prior_obj = "prior"
  )
)


posterior <- setClass(
  Class = "posterior",
  slots = list(
    data = "list",
    desc = "character",
    K = "numeric",
    lik = "function",
    prior = "function",
    theta_range = "numeric",
    likelihood_obj = "likelihood",
    prior_obj = "prior"
  )
)

prediction <- setClass(
  Class = "prediction",
  slots = list(
    data = "list",
    desc = "character",
    K = "numeric",
    lik = "function",
    prior = "function",
    theta_range = "numeric",
    likelihood_obj = "likelihood",
    prior_obj = "prior"
  )
)


setClassUnion("bayesplay", c(
  "likelihood",
  "prior",
  "product",
  "posterior",
  "prediction"
))

setMethod(
  "show",
  "bayesplay",
  function(object) {
    cat(object@desc, "\n")
  }
)

show <- function(x) {
  UseMethod("show")
}

#' Get fields from data slot
#' @param x a \code{bayesplay} object
#' @param name field name
#' @return content of the named field from the data slot
setMethod(
  "$",
  "bayesplay",
  function(x, name) {
    returnval <- x@data[[name]]
    return(returnval)
  }
)

#' Get names from data slot
#' @param x a \code{bayesplay} object
#' @return the field names from the data slot
setMethod("names",
  signature = "bayesplay",
  function(x) {
    return(names(x@data))
  }
)

# constants
theta <- "\u03F4"
posterior_labs <- list(x = theta, y = "Density")
likelihood_labs <- list(x = theta, y = "Pr(Outcome)")



setClass(
  Class = "normal",
  list(
    family = "character",
    fun = "function",
    default_range = "numeric"
  ),
  prototype = list(
    family = "normal",
    fun = function(x, mean, sd, ...) {
      dnorm(x = mean, mean = x, sd = sd)
    },
    default_range = c(-Inf, Inf)
  )
)


setClass(
  Class = "point",
  list(
    family = "character",
    fun = "function",
    default_range = "numeric"
  ),
  prototype = list(
    family = "point",
    fun = function(x, point, ...) {
      ifelse(x == point, 1, 0)
    },
    default_range = c(-Inf, Inf)
  )
)


setClass(
  Class = "uniform",
  list(
    family = "character",
    fun = "function",
    default_range = "numeric"
  ),
  prototype = list(
    family = "uniform",
    fun = function(x, min, max, ...) {
      dunif(x = x, min = min, max = max)
    },
    default_range = c(-Inf, Inf)
  )
)




setClass(
  Class = "student_t",
  list(family = "character", fun = "function", default_range = "numeric"),
  prototype = list(
    family = "student_t",
    fun = function(x, mean, sd, df) {
      dt_scaled(x = mean, mean = x, sd = sd, df = df)
    },
    default_range = c(-Inf, Inf)
  )
)


setClass(
  Class = "cauchy",
  list(family = "character", fun = "function", default_range = "numeric"),
  prototype = list(
    family = "cauchy",
    fun = function(x, location = 0, scale) {
      dcauchy(x = x, location = location, scale = scale)
    },
    default_range = c(-Inf, Inf)
  )
)

setClass(
  Class = "beta",
  list(family = "character", fun = "function", default_range = "numeric"),
  prototype = list(
    family = "beta",
    fun = function(x, alpha, beta) {
      dbeta(x = x, shape1 = alpha, shape2 = beta)
    },
    default_range = c(0, 1)
  )
)

setClass(
  Class = "noncentral_t",
  list(family = "character", fun = "function"),
  prototype = list(
    family = "noncentral_t",
    fun = function(x, t, df) {
      suppressWarnings(dt(x = t, df = df, ncp = x))
    }
  )
)


setClass(
  Class = "noncentral_d",
  list(family = "character", fun = "function"),
  prototype = list(
    family = "noncentral_d",
    fun = function(x, d, n) {
      suppressWarnings(dt(x = d * sqrt(n), df = n - 1, ncp = x * sqrt(n)))
    }
  )
)

setClass(
  Class = "noncentral_d2",
  list(family = "character", fun = "function"),
  prototype = list(
    family = "noncentral_d2",
    fun = function(x, d, n1, n2) {
      suppressWarnings(dt(
        x = d / sqrt(1 / n1 + 1 / n2),
        df = n1 + n2 - 2,
        ncp = x * sqrt((n1 * n2) / (n1 + n2))
      ))
    }
  )
)

setClass(
  Class = "binomial",
  list(family = "character", fun = "function"),
  prototype = list(
    family = "binomial",
    fun = function(p, successes, trials) {
      dbinom(prob = p, size = trials, x = successes)
    }
  )
)



setClassUnion(
  "family",
  c(
    "normal",
    "student_t",
    "noncentral_t",
    "noncentral_d",
    "noncentral_d2",
    "binomial",
    "point",
    "uniform",
    "cauchy"
  )
)
