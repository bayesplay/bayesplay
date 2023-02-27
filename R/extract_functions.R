#' Extract the posterior
#' @description Extract the \code{posterior} object from a \code{product} object
#' @param x a \code{product} object
#' @return a \code{posterior} object
#' @export
extract_posterior <- function(x) {
  if (!inherits(x, "product")) {
    stop("Object not of class product", call. = FALSE)
  }


  if (x@approximation == TRUE) {
    stop("Marginal likelihood has been approximated; Can't reliably output a posterior function.", call. = FALSE)
  }

  desc <- paste0(
    "Posterior\n",
    sub(
      x = sub(
        pattern = "  Family\n  ", replacement = "",
        x = x@likelihood_obj@desc, fixed = TRUE
      ),
      pattern = "\n  Parameters", replacement = ""
    ),
    sub(
      x = sub(
        pattern = "  Family\n  ", replacement = "",
        x = x@prior_obj@desc, fixed = TRUE
      ), pattern = "\n  Parameters",
      replacement = ""
    ),
    "\nNormalising constant: ", round(x[["integral"]], 4L)
  )
  x@desc <- desc

  new(
    Class = "posterior",
    data = x@data,
    desc = x@desc,
    K = x@K,
    lik = x@lik,
    prior = x@prior,
    theta_range = x@theta_range,
    likelihood_obj = x@likelihood_obj,
    prior_obj = x@prior_obj
  )
}

#' Extract predictions
#' @description Extract the marginal predictions over the prior
#' @param x a \code{product} object
#' @return a \code{prediction} object
#' @export
extract_predictions <- function(x) {
  if (!inherits(x, "product")) {
    stop("Object not of class product", call. = FALSE)
  }

  desc <- paste0(
    "Marginal prediction\n",
    sub(
      x = sub(
        pattern = "  Family\n  ", replacement = "",
        x = x@likelihood_obj@desc, fixed = TRUE
      ), pattern = "\n  Parameters",
      replacement = ""
    ),
    sub(
      x = sub(
        pattern = "  Family\n  ", replacement = "",
        x = x@prior_obj@desc, fixed = TRUE
      ), pattern = "\n  Parameters",
      replacement = ""
    ),
    "\nPrediction range: X = ", range_as_text(get_max_range(x)), "\n",
    "Current observation: X = ", x@likelihood_obj@observation
  )
  new(
    Class = "prediction",
    data = x@data,
    desc = desc,
    K = x@K,
    lik = x@lik,
    prior = x@prior,
    theta_range = x@theta_range,
    likelihood_obj = x@likelihood_obj,
    prior_obj = x@prior_obj
  )
}
