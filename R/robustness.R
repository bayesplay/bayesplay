
#' Title
#'
#' @param data_model
#' @param alternativeprior
#' @param nullprior
#' @param parameters
#' @param precision
#' @param cutoff
#' @param multicore
#'
#' @return
#' @export
#'
#' @examples
bfrr <- function(data_model,
               alternativeprior,
               nullprior, parameters, precision, cutoff = 3, multicore = FALSE) {
  if (multicore == TRUE) {
    future::plan("multicore")
    message("Running multicore")
  }


  values <- purrr::map(parameters, function(x) {
    do.call("seq", as.list(c(x, precision)))
  })
  values_df <- purrr::cross_df(values)
  if ("sd" %in% names(values_df)) {
    values_df <- dplyr::filter(values_df, sd != 0)
  }

  if ("df" %in% names(values_df)) {
    values_df <- dplyr::filter(values_df, df != 0)
  }

  base_prior_family <- alternativeprior$family

  values_list <- as.list(
    dplyr::group_split(
      dplyr::group_by(
        dplyr::mutate(values_df, index = 1:dplyr::n()), index
      )
    )
  )

  if (multicore == FALSE) {
    prior_list <- purrr::map_df(values_list, function(x) {
      x <- x |> dplyr::select(-index) #nolint
      base_params <- alternativeprior$parameters
      base_defined <- names(alternativeprior$parameters)
      instance_defined <- names(x)
      params <- c(
        list(family = base_prior_family),
        base_params[!(base_defined %in% instance_defined)], as.list(x)
      )

      instance_prior <- do.call("prior", params)

      tryCatch(
          {bf <- bayesplay::integral(data_model * instance_prior) /
        bayesplay::integral(data_model * nullprior)},
      error = function(e) {print(instance_prior); stop()})

      purrr::map(
        c(list(bf = bf), params),
        function(x) ifelse(length(x) == 1, x, list(x))
      ) |>
        tibble::as_tibble_row()
    })
  } else {
    prior_list <- furrr::future_map_dfr(values_list, function(x) {
      x <- x |> dplyr::select(-index) #nolint
      base_params <- alternativeprior$parameters
      base_defined <- names(alternativeprior$parameters)
      instance_defined <- names(x)
      params <- c(
        list(family = base_prior_family),
        base_params[!(base_defined %in% instance_defined)], as.list(x)
      )

      instance_prior <- do.call("prior", params)

      bf <- bayesplay::integral(data_model * instance_prior) /
        bayesplay::integral(data_model * nullprior)

      purrr::map(
        c(list(bf = bf), params),
        function(x) ifelse(length(x) == 1, x, list(x))
      ) |>
        tibble::as_tibble_row()
    }, .progress = TRUE)
  }

  prior_list |>
    dplyr::mutate(
      hyp = dplyr::case_when(
        bf <= 1 / cutoff ~ "H0",
        bf >= cutoff ~ "H1",
        TRUE ~ "Inc"
      )
    )
}

