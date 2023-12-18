map <- function(x, f, ...) {
  f <- match.fun(f)
  dout <- lapply(x, f, ...)
  row.names(dout) <- NULL
  dout
}


pmap <- function(x, f, ...) {
  x_list <- unname(split(x, ~ row.names(x)))
  f <- match.fun(f)
  dout <- lapply(x_list, f, ...)
  dout <- Reduce(f = rbind, dout)
  row.names(dout) <- NULL
  dout
}

par_map <- function(x, f, ...) {
  f <- match.fun(f)
  # mclapply doesn't work on windows?
  # dout <- parallel::mclapply(x, f, mc.cores = parallel::detectCores(), ...) # nolint
  dout <- parallel::parLapply(x, f, ...)
  row.names(dout) <- NULL
  dout
}

par_pmap <- function(x, f, ...) {
  x_list <- unname(split(x, ~ row.names(x)))
  f <- match.fun(f)
  dout <- parallel::mclapply(x_list, f,
    mc.cores = parallel::detectCores(), ...
  )
  row.names(dout) <- NULL
  Reduce(f = rbind, dout)
}

list_to_df <- function(x) {
  n <- names(x)
  x |>
    matrix(nrow = length(n)) |>
    t() |>
    as.data.frame() |>
    stats::setNames(n)
}

pull <- function(.data, var) {
  var <- substitute(var)
  .data[[var]]
}


filter <- function(.data, ...) {
  filter_pattern <- eval(substitute(alist(...)))
  operator <- map(filter_pattern, \(f) deparse(f[[1L]]))
  variable <- map(filter_pattern, \(a) paste0("d[['", a[[2L]], "']]"))
  condition <- map(filter_pattern, \(a) a[[3L]])


  df_list <- split(.data, row.names(.data)) |> unname()
  filter_function_text <- paste(variable, operator, condition,
    collapse = " && "
  )
  filter_function <- parse(text = paste0("\\(d) ", filter_function_text))
  data_out_list <- Filter(eval(filter_function), df_list)
  data_out <- Reduce(function(x, y) rbind(x, y), data_out_list)
  row.names(data_out) <- NULL
  data_out
}


slide <- function(xvar, n) {
  if ((n %% 2L == 0L)) {
    stop("n must be odd")
  }
  lead <- (n - 1L) / 2L
  for (i in (1L + lead):(length(xvar) - (lead))) {
    prev_x <- xvar[[i - 1L]]
    next_x <- xvar[[i + 1L]]
    if (prev_x == next_x) {
      xvar[[i]] <- prev_x
    }
  }
  xvar
}

select <- function(.data, var) {
  .data[var]
}
