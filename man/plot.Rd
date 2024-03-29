% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R, R/robustness.r
\name{plot}
\alias{plot}
\alias{plot.prior}
\alias{plot.posterior}
\alias{plot.likelihood}
\alias{plot.product}
\alias{plot.prediction}
\alias{plot.robustness}
\title{Plot a bayesplay object}
\usage{
\method{plot}{prior}(x, ...)

\method{plot}{posterior}(x, add_prior = FALSE, ...)

\method{plot}{likelihood}(x, ...)

\method{plot}{product}(x, ...)

\method{plot}{prediction}(x, model_name = "model", ...)

\method{plot}{robustness}(x, ...)
}
\arguments{
\item{x}{a \code{robustness} object}

\item{...}{arguments passed to methods}

\item{add_prior}{set to TRUE to add prior to the posterior plot}

\item{model_name}{name of the model}
}
\value{
a \code{ggplot2} object
}
\description{
Plots an object created by bayesplay
}
