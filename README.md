
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bayesplay: The Bayesian playground

<!-- badges: start -->

![R-CMD-check](https://github.com/bayesplay/bayesplay/workflows/R-CMD-check/badge.svg)
![CRAN](https://www.r-pkg.org/badges/version-ago/bayesplay)
![downloads](https://cranlogs.r-pkg.org/badges/grand-total/bayesplay)
[![codecov](https://codecov.io/gh/bayesplay/bayesplay/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bayesplay/bayesplay)
<!-- badges: end -->

The goal of bayesplay is to provide an interface for calculating Bayes
factors for simple models. It does this in a way that makes the
calculations more *transparent* and it is therefore useful as a teaching
tools.

## Installation

bayesplay is now on CRAN. You can install it with:

``` r
install.packages("bayesplay")
```

Or if you want to live on the edge, you can install the development
version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bayesplay/bayesplay")
```

## Basic usage

The `bayesplay` package comes with three basic functions for computing
Bayes factors.

1.  The `likelihood()` function for specifying likelihoods

2.  The `prior()` function for specifying priors

3.  And the `integral()` function

Currently the following distributions are supported for likelihoods and
priors

### Priors

1.  Normal distribution (`normal`)

2.  Uniform distribution (`uniform`)

3.  Scaled and shifted *t* distribution (`student_t`)

4.  Cauchy distributions (`cauchy`)

5.  Beta distribution (`beta`)

### Likelihood

1.  Normal distribution (`normal`)

2.  Scaled and shifted *t* distribution (`student_t`)

3.  Binomial distribution (`binomial`)

4.  Various noncentral *t* distributions, including:

    - Noncentral *t* distribution (`noncentral_t`)

    - Noncentral *t* distribution scaled for a paired samples/one sample
      Cohen’s *d* (`noncentral_d`)

    - Noncentral *t* distribution scaled for an independent samples
      Cohen’s *d* (`noncentral_d2`)

## Worked examples

For worked examples of the basic usage see [basic
usage](https://bayesplay.github.io/bayesplay/articles/basic.html). Or
for basic plot functionality see [basic
plotting](https://bayesplay.github.io/bayesplay/articles/plots.html)

## Changelog

> Breaking changes for \< v0.9.0
>
> `distribution` parameter for specifying likelihoods and priors has
> been renamed `family`
>
> `noncentral_d` and `noncentral_d2` are now parametrised in terms of
> sample size rather than df
