In typical Bayes factor analyses, a prior is specified for the alternative and
null models. For example, an analyst might specify a normal distribution with a
mean of 0 and standard deviation of 15 as the prior for the alternative model.
In `bayesplay` syntax, this would be done as follows:

```{r, eval=FALSE}
prior(family = "normal", mean = 0, sd = 15)
```

In specifying a prior in this way, the analyst chooses a set value for the two
parameters (mean and standard deviation). It might be of interest to



The `bayesplay` package includes functionality for systematically varying
parameters of the alternative prior for computing *robustness regions* and for
visualizing these robustness regions.

```{r}

data_model <- likelihood(family = "student_t", mean = 5, sd = 11, df = 12)
alternative_prior <- prior(family = "normal", mean = 0, sd = 15)
null_prior <- prior(family = "point", point  = 0)

m1 <- data_model * alternative_prior
m0 <- data_model * null_prior

bf <- integral(m1) / integral(m0)

summary(bf)

```

One might then wonder how robust this conclusions is to variations in the
alternative prior. Would the conclusions change if a higher value
or a lower value was chosen for the standard deviation of the prior? With the
`bfrr` function it's possible to set an upper and lower bound of the standard
deviation and to compute an visualise the resulting Bayes factor for values
within this range.

In the example below, the standard deviation is varies across the range from 1
to 20 (in steps of 1).

```{r}

rr <- bfrr(
    likelihood = data_model, # reuse the definition above
    alternative_prior = alternative_prior, # reuse the definition above
    null_prior = null_prior, # reuse the definition above
    parameters = list(sd = c(1, 50)), # vary sd from 1 to 20
    precision = 1
)

```
