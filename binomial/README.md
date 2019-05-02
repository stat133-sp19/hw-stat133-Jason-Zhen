## Overview

`"binomial"` is a minimal package that
provides functions to analyze the probabilities of a binomial random variable.

These functions include: 

  - `bin_probability()` returns probability of obtaining # of successes for # of trials and probability of success.
  - `bin_distribution()` returns distribution of the above mentioned probabilities
  - `bin_cumulative()` returns cumulation of probabilities of the above mentioned 
  - `bin_variable()` creates an object of class `binvar`
  - `summary()` returns summary information of a `binvar` object
  - `plot()` used on bindis or bincum objects to plot the distribution or cumulation
  

## Motivation

This package has been developed to analyze binomial random variables at a basic level moreso for the creator to practice R package development. 

## Installation

Install the package from github like so:

``` r
# install "binomial" (without vignettes)
devtools::install_github("https://github.com/stat133-sp19/hw-stat133-Jason-Zhen/edit/master/binomial")

# install "binomial" (with vignettes)
devtools::install_github("https://github.com/stat133-sp19/hw-stat133-Jason-Zhen/edit/master/binomial", build_vignettes = TRUE)
```

## Usage

``` r
library(binomial)

bin_probabilities(success = 1 , trials = 5, p = 0.3)
#> [1] 0.36015

dis1 <- bin_distribution(trials =3 , p = 0.5)
dis1
#>  success probability
#> 1       0       0.125
#> 2       1       0.375
#> 3       2       0.375
#> 4       3       0.125

plot(dis1)

dis2 <- bin_cumulative(trials = 3, p = 0.5)
dis2
#>  success probability cumulative
#> 1       0       0.125      0.125
#> 2       1       0.375      0.500
#> 3       2       0.375      0.875
#> 4       3       0.125      1.000

plot(dis2)

bin1 <- bin_variable(trials = 3, p = 0.5)
summary(bin1)

#> Summary Binomial

#> Parameters
#> - number of trials: 3
#> - prob of success : 0.5

#> Measures
#> - mean     : 1.5
#> - variance : 0.75
#> - mode     : 2
#> - skewness : 0
#> - kurtosis : -0.6666667
```
