# Calculates the mean of a binomial random variable given number of trials and prob
aux_mean <- function(trials, prob) {
  mean = trials*prob
  return(mean)
}


# Calculates the variance of a binomial random variable given number of trials and prob
aux_variance <- function(trials,prob) {
  variance = trials*prob*(1-prob)
  return(variance)
}


# Calculates the mode of a binomial random variable given number of trials and prob
aux_mode <- function(trials,prob) {
  mode = floor(trials*prob+prob)
  return(mode)
}


# Calculates the skewness of a binomial random variable given number of trials and prob
aux_skewness <- function(trials,prob) {
  skewness = (1-2*prob)/(trials*prob*(1-prob))^(1/2)
  return(skewness)
}


# Calculates the skewness of a binomial random variable given number of trials and prob
aux_kurtosis <- function(trials,prob) {
  kurtosis = (1-6*prob*(1-prob))/(trials*prob*(1-prob))
  return(kurtosis)
}
