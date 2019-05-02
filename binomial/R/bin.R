#' @title Number of Combinations of a Binomial
#' @description Calculates n choose k (choose() in base r)
#' @param n number of trials
#' @param k number of success
#' @return total number of combinations of "successes" which can occur in n trials
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(n = 5, k = 0)
#' bin_choose(n = 5, k = 1:3)
bin_choose <- function(n,k) {
  if (any(k>n)) {
    stop("k cannot be greater than n")
  }
  choose = factorial(n)/(factorial(k)*factorial(n-k))
  return(choose)
}


#' @title Probability of a Binomial Random Variable
#' @description Calculates probability that the number of successes occur given the number of trials and probability of success
#' @param success number of success
#' @param trials number of trials
#' @param prob probability of success
#' @return probability that the number of successes occur given the number of trials and probability of success
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' bin_probability(success = 55, trials = 100, prob = 0.45)
bin_probability <- function(success,trials,prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  check_success(success = success, trials = trials)

  probability = bin_choose(trials,success)*prob^(success)*(1-prob)^(trials-success)
  return(probability)
}


#' @title Distribution of a Binomial Random Variable
#' @description calculates the probability of different number of successes for a given number of trials
#' @param trials number of trials
#' @param prob probability of success for each trial
#' @return data frame with two columns, one for successes and one for its corresponding probability
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials,prob) {
  x = bin_probability(0:trials,trials = trials, prob = prob)
  p = data.frame(success =0:trials,probability= x)
  class(p) = c("bindis","data.frame")
  return(p)
}


#' @export
plot.bindis <- function(p) {
  barplot(p$probability, xlab="successes", ylab="probability", names.arg=p$success)
}


#' @title Cumulation of Probability for a Binomial Random Variable
#' @description calculates the probability and cumulative probability of different number of successes for a given number of trials
#' @param trials number of trials
#' @param success probability of success for each trial
#' @return data frame with three columns, one for successes , one for its corresponding probability, and one for the cumulative probability
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials,prob) {
  x = bin_distribution(trials=trials,prob=prob)
  data.frame(x)
  z = c(x[1,2],rep(0,trials))

  for (i in 1:trials){
    z[i+1] = x[i+1,2]+z[i]
  }

  y=data.frame(x, cumulative = z)
  class(y) = c("bincum","data.frame")
  return(y)
}


#' @export
plot.bincum <- function(y) {
  plot(y$success,y$cumulative, type="o", xlab="successes", ylab="probability")
}


#' @title Binomial Random Variable, variable creator
#' @description Creates a Binomial Random Variable given number of trials and prob
#' @param trials number of trials
#' @param prob probability of success
#' @return Binomial Random Variable object (named binvar) that represents a binomial random variable
#' @export
#' @examples
#' bin_variable(trials = 10, prob = 0.3)
bin_variable <- function(trials,prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  x <- list(trials = trials, prob = prob)
  class(x) <- ("binvar")
  return(x)
}


#' @export
print.binvar <- function(x) {
  cat("Binomial Variable",sep="\n")
  cat(sep="\n")
  cat("Parameters",sep="\n")
  cat("- number of trials: ")
  cat(x[[1]],sep="\n")
  cat("- prob of success : ")
  cat(x[[2]],sep="\n")
}


#' @export
summary.binvar <- function(x) {
  trials = x[1]
  prob = x[2]
  mean = aux_mean(x[[1]], x[[2]])
  variance = aux_variance(x[[1]], x[[2]])
  mode = aux_mode(x$trials, x$prob)
  skewness = aux_skewness(x[[1]], x[[2]])
  kurtosis = aux_kurtosis(x[[1]], x[[2]])
  summary_list <- c(trials,prob,mean,variance,mode,skewness,kurtosis)
  class(summary_list) <- ("summary.binvar")
  return(summary_list)
}


#' @export
print.summary.binvar <- function(x) {
  cat("Summary Binomial", sep="\n")
  cat(sep="\n")

  cat("Parameters", sep="\n")
  cat("- number of trials: ")
  cat(x[[1]], sep="\n")
  cat("- prob of success : ")
  cat(x[[2]], sep="\n")
  cat(sep="\n")

  cat("Measures", sep="\n")
  cat("- mean     : ")
  cat(x[[3]], sep="\n")
  cat("- variance : ")
  cat(x[[4]], sep="\n")
  cat("- mode     : ")
  cat(x[[5]], sep="\n")
  cat("- skewness : ")
  cat(x[[6]], sep="\n")
  cat("- kurtosis : ")
  cat(x[[7]], sep="\n")
}


#' @title Mean of a Binomial Random Variable
#' @description calculates the mean of a binomial random variable given number of trials and prob
#' @param trials number of trials
#' @param prob probability for success in each trial
#' @return the mean of the binomial random variable
bin_mean <- function(trials,prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  aux_mean(trials = trials, prob = prob)
}

#' @title Variance of a Binomial Random Variable
#' @description calculates the variance of a binomial random variable given number of trials and prob
#' @param trials number of trials
#' @param prob probability for success in each trial
#' @return the variance of the binomial random variable
bin_variance <- function(trials,prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  aux_variance(trials = trials, prob = prob)
}

#' @title Mode of a Binomial Random Variable
#' @description calculates the mode of a binomial random variable given number of trials and prob
#' @param trials number of trials
#' @param prob probability for success in each trial
#' @return the mode of the binomial random variable
bin_mode <- function(trials,prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  aux_mode(trials = trials, prob = prob)
}

#' @title Skewness of a Binomial Random Variable
#' @description calculates the skewness of a binomial random variable given number of trials and prob
#' @param trials number of trials
#' @param prob probability for success in each trial
#' @return the skewness of the binomial random variable
bin_skewness <- function(trials,prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  aux_skewness(trials = trials, prob = prob)
}

#' @title Kurtosis of a Binomial Random Variable
#' @description calculates the kurtosis of a binomial random variable given number of trials and prob
#' @param trials number of trials
#' @param prob probability for success in each trial
#' @return the kurtosis of the binomial random variable
bin_kurtosis <- function(trials,prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  aux_kurtosis(trials = trials, prob = prob)
}
