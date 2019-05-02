# Returns true if prob is a number between 0 and 11
check_prob <- function(prob) {
  if (length(prob) > 1) {
    stop("p cannot have a length greater than 1")
  }
  if (prob >= 0 & prob <= 1) {
    return(TRUE)
  }
  else {
    stop("p has to be a number between 0 and 1")
  }
}

# Returns true if trials is a whole number greater than 0
check_trials <- function(trials) {
  if (length(trials) > 1) {
    stop("trials cannot have a length greater than 1")
  }
  if (trials > 0 & trials %% 1 == 0) {
    return(TRUE)
  }
  else {
    stop("invalid trials value")
  }
}

# Returns true if success < trials and success >= 0 and success is a whole number
check_success <- function(success, trials) {
  if (any(success>trials)) {
    stop("success value(s) cannot be greater than trials")
  }
  
  if (any(success < 0)) {
    stop("invalid success value(s), success value(s) must be greater than 0")
  }
  
  if (any(success %% 1 !=0)) {
    stop("success must be whole number(s)")
  }
  if (success <= trials & success >= 0 & all(success %% 1 ==0)) {
    return(TRUE)
  }
  
}