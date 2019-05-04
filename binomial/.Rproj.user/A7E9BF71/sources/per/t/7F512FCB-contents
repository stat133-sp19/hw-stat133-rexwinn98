
install.packages("devtools")
library("devtools")
library(roxygen2)

# Checks if probability is valid
check_prob <- function(prob){
  if (prob <= 1 & prob >= 0){
    return(TRUE)
  }
  else {
    stop("invaid prob value")
  }
}

# Test if an input trials is a valid value
check_trials <- function(trials){
  if (trials >= 0 & (trials %% 1) == 0){
    return(TRUE)
  }
  else {
    stop("invaid trials value")
  }
}

# Test if number of successes is valid
check_success <- function(success, trials){
  if (sum((success >= 0 & (success %% 1) == 0 & success <= trials)) == length(success)){
    return(TRUE)
  }
  else {
    stop("invaid success value")
  }
}

# Auxilary functions

aux_mean <- function(trials, prob){
  return(trials * prob)
}

aux_variance <- function(trials, prob){
  return(trials * (1 - prob) * prob)
}

aux_mode <- function(trials, prob){
  return(as.integer(trials * prob + prob))
}

aux_skewness <- function(trials, prob){
  return((1 - 2 * prob)/sqrt(trials * prob * (1 - prob)))
}

aux_kurtosis <- function(trials, prob){
  return((1 - 6 * prob * (1 - prob))/(trials * prob * (1 - prob)))
}

#' @title Binomial Choose
#' @description Calculates the number of combinations in which k succeses can occur in n trials
#' @param n number of trials
#' @param k number of successes
#' @return number of combinations
#' @export
#' @examples bin_choose(4, 2) = 6
bin_choose <- function(n, k){
  return((factorial(n)/(factorial(k)*factorial(n-k))))
}

#' @title Binomial Probability
#' @description Calculates the probability of getting a number of successes from  number of trials with a probability of a success rate
#' @param success number of desired successes
#' @param trials number of trials
#' @param prob probability that a trial will be a success
#' @return probability of getting number of successes from trials
#' @export
#' @examples bin_probability(2, 5, .5) = .3125
bin_probability <- function(success, trials, prob){
  if(check_trials(trials) == TRUE & check_prob(prob) == TRUE & check_success(success, trials) == TRUE){
    return(bin_choose(trials, success) * (prob ** success) * (prob ** (trials - success)))
  } else {
    return(check_trials(trials) & check_prob(prob) & check_success(success, trials))
  }
}

#' @title Binomial Distribution
#' @description Calculates the probabilities  of getting 0 through trials success s
#' @param trials number of trials
#' @param prob probability that a trial will be a success
#' @return probability of getting number of successes from trials for 0 through trials successes
#' @export
#' @examples bin_probability(5, .5)
bin_distribution <- function(trials, prob){
  success <- 0:trials
  probability <- bin_probability(success, trials, prob)
  probability1 <- data.frame(success, probability)
  class(probability1) <- unlist(c('bindis', 'data.frame'))
  return(probability1)
}

#' @export
plot.bindis <- function(bindis){
  return(barplot(height = unlist(bindis[2]), names.arg = unlist(bindis[1]), xlab = 'successes' ))
}

#' @title Binomial Cumulation
#' @description Returns a data frame with probability distribution and cumulative probabilities
#' @param trials number of trials
#' @param prob probability that a trial will be a success
#' @return data frame with two classes, succes in the first column, probability in the second column, and cumulative in the third column
#' @export
#' @examples bin_cumulative(5, .5)
bin_cumulative <- function(trials, prob){
  initial_dist <- bin_distribution(trials, prob)
  cumulative <- cumsum(initial_dist$probability)
  initial_dist$cumulative <- cumulative
  class(initial_dist) <- c("bincum", "data.frame")
  return(initial_dist)
}

#' @export
plot.bincum <- function(bincum){
  return(plot(bincum$success, bincum$cumulative, type = "b", ylab = 'cumulative', xlab = 'successes'))
}

#' @title Binomial Variable
#' @description Finds the binomial variable of a binomial distribution
#' @param trials number of trials
#' @param prob probability that a trial will be a success
#' @return A binomial random variable object
#' @export
#' @examples bin_variable(5, .5)
bin_variable <- function(trials, prob){
  var <- vector()
  var[1] <- trials
  var[2] <- prob
  var[3] <- aux_variance(trials, prob)
  var[4] <- aux_mean(trials, prob)
  var[5] <- aux_kurtosis(trials, prob)
  var[6] <- aux_mode(trials, prob)
  var[7] <- aux_skewness(trials, prob)
  class(var) <- "binvar"
  if (check_trials(trials) == TRUE & check_prob(prob) ==    TRUE){
    return(var)
  } else {
    return(check_trials(trials) & check_prob(prob))
  }
}

#' @export
print.binvar <- function(binvar){
  p <- paste0(cat("Binomial Variable\n Parameters: trials, prob"), c(binvar[1], binvar[2]))
  print(p)
}

#' @export
summary.binvar<- function(binvar){
  return(c(binvar[1], binvar[2], binvar[4], binvar[3], binvar[6], binvar[7], binvar[5]))
}

#' @export
print.summary.binvar <- function(binvar){
  p <- paste("trials, mean, variance, mode, skewness, kurtosis", c(binvar[1], binvar[2], binvar[4], binvar[3], binvar[6], binvar[7], binvar[5]))
  print(p)
}


#' @title Binomial Mean
#' @description Returns mean of binomial dist
#' @param trials number of trials
#' @param prob probability that a trial will be a success
#' @return Mean
#' @export
#' @examples bin_cumulative(5, .5)
bin_mean <- function(trials, prob){
  if (check_trials(trials) == TRUE & check_prob(prob) ==    TRUE){
    return(aux_mean(trials, prob))
  } else {
    return(check_trials(trials) & check_prob(prob))
  }
}

#' @title Binomial Variance
#' @description Returns variance of binomial dist
#' @param trials number of trials
#' @param prob probability that a trial will be a success
#' @return Variance
#' @export
#' @examples bin_cumulative(5, .5)
bin_variance <- function(trials, prob){
  if (check_trials(trials) == TRUE & check_prob(prob) ==    TRUE){
    return(aux_variance(trials, prob))
  } else {
    return(check_trials(trials) & check_prob(prob))
  }
}

#' @title Binomial Mode
#' @description Returns Mode of binomial dist
#' @param trials number of trials
#' @param prob probability that a trial will be a success
#' @return mode
#' @export
#' @examples bin_cumulative(5, .5)
bin_mode <- function(trials, prob){
  if (check_trials(trials) == TRUE & check_prob(prob) ==    TRUE){
    return(aux_mode(trials, prob))
  } else {
    return(check_trials(trials) & check_prob(prob))
  }
}

#' @title Binomial Skewness
#' @description Returns skewness of binomial dist
#' @param trials number of trials
#' @param prob probability that a trial will be a success
#' @return skewedness
#' @export
#' @examples bin_cumulative(5, .5)
bin_skewness <- function(trials, prob){
  if (check_trials(trials) == TRUE & check_prob(prob) ==    TRUE){
    return(aux_skewness(trials, prob))
  } else {
    return(check_trials(trials) & check_prob(prob))
  }
}

#' @title Binomial kurtosis
#' @description Returns kurtosis of binomial dist
#' @param trials number of trials
#' @param prob probability that a trial will be a success
#' @return Kurtosis
#' @export
#' @examples bin_cumulative(5, .5)
bin_kurtosis <- function(trials, prob){
  if (check_trials(trials) == TRUE & check_prob(prob) ==    TRUE){
    return(aux_kurtosis(trials, prob))
  } else {
    return(check_trials(trials) & check_prob(prob))
  }
}




