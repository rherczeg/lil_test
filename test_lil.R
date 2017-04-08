# For more robust functions error handling should be applied. Input has to be checked for:
# 1. It has to exist.
# 2. It should be numeric and vector.
# 3. If there is any problem it has to give back respone for the user.

# These suggestions were not applied in this script due to the lack of time.


# calculate mean
test.mean <- function(x) { 
  tmp.sum <- sum(x)
  tmp.length <- length(x)
# we can also use "length - 1" due to unbiased estimation
# tmp.length <- length(x) - 1 
  tmp.mean <- tmp.sum / tmp.length
  return(tmp.mean)
}

# calculate Standard Deviation
test.sd <- function(x) {
  tmp.var <- var(x)
  tmp.sd <- sqrt(tmp.var)
  return(tmp.sd)
}

# calculate Confidence Intervals from a normal distribution
test.confInterval <- function(mean, sd, size, level) {
  # calculate the two-sided value
  level <- 1- ((1 - level) / 2)
  err <- qnorm(level) * sd/sqrt(size)
  tmp.confInter <- c(mean - err, mean + err)
  return(tmp.confInter)
}

# set seed
set.seed(123)
# create vector
vec <- runif(8999, 0.0, 1.0)

# calculate the Confidence Intervals and store in a variables
result <- test.confInterval(test.mean(vec), test.sd(vec), length(vec), .95)
result
