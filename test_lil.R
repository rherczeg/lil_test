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
