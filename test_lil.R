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
