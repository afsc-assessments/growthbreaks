
signifMK <- function(x, d, upper, lower, eval = 0, crit.eval){
  miss <- upper > eval & lower < eval ## check if CI contains fail value
  rej <- x ## save evaluated point
  # want <- d > eval  | d < eval ## T/F if derivative is greater or less than 0
  # want <- d > eval & d > crit.eval[2]  | d < eval & d < crit.eval[1] ## T/F if derivative is greater or less than 0 and outside interval
  big <- which.max(c(abs(min(d)),abs(max(d))))
  want <- d == c(min(d),max(d))[big] ## return just one if equal
  rej[!want |miss] <- NA ## wherever equals zero or CI contains, input NA
  return(rej)
}

## hand it pdats, derivatives, intervals, and value to compare
## this will simply reject derivatives that equal zero or fall outside the CI
signifD <- function(x, d, upper, lower, eval = 0) {
  miss <- upper > eval & lower < eval
  incr <- decr <- x
  want <- d > eval
  incr[!want | miss] <- NA
  want <- d < eval
  decr[!want | miss] <- NA
  list(incr = incr, decr = decr)
}
## mk function -- see if derivative CI contains 0
# signifCI <- function(x, upper, lower, eval = 0){
#   rej <- x ## print in terms of evaluated quantities
#   rej[upper > 0 & lower < 0] <- NA
#   return(rej)
# }
