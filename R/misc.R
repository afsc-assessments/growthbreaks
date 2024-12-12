vonB <- function(linf, k, t0,  ages_use=1:20){
  ypred = linf*(1-exp(-k*(ages_use-t0)))
  return(ypred)
}

logistic <- function(ages, a50, delta){
  1/(1+exp(-log(19)*(ages-a50)/delta))
}

cols <- c("#86BBD8","#2F4858", "#F6AE2D", "#F26419", "#E86A92", "#57A773")

vonB_optim <- function(pars, ages_use, obs){
  linf <- pars[1]
  k <- pars[2]
  t0 <- pars[3]
  sigma <- pars[4]
  selex <- NULL ## TODO calc selex on ypred and multiply

  # ypred <- linf*(1-exp(-k*(age-t0)))
  ypred <- vonB(linf, k, t0, ages_use)
  ans <- sum(dnorm(ypred,obs, sigma, FALSE))
  return(ans)

}

# TMB::compile(here::here("TMB","sptlVB_Sel_Sigma.cpp"))
# dyn.load(TMB::dynlib(here::here("TMB","sptlVB_Sel_Sigma")))
