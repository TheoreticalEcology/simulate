# create data: steal example from mgcv::negbin
library(mgcv)
library(simulate)

set.seed(3)
n <- 400 # sample size
nsim <- 100 # number of simulations from the model

dat <- gamSim(1, n = n)

# negative binomial
g <- exp(dat$f / 5)
dat$ynb <- rnbinom(g, size = 3, mu = g)

# beta: for example simply rescale y to [0,1]
dat$yb <- (dat$y - min(dat$y, na.rm = T)) / (max(dat$y, na.rm = T) - min(dat$y, na.rm = T))

# zero inflated poisson
rzip <- function(gamma, theta= c(-2,.3)) {
  ## generate zero inflated Poisson random variables, where 
  ## lambda = exp(gamma), eta = theta[1] + exp(theta[2])*gamma
  ## and 1-p = exp(-exp(eta)).
  y <- gamma; n <- length(y)
  lambda <- exp(gamma)
  eta <- theta[1] + exp(theta[2]) * gamma
  p <- 1- exp(-exp(eta))
  ind <- p > runif(n)
  y[!ind] <- 0
  np <- sum(ind)
  ## generate from zero truncated Poisson, given presence...
  y[ind] <- qpois(runif(np, dpois(0, lambda[ind]), 1), lambda[ind])
  y
} 

dat$yzip <- rzip(dat$f/4-1)

##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

# test simulate.gam: negative binomial 
## known theta fit ...
fgnb0 <- gam(ynb ~ s(x0) + s(x1) + s(x2) + s(x3), family = negbin(3), data = dat)

## same with theta estimation...
fgnb <- gam(ynb ~ s(x0) + s(x1) + s(x2) + s(x3), family = nb(), data = dat)

## simulate
sim.fgnb0 <- simulate.gam(object = fgnb0, nsim = nsim)
print(colMeans(sim.fgnb0))

sim.fgnb <- simulate.gam(object = fgnb, nsim = nsim)
print(colMeans(sim.fgnb))

##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

# test simulate.gam: beta regression
fgb <- gam(yb ~ s(x0) + s(x1) + s(x2) + s(x3), family = betar, data = dat)

## simulate
sim.fgb <- simulate.gam(object = fgb, nsim = nsim)
print(colMeans(sim.fgb))

##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

# test simulate.gam: zero-inflated poisson
fgzip <- gam(yzip ~ s(x0) + s(x1) + s(x2) + s(x3), family = ziP(), data = dat)

## simulate
sim.fgzip <- simulate.gam(object = fgzip, nsim = nsim)
print(colMeans(sim.fgzip))