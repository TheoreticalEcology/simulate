#simulate.gam.betarHELP

library(mgcv)
library(simulate)

#simulate some data
dat <- gamSim(n=100)
#constrain it 0-1
dat$y <- with(dat,(y-min(y))/(max(y)-min(y)))
#fit a model
m <- gam(y~s(x0),data=dat,family = "betar")
#simulate new values
simY <- simulate(m)