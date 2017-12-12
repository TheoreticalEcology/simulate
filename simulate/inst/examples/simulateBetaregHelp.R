library(simulate)
library(betareg)

#fit a model from ?betareg examples
data("GasolineYield", package = "betareg")
gy <- betareg(yield ~ batch + temp, data = GasolineYield)
summary(gy)

#get 100 new simulations
sims <- simulate(gy,nsim=100)

