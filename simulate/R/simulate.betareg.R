#'simulate.betareg
#'
#'@author Lionel Hertzog
#'@description Get new response values for beta regression
#' @details The function simulate new response data
#' 
#' @export 
#' @example inst/examples/simulateBetaregHelp.R

simulate.betareg <- function(object,nsim){
  prediction <- predict(object,type="link")
  linkinv <- object$link$mean$linkinv

  distr <- function(linpred_inv){
    phi <- object$coefficients$precision
    rbeta(length(linpred_inv), shape1 = phi*linpred_inv, shape2 = (1- linpred_inv) * phi)
  }
  
  sim <- sim_xxx(nsim,prediction,linkinv,distr)
  #create new class for simulation
  #class(sim) <- 
  return(sim)
}
