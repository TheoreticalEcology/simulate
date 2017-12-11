#'simulate.lme
#' @author Lionel Hertzog
#' @description Simulate new response from a lme object (nlme)
#' @details The function simulate new response data
#' 
#' @export 
#' @example inst/examples/simulate.lmeHelp.R

simulate.lme <- function(object){
  
  prediction <- predict(object)
  linkinv <- identity()
  
  #grab the random effect structure
  to_draw_RE <- list(Sigma=getVarCov(object),fixef=fixef(object),modmat=model.matrix(object))
  
  #grab the correlation structure
  
  distr <- function(linpred_inv){
    sigma <- object$sigma
    sim <- rnorm(linpred_inv,sigma)
    return(sim)
  }
  
  sim <- sim_xxx(nsim,prediction,linkinv,distr,rand.eff=rand.eff)
  #create new class for simulation
  #class(sim) <- 
  return(sim)
}