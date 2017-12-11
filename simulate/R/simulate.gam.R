#'simulate.gam.beta
#' @author Lionel Hertzog
#' @description Simulate new response from a GAM (mgcv) 
#' @details The function simulate new response data
#' 
#' @export 
#' @example inst/examples/simulate.gamHelp.R

simulate.gam <- function(object,nsim){
  
  prediction <- predict(object,type="link")
  linkinv <- object$family$linkinv
  
  fam <- object$family$family
  if(grep("Beta",fam)){
    distr <- function(linpred_inv){
      rbeta
    }
  }
  else if(grep("Negative binomial",fam)){
    distr <- function(linpred_inv){
      rnbinom
    }
  }
  else if(grep("ziP",fam)){
    distr <- function(linpred_inv){
      theta <- object$family$getTheta(trans=TRUE)
      sim <- rzip(linpred_inv,theta)
      return(sim)
    }
    
  }
  sim <- sim_xxx(nsim,prediction,linkinv,distr)
  #create new class for simulation
  #class(sim) <- 
  return(sim)
}