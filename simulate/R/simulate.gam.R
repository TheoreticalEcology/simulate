#'simulate.gam.beta
#' @author Lionel Hertzog
#' @description Simulate new response from a GAM (mgcv) 
#' @details The function simulate new response data
#' 
#' @export 
#' @example inst/examples/simulateGamHelp.R

simulate.gam <- function(object,nsim){
  
  prediction <- predict(object,type="link")
  linkinv <- object$family$linkinv
  
  fam <- object$family$family
  if(length(grep("Beta",fam))!=0){
    distr <- function(linpred_inv){
      phi <- object$family$getTheta(trans =T)
      rbeta(length(linpred_inv), shape1 = phi*linpred_inv, shape2 = phi-linpred_inv*phi) #Here linpred_inv is the mu of the beta distribution
    }
  }
  else if(length(grep("Negative Binomial",fam))!=0){
    distr <- function(linpred_inv){
      rnbinom(length(linpred_inv), size = object$family$getTheta(trans = T), mu = linpred_inv)
    }
  }
  else if(length(grep("ziP",fam))!=0){
    distr <- function(linpred_inv){
      theta <- object$family$getTheta(trans=FALSE)
      sim <- function(linpred_inv,theta){
        ## generate zero inflated Poisson random variables, where 
        ## lambda = exp(linpred_inv), eta = theta[1] + exp(theta[2])*linpred_inv
        ## and 1-p = exp(-exp(eta)).
        y <- linpred_inv; n <- length(y)
        lambda <- exp(linpred_inv)
        eta <- theta[1] + exp(theta[2])*linpred_inv
        p <- 1- exp(-exp(eta))
        ind <- p > runif(n)
        y[!ind] <- 0
        np <- sum(ind)
        ## generate from zero truncated Poisson, given presence...
        y[ind] <- qpois(runif(np,dpois(0,lambda[ind]),1),lambda[ind])
        y
      } 
      return(sim)
    }
    
  }
  else{
    stop()
    print("Not implemented")
  }
  sim <- sim_xxx(nsim,prediction,linkinv,distr)
  #create new class for simulation
  #class(sim) <- 
  return(sim)
}
  