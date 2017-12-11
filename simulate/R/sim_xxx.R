#' sim_xxx
#' @author Loic Chalmandrier, Florian Hartig, Severin Hauenstein, Lionel Hertzog, Mira Kattwinkel
#' 
#' @description Internal function to compute new predictions
#' 
#' @details Simulate new response data (so far for gam::mgcv)
#' 
#' @param nsim number of response vectors to simulate; defaults to 1
#' @param pred predictions, generated with \code{predict(object, type = "link")}
#' @param linkinv inverse link function.
#' @param distr error distribution for the specific family
#' @param rand.eff vector of random effects; defaults to NA = no random 
#' effects
#' @param corStruct matrix for the temporal or spatial correlation structure;
#' defaults to NA = no correlation. See \code{\link{nlme::corClasses}}.
#' 
#' @example inst/examples/simulate.gamHelp.R

lme4::.simulateFun

sim_xxx <- function(nsim = 1, pred, linkinv, distr, rand.eff = NULL, corStruct = NULL){
  # TODO: option to keep draws from rand.eff and/or cor.str constant for all nsim
  
  npred <- length(pred)
  out <- matrix(NA, ncol = nsim, nrow = npred)
  
  for(i in 1:nsim){
    sim_RE <- rep(0, npred)
    if(!is.null(rand.eff))
    # draw random effects
      sim_RE <- draw_RE()
    
    sim_CorStruct <- rep(0, npred)
    if(!is.null(corStruct))
    # draw from correlation structure
      sim_CorStruct <- draw_CorStruct()
    
    # put everything together as the linear predictor
    linpred <- pred + sim_RE + sim_CorStruct
    
    #inverse link the predicted values
    linpred_inv <- linkinv(linpred)
    
    # sample from the inverse linear predictor depending on the distribution
    out[, i] <- distr(linpred_inv)
  }
  return(out)
}