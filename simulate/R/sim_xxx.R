#' sim_xxx
#' @author Lionel Hertzog
#' 
#' @description Internal function to compute new prediction
#' 
#' @details Simulate new response data (so far for gam::mgcv)
#' 
#' @param nsim number of response vectors to simulate; defaults to 1
#' @param pred predictions, generated with \code{predict(object, type = "link")}
#' @param linkinv inverse link function.
#' @param distr error distribution for the specific family
#' @param rand.eff vector of random effects; defaults to NA = no random 
#' effects
#' @param cor.str matrix for the temporal or spatial correlation structure;
#' defaults to NA = no correlation
#' 
#' @example inst/examples/simulate.gamHelp.R


sim_xxx <- function(nsim = 1, pred, linkinv, distr, rand.eff = NA, cor.str = NA ){
  # TODO: option to keep draws from rand.eff and/or cor.str constant for all nsim
  
  npred <- length(pred)
  out <- matrix(NA, ncol = nsim, nrow = npred)
  
  for(i in 1:nsim){
    sim_RE <- rep(0, npred)
    if(!is.na(rand.eff))
    # draw random effects
      sim_RE <- draw_RE()
    
    sim_CorStruct <- rep(0, npred)
    if(!is.na(cor.str))
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