#'sim_xxx
#' @author Lionel Hertzog
#' @description Internal function to compute new prediction
#' @details The function simulate new response data
#' 
#' @example inst/examples/simulate.gamHelp.R


sim_xxx <- function(nsim,pred,linkinv,distr){
  
  out <- matrix(NA,ncol=nsim,nrow=length(pred))
  
  for(n in 1:nsim){
    #add here before the random effect
    sim_RE <- draw_RE()
    #add here before the correlation structure
    sim_CorStruct <- draw_CorStruct()
    #get it all together
    linpred <- pred + sim_RE + sim_CorStruct
    #inverse link the predicted values
    linpred_inv <- linkinv(linpred)
    
    #draw nsim samples
    out[,n] <- distr(linpred_inv)
  }
  
  return(out)
}