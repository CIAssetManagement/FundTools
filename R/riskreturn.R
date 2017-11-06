#' @title Historical VaR and CVaR of a Security's Portfolio
#' @description Calculates the VaR metric of a security's portfolio
#' @param instruments is an array of instruments.
#' @param shares is an array of each instrument's shares.
#' @param cash is the amount of cash in the portfolio.
#' @param confidence is the level of confidence at which the VaR and CVaR are calculated.
#' @param period es the period in days for the VaR and CVaR calculation.
#' @return VaR and CVaR of the security's portfolio
#' @export
RiskValues <- function(instruments,shares,cash,confidence = 0.95,period=252){
  fechas <- seq.Date(Sys.Date()-2.5*period,Sys.Date()-1,1)
  precios <- tail(get_prices(fechas,instruments)[,-1],period)
  precios <- t(t(precios) * shares)
  precios1 <- rowSums(precios,na.rm=TRUE)
  #Adding the cash to the portfolio
  precios1 <- precios1 + cash
  rendimientos <- (precios1[-1]/precios1[-length(precios1)])-1
  #VaR
  valuea <- stats::quantile(rendimientos,1-confidence,na.rm=TRUE)
  valuea <- ifelse(valuea > 0,0,valuea)
  #CVaR
  cvaluea <- mean(rendimientos[rendimientos < valuea])
  values <- data.frame(VaR=valuea,CVaR=cvaluea)
  return(values)
}
