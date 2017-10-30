#' @title Historical VaR of a Security's Portfolio
#' @description Calculates the VaR metric of a security's portfolio
#' @param instruments is an array of instruments.
#' @param shares is an array of each instrument's shares.
#' @param confidence is the level of confidence at which the VaR is calculated.
#' @param period es the period in days for the VaR calculation.
#' @return VaR of the security's portfolio
#' @export
ValueAtRisk <- function(instruments,shares,confidence = 0.95,period=252){
  fechas <- seq.Date(Sys.Date()-(period+116),Sys.Date()-1,1)
  precios <- get_prices(fechas,instruments)[,-1]
  precios <- t(t(precios) * shares)
  precios1 <- rowSums(precios,na.rm=TRUE)
  if(length(precios1) < period)
    print(paste0("El cálculo se realizó con: ",length(precios)," datos"))
  rendimientos <- (precios1[-1]/precios1[-length(precios1)])-1
  valuea <- stats::quantile(rendimientos,1-confidence,na.rm=TRUE)
  valuea <- ifelse(valuea > 0,0,valuea)
  return(valuea)
}
