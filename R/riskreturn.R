#' @title Historical VaR and CVaR of a Security's Portfolio
#' @description Calculates the VaR metric of a security's portfolio
#' @param fecha is the date of calculation.
#' @param instruments is an array of instruments.
#' @param shares is an array of each instrument's shares.
#' @param efectivo is the quantity of cash in the portfolio.
#' @param confidence is the level of confidence at which the VaR and CVaR are calculated.
#' @param period es the period in days for the VaR and CVaR calculation.
#' @return VaR and CVaR of the security's portfolio
#' @export
RiskValues <- function(fecha = Sys.Date()-1,instruments,shares,efectivo,confidence = 0.95,period=252){
  #Get prices
  fechas <- seq.Date(fecha-2.5*period,fecha,1)
  precios <- tail(get_prices(fechas,instruments)[-1],period)
  #Adjust prices
  for(i in seq(1,length(colnames(precios)),1)){
    indice <- which(is.na(precios[,i]) == TRUE)
    if(length(indice) != 0){
      indice <- max(which(is.na(precios[,i]) == TRUE))
      precios[seq(1,indice,1),i] <- precios[indice+1,i]
    }
  }
  #Calculate value of portfolio
  monto <- sweep(precios,2,shares,`*`)
  monto <- rowSums(monto) + efectivo
  #Calculate the rate's series
  rendimientos <- (monto[-1]/monto[-length(monto)])

  #Calculate VaR and CVaR
  valuea <- quantile(rendimientos,1-confidence) - 1
  cvaluea <- mean(quantile(rendimientos,1-seq(confidence,0.99,0.001)) - 1)

  values <- data.frame(Days=length(precios[,1]),VaR=valuea,CVaR=cvaluea)
  return(values)
}
