#' @title Historical VaR and CVaR of a Security's Portfolio
#' @description Calculates the VaR metric of a security's portfolio
#' @param fecha is the date of calculation.
#' @param instruments is an array of instruments.
#' @param shares is an array of each instrument's shares.
#' @param efectivo is the amount of cash in the portfolio.
#' @param chequera is the amount of dollars in the portfolio.
#' @param confidence is the level of confidence at which the VaR and CVaR are calculated.
#' @param period es the period in days for the VaR and CVaR calculation.
#' @return VaR and CVaR of the security's portfolio
#' @export
RiskValues <- function(fecha = Sys.Date()-1,instruments,shares,efectivo,chequera = 0,confidence = 0.95,period=252){
  #This factor makes  the VaR estimate more conservative than Risk Department VaR
  factor <-  1.1
  #Get prices
  fechas <- seq.Date(fecha-2.5*period,fecha,1)
  precios <- tail(get_prices(fechas,instruments),period)
  #Adjust shares
  shares <- shares[order(instruments)]
  weights <- precios[length(precios$fecha),-1]*shares
  total <- sum(weights,efectivo,chequera)
  weights <- weights/total
  #Adjust prices
  excepciones <- c('1A','1B','1','1E','1F','1I','3','41','51','52','53','1ISP','1ASP','91SP')
  nombres <- sapply(colnames(precios),function(x){return(strsplit(x,"-")[[1]][1])})
  indices <- which(!(nombres %in% excepciones))[-1]
  for(x in indices){
    precios[,x] <- c(0,VaR_BondPriceChange(colnames(precios)[x],precios$fecha)[,2])
  }
  #Dollars on portfolio
  if(chequera > 0){
    shares_chequera <- chequera / get_prices(precios$fecha[length(precios$fecha)-1],"*CSP-MXPUSDS-V48")[,2]
    weights <- as.numeric(c(weights,chequera/total))
    dates <- seq.Date(fecha-2.5*period,fecha,1)
    precios$CHD <- shares_chequera*tail(get_prices(dates,"*CSP-MXPUSDS-V48")[,2],period)
  }
  if(length(colnames(precios))-1 != length(indices) & length(indices) != 0){
    #Rendimiento de fondos híbridos
    ultimo_dato <- length(precios$fecha)
    rendimientos <- cbind(precios[-1,c(1,indices)],precios[-1,-c(1,indices)]/precios[-ultimo_dato,-c(1,indices)]-1)
    weights <- weights[order(colnames(rendimientos[,-1]))]
  } else {
    if(length(indices) == 0){
      #Rendimientos de fondos renta variable
      rendimientos <- precios[-1,-1]/precios[-length(precios$fecha),-1]-1
    } else {
      #Sensibilidades de fondos renta fija
      rendimientos <- precios
    }
  }
  rendimientos$fecha <-  NULL
  #Calculate yield of portfolio
  rendimientos_ponderados <- sweep(rendimientos,2,as.numeric(weights),`*`)
  rendimientos_portafolio <- rowSums(rendimientos_ponderados)
  #Calculate VaR and CVaR
  valuea <- 100*factor*quantile(rendimientos_portafolio,1-confidence)
  cvaluea <- 100*factor*mean(quantile(rendimientos_portafolio,1-seq(confidence,0.99,0.001)))

  values <- data.frame(Days=length(precios[,1]),VaR=valuea,CVaR=cvaluea)
  return(values)
}
