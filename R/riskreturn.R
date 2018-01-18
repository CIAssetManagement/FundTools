#' @title Historical VaR and CVaR of a Security's Portfolio
#' @description Calculates the VaR metric of a security's portfolio
#' @param fecha is the date of calculation.
#' @param instruments is an array of instruments.
#' @param shares is an array of each instrument's shares.
#' @param confidence is the level of confidence at which the VaR and CVaR are calculated.
#' @param period es the period in days for the VaR and CVaR calculation.
#' @param type is the type of the portfolio: "stocks" or "bonds".
#' @return VaR and CVaR of the security's portfolio
#' @export
RiskValues <- function(fecha,instruments,shares,type,confidence = 0.95,period=252){
  #Get prices
  fechas <- seq.Date(fecha-2.5*period,fecha,1)
  precios <- tail(get_prices(fechas,instruments)[-1],period)
  #Calculate weights
  pesos <- tail(t(t(precios) * shares),1)
  pesos <- as.numeric(pesos / sum(pesos))
  #Calculate standard devation and correlation
  rendimientos <- (precios[-1,]/precios[-length(precios[,1]),])
  desv <- apply(rendimientos,2,function(df) sd=sd(df,na.rm = TRUE))
  correlation <- cor(rendimientos,use = "complete.obs")
  sigmap <- sqrt(as.vector(pesos*desv) %*% correlation %*% as.vector(t(pesos*desv)))
  if(type=="bonds"){
    con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="127.0.0.1",user="root", password="CIBANCO.00", dbname="mydb")
    fechas <- format(fechas,'%m/%d/%Y')
    query <- paste0("SELECT nivel FROM nodos WHERE id = 'CETES-28' AND fecha in ('",paste(fechas, collapse = "','"),
                    "')")
    y <- DBI::dbGetQuery(con, query)$nivel
    y <- tail(y,period)
    DBI::dbDisconnect(con)
    sigmay <- sd(y[-1]/y[-length(y)])
    sigmap <- sigmap*sigmay
  }
  valuea <- sigmap*qnorm(1-confidence, mean = 0, sd = 1)
  cvaluea <- mean(qnorm(seq(0.001,1-confidence,0.001),0,1)*as.vector(sigmap))


  values <- data.frame(Days=length(precios[,1]),VaR=valuea,CVaR=cvaluea)
  return(values)
}
