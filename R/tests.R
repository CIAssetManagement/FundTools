
#' @title Price of a Bond
#' @description Calculates the price of a Bond with the maturity date, calculation date, coupon rate and YTM of the Bond
#' @param mat is the maturity date of the Bond
#' @param day is the day in which the price is calculated
#' @param tcoupn is the coupon rate of the bond (annualized)
#' @param ytm is the Yield to Maturity of the Bond
#' @return the price of the Bond at the calculation date
#' @export
BondPrice <- function(mat,day,tcoupn,ytm){
  #ytm anualizado
  #tcoup as decimals
  #dates as strings

  #Turn ytm to effective per period
  ytm <-  182*ytm/360
  #Coupon
  coupn <- 182*100*tcoupn/360
  #Day of pricing and maturity date
  maturity <-  as.Date(mat, format="%Y%m%d")
  today <- as.Date(day, format="%Y%m%d")
  #Number of coupons
  ncoupn <- ceiling(as.numeric(maturity - today)/182)
  #Days until the next coupon
  dcoupn <- (182 - as.numeric(maturity - today)) %% 182

  p <-  coupn+coupn*(1/ytm - 1/(ytm*(1+ytm)^(ncoupn-1)))
  p <-  p + (100/(1+ytm)^(ncoupn-1))
  p <- p/(1+ytm)^(1-dcoupn/182)

  return (p)
}

#' @title Macaulay Duration of a Bond
#' @description Calculates the Macaulay Duration of a Bond with the maturity date, calculation date, coupon rate and YTM of the Bond
#' @param mat is the maturity date of the Bond
#' @param day is the day in which the price is calculated
#' @param tcoupn is the coupon rate of the bond (annualized)
#' @param ytm is the Yield to Maturity of the Bond
#' @return the Macaulay Duration of the Bond
#' @export
MacaulayDuration <- function(mat,day,tcoupn, ytm) {
  #ytm anualizado
  #tcoup as decimals
  #dates as strings

  #Market price
  mprice <- BondPrice(mat,day,tcoupn,ytm)
  #Turn ytm to effective per period
  ytm <-  182*ytm / 360
  #Coupon
  coupn <- 182*100*tcoupn/360
  #Day of pricing and maturity date
  maturity <-  as.Date(mat, format="%Y%m%d")
  today <- as.Date(day, format="%Y%m%d")
  #Number of coupons
  ncoupn <- ceiling(as.numeric(maturity - today)/182)
  #Days until the next coupon
  dcoupn <- (182 - as.numeric(maturity - today)) %% 182

  #Numerator of the Macaulay Duration formula
  num <- seq(1,ncoupn,1)*coupn
  num[length(num)] <- num[length(num)] + 100*ncoupn

  #Denominator of the Macaulay Duration formula
  if (dcoupn = 182) {
    denom <- (1 + ytm)^(1:ncoupn)
  }
  else {
    denom <- (1+ytm)^(1-dcoupn/182)
    denom <- c(denom,(1+ytm)^(2:ncoupn))
  }
  mcd <- sum(num/denom)
  mcd <- mcd/mprice
  return (mcd)
}

#' @title Price Change of a Bond
#' @description Calculates the Price Change of a Bond with the maturity date, calculation date, coupon rate, YTM of the Bond and Yield change
#' @param mat is the maturity date of the Bond
#' @param day is the day in which the price is calculated
#' @param tcoupn is the coupon rate of the bond (annualized)
#' @param ytm is the Yield to Maturity of the Bond
#' @param cyield is the change in the yield, by default is set to 1 bps.
#' @return the Price Change in the Bond
#' @export
PriceChange <- function(mat,day,tcoupn,ytm,cyield=0.01){
  #ytm anualizado
  #tcoup as decimals
  #dates as strings

  #Macaulay Duration
  mcd <-  MacaulayDuration(mat,day,tcoupn, ytm)
  #Per period YIeld to Maturity
  ytm <-  182*ytm/360
  #Modified Duration
  md <- mcd/(1+ytm)
  #Price Change
  pc <- -md   *   cyield

  return (pc)
}


#' @title Say goodbye
#' @description Receives a name and says goodbye
#' @param name a string
#' @return a string with the goodbye
#' @export
goodbye <- function(name = "") {
  paste("goodbye", name)
}


#' @title Say goodbye2
#' @description Receives a name and says goodbye
#' @param name a string
#' @return a string with the goodbye
#' @export
goodbye2 <- function(name = "") {
  paste("goodbye!!!", name)
}
