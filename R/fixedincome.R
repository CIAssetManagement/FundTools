#' @title Grade2Number
#' @description Calculates a comparable number for Fitch, Moody's, S&P and HR Ratings grades.
#' @param note is the grade assigned by the rating agencies.
#' @return a comparable number for each note.
#' @export
Grade2Number <- function(note){
  con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="127.0.0.1",user="root", password="CIBANCO.00", dbname="mydb")
  query <- paste0("SELECT Calificadora,Valor FROM calificaciones WHERE Calificadora IN ('",paste(note,collapse = "','"),"')")
  number <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  return(number)
}

#' @title Number2Grade
#' @description Calculates a grade for a given number
#' @param number is the number given by the user.
#' @return a grade comparable with Fitch, Moody's, S&P and HR Ratings grades.
#' @export
Number2Grade <- function(number){
  part1 <- floor(number)
  part2 <- ifelse(number - part1 < 0.5,".0",".5")
  parts <- paste0(part1,part2)

  note <- switch(parts,"10"="AAA","9.5"="AA+","9"="AA","8.5"="AA-","8"="A+","7.5"="A","7"="A-",
                 "6.5"="BBB+","6"="BBB","5.5"="BBB-","5"="BB+","4.5"="BB","4"="BB-","3.5"="B+",
                 "3"="B","2.5"="B-","2"="C+","1.5"="C","1"="C-","0"="-")
  return(note)
}

#' @title Price of a Bond
#' @description Calculates the price of a Bond with the maturity date, calculation date, coupon rate and YTM of the Bond
#' @param mat is the maturity date of the Bond
#' @param day is the day in which the price is calculated
#' @param tcoupn is the coupon rate of the bond (annualized)
#' @param sobretasa is the spread of the coupon.
#' @param yield is the Yield to Maturity of the Bond
#' @param period is the time for every coupon payment
#' @return the price of the Bond at the calculation date
#' @export
BondPrice <- function(mat,day,tcoupn,sobretasa,yield,period=182){
  #Turn yield to effective per period
  ytm <-  period*(sobretasa+yield)/36000
  #Coupon
  coupn <- period*tcoupn/360
  #Day of pricing and maturity date
  maturity <-  as.Date(mat, format="%Y-%m-%d")
  today <- as.Date(day, format="%Y-%m-%d")
  #Number of coupons
  ncoupn <- ceiling(as.numeric(maturity - today)/period)
  #Days until the next coupon
  dcoupn <- as.numeric(maturity - today) %% period

  #Numerator and Denominator
  num <- rep(coupn,ncoupn)
  num[length(num)] <- num[length(num)] + 100
  denom <- (1+ytm)^((0:(ncoupn-1))+dcoupn/period)
  devengado <- coupn*(period-dcoupn)/period
  p <- sum(num/denom) - devengado

  return (as.numeric(p))
}

#' @title Generates a Price Path for a Bond that is used to calculate VaR.
#' @description Calculates the price of a Bond with the maturity date, calculation date, coupon rate and YTM of the Bond
#' @param id is the id for which the path is constructed.
#' @param dates is a vector of dates for the path.
#' @return the price of the Bond at the calculation date
#' @export
VaR_BondPriceChange <- function(id, dates){
  #Getting the bond info
  day <- max(dates)
  mydb <- DBI::dbConnect(drv=RMySQL::MySQL(),host="127.0.0.1",user="root", password="CIBANCO.00", dbname="mydb")
  query <- paste0("SELECT * FROM prices NATURAL JOIN bonds WHERE id='",id,"' AND prices.fecha='",day,"'")
  bono <- DBI::dbGetQuery(mydb,query)
  elemento <- substr(bono$TipoTasa,1,2)
  #Getting the appropiate rate
  subtasa <- DBI::dbGetQuery(mydb,paste0("SELECT id FROM tasas WHERE id='",bono$TipoTasa,"' AND fecha='",day,"'"))
  if(length(subtasa$id) != 0){
    subtasa <- c("1",subtasa$id)
  } else {
    venc <- as.numeric(as.Date(bono$FechaVencimiento) - day)
    subtasa <- DBI::dbGetQuery(mydb,paste0("SELECT id FROM tasas WHERE id='",bono$TipoTasa,"-",venc,"' AND fecha='",day,"'"))
    if(length(subtasa$id) != 0){
      subtasa <- c("1",subtasa$id)
    } else {
      if(elemento %in% c("M_","CE")){
        if(elemento == "M_"){
          subtasa <- c("1",paste0("BONOS-",as.numeric(as.Date(bono$FechaVencimiento)-day)))
        } else {
          subtasa <- c("1",paste0("CETES-",as.numeric(as.Date(bono$FechaVencimiento)-day)))
        }
      } else {
        if(elemento == "MA"){
          algo <- bono$TipoTasa
          subtasa1 <- paste0(substr(algo,5,8),"S-",gsub(".*?([0-9]+).*", "\\1", algo))
          subtasa <- c("2",subtasa1,"Fondeo-GuberMX")
        } else {
          if(elemento == "F."){
            subtasa <- c("1","Fondeo-BancarioMX")
          } else {
            subtasa <- c("0",bono$TasaCupon)
          }
        }
      }
    }
  }

  #Obtaining the prices series
  if(subtasa[1] == "1"){
    query <- paste0("SELECT fecha,nivel FROM tasas WHERE id = '",subtasa[2],"' AND fecha IN ('",
                    paste(dates,collapse = "','"),"')")
    tasa <- DBI::dbGetQuery(mydb,query)
  } else {
    if(subtasa[1] == "2"){
      query <- paste0("SELECT id,fecha,nivel FROM tasas WHERE id IN ('",paste(subtasa[c(2,3)],collapse = "','"),
                      "') AND fecha IN ('",paste(dates,collapse = "','"),"')")
      tasa <- DBI::dbGetQuery(mydb,query)
      tasa1 <- tasa %>% mutate(fecha = as.Date(fecha)) %>% spread(id, nivel) %>% data.frame(check.names = FALSE)
      tasa <- data.frame(fecha = tasa1$fecha, nivel = mapply(max,tasa1[,2],tasa1[,3]))
    } else {
      tasa <- data.frame(fecha = dates,nivel = as.numeric(subtasa[2]))
    }
  }
  precios <- mapply(PriceChange,bono$FechaVencimiento,day,bono$TasaCupon,bono$SobreTasa,tasa$nivel[-1],bono$Frecuencia,
                    (tasa$nivel[-1]-tasa$nivel[-length(tasa$fecha)])/100)
  DBI::dbDisconnect(mydb)
  precios <- data.frame(tasa$fecha[-1],precios = as.numeric(precios))
  # if(substr(subtasa[2],1,5) == "LIBOR"){
  #   last <- as.Date(as.character(precios$tasa.fecha[1]))
  #   datesusd <- c(as.Date(as.character(precios$tasa.fecha)),seq(last,length.out = 5,by = "-1 day"))
  #   datesusd <- datesusd[-1]
  #   totalusd <- length(precios$tasa.fecha)
  #   precios$precios <- precios$precios*get_prices(datesusd,"*CSP-MXPUSDS-V48")[1:totalusd,2]
  # }
  # if(substr(subtasa[2],1,5) == "TREAL"){
  #   precios$precios <- precios$precios*get_rates(precios[,1],"UDI")[,2]
  # }
  colnames(precios) <- c('Fecha',bono$id)
  return(precios)
}

#' @title Macaulay Duration of a Bond
#' @description Calculates the Macaulay Duration of a Bond with the maturity date, calculation date, coupon rate and YTM of the Bond
#' @param mat is the maturity date of the Bond
#' @param day is the day in which the price is calculated
#' @param tcoupn is the coupon rate of the bond (annualized)
#' @param sobretasa is the spread of the coupon.
#' @param yield is the Yield to Maturity of the Bond
#' @param period is the time for every coupon payment
#' @return the Macaulay Duration of the Bond
#' @export
MacaulayDuration <- function(mat,day,tcoupn,sobretasa,yield, period=182) {
  #Market price
  mprice <- BondPrice(mat,day,tcoupn,sobretasa,yield,period)
  #Turn ytm to effective per period
  ytm <-  period*(yield+sobretasa)/36000
  #Coupon
  coupn <- period*tcoupn/360
  #Day of pricing and maturity date
  maturity <-  as.Date(mat, format="%Y-%m-%d")
  today <- as.Date(day, format="%Y-%m-%d")
  #Number of coupons
  ncoupn <- ceiling(as.numeric(maturity - today)/period)
  #Days until the next coupon
  dcoupn <- as.numeric(maturity - today) %% period

  #Numerator and Denominator of the Macaulay Duration formula
  num <- seq(1,ncoupn,1)*coupn
  num[length(num)] <- num[length(num)] + 100*ncoupn
  denom <- (1+ytm)^((0:(ncoupn-1))+dcoupn/period)
  mcd <- sum(num/denom)
  #Half years
  mcd <- mcd/mprice
  #Complete years
  mcd <- mcd/(360/period)
  return (mcd)
}

#' @title Modified Duration of a Bond
#' @description Calculates the Duration of a Bond with the maturity date, calculation date, coupon rate and YTM of the Bond
#' @param mat is the maturity date of the Bond
#' @param day is the day in which the price is calculated
#' @param tcoupn is the coupon rate of the bond (annualized)
#' @param sobretasa is the spread of the coupon.
#' @param yield is the Yield to Maturity of the Bond
#' @param period is the time for every coupon payment
#' @return the  Modified Duration of the Bond
#' @export
ModifiedDuration <- function(mat,day,tcoupn,sobretasa,yield,period=182) {
  #Turn yield to effective per period
  ytm <-  period*(yield+sobretasa)/36000
  #Macaulay Duration
  mdur <- MacaulayDuration(mat,day,tcoupn,sobretasa,yield,period)
  #Modified Duration
  dur <- mdur/(1+ytm)
  return (dur)
}

#' @title Convexity of a Bond
#' @description Calculates the Convexity of a Bond with the maturity date, calculation date, coupon rate and YTM of the Bond
#' @param mat is the maturity date of the Bond
#' @param day is the day in which the price is calculated
#' @param tcoupn is the coupon rate of the bond (annualized)
#' @param sobretasa is the spread of the coupon.
#' @param yield is the Yield to Maturity of the Bond
#' @param period is the time for every coupon payment
#' @return the Convexity of the Bond
#' @export
Convexity <- function(mat,day,tcoupn,sobretasa,yield, period=182) {
  #Market price
  mprice <- BondPrice(mat,day,tcoupn,sobretasa,yield,period)
  #Turn ytm to effective per period
  ytm <-  period*(yield+sobretasa)/36000
  #Coupon
  coupn <- period*tcoupn/360
  #Day of pricing and maturity date
  maturity <-  as.Date(mat, format="%Y-%m-%d")
  today <- as.Date(day, format="%Y-%m-%d")
  #Number of coupons
  ncoupn <- ceiling(as.numeric(maturity - today)/period)
  #Days until the next coupon
  dcoupn <- as.numeric(maturity - today) %% period

  #Numerator and Denominator of the Convexity formula
  num <- seq(1,ncoupn,1)^2 * seq(1,ncoupn,1) * coupn
  num[length(num)] <- num[length(num)] + 100*ncoupn*ncoupn^2
  denom <- (1+ytm)^((0:(ncoupn-1))+dcoupn/period)
  conv <- sum(num/denom)/(1+ytm)^2
  conv <- conv/mprice
  return (conv)
}

#' @title Price Change of a Bond
#' @description Calculates the Price Change of a Bond with the maturity date, calculation date, coupon rate, YTM of the Bond and Yield change
#' @param mat is the maturity date of the Bond
#' @param day is the day in which the price is calculated
#' @param tcoupn is the coupon rate of the bond (annualized)
#' @param sobretasa is the spread of the coupon.
#' @param yield is the Yield to Maturity of the Bond
#' @param period is the time for every coupon payment
#' @param cyield is the change in the yield, by default is set to 1 bps.
#' @return the Price Change in the Bond
#' @export
PriceChange <- function(mat,day,tcoupn,sobretasa,yield,period=182,cyield=0.0001){
  #Duration
  dur <-  ModifiedDuration(mat,day,tcoupn,sobretasa,yield,period)
  #Convexity
  conv <- Convexity(mat,day,tcoupn,sobretasa,yield,period)
  #Price Change
  pc <- -dur*cyield + 0.5*conv*cyield^2

  return (pc)
}

#' @title Yield to Maturity of a Bond
#' @description Calculates the YTM of a Bond with the maturity date, calculation date, coupon rate and price of the Bond
#' @param mat is the maturity date of the Bond
#' @param day is the day in which the price is calculated
#' @param tcoupn is the coupon rate of the bond (annualized)
#' @param sobretasa is the spread of the coupon.
#' @param precio is the price of the bond
#' @param period is the time for every coupon payment.
#' @return the YTM of the Bond at the calculation date
#' @export
YTM <- function(mat,day,tcoupn,sobretasa,precio, period=182){

  #Coupon
  coupn <- period*tcoupn/360
  #Day of pricing and maturity date
  maturity <-  as.Date(mat, format="%Y-%m-%d")
  today <- as.Date(day, format="%Y-%m-%d")
  #Number of coupons
  ncoupn <- ceiling(as.numeric(maturity - today)/period)
  #Days until the next coupon
  dcoupn <- as.numeric(maturity - today) %% period

  #################    Newton method    ###################

  #Function f(x) for the Newton method
  f <- function(ytm){
    #Numerator of the function
    f_x <- BondPrice(mat,day,tcoupn,sobretasa,ytm,period) - precio
    return(f_x)
  }

  #Function f'(x) for the Newton method
  f1 <- function(ytm){
    f <- -ModifiedDuration(mat,day,tcoupn,sobretasa,ytm,period)*precio
    return(f)
  }

  #Iterations of Newton method
  xn1 <- tcoupn
  tol <- 1
  while(tol > 1e-7){
    xn <- xn1 - (f(xn1)/f1(xn1))
    tol <- abs(xn - xn1)
    xn1 <-  xn
  }
  return(xn1)
}

#' @title Modified Duration of a Bond Portfolio
#' @description Calculates the Duration of a Bond portfolio with the id's of the instruments and it's weights on the portfolio.
#' @param date is the date in which the Portfolio Duration is going to be calculated.
#' @param instruments is an array containing the instrument's id.
#' @param weight is an array containing the instrument's weights in the portfolio.
#' @return Modified Duration of the Portfolio.
#' @export
PortfolioDuration <- function(date=Sys.Date()-1,instruments,weight){
  Bonos <- get_bonds(instruments)
  Bonos$Peso <- weight
  Bonos$TasaCupon <- Bonos$TasaCupon/100
  Precios <- t(get_prices(date,instruments))[,1]
  Bonos$Precio <- as.numeric(Precios[2:length(Precios)])
  Bonos$FechaHoy <- rep(Sys.Date(),length(Bonos$id))
  Bonos$YTM <- mapply(YTM,Bonos$FechaVencimiento,Bonos$FechaHoy,Bonos$TasaCupon,Bonos$SobreTasa,Bonos$Precio,
                      Bonos$Frecuencia)
  Bonos$Duracion <- mapply(ModifiedDuration,Bonos$FechaVencimiento,Bonos$FechaHoy,Bonos$TasaCupon,Bonos$SobreTasa,
                           Bonos$YTM,Bonos$Frecuencia)
  duration <- sum(Bonos$Duracion*Bonos$Peso)
  return(duration)
}

#' @title Convexity of a Bond Portfolio
#' @description Calculates the Convexity of a Bond portfolio with the id's of the instruments and it's weights on the portfolio.
#' @param date is the date in which the Portfolio Convexity is going to be calculated.
#' @param instruments is an array containing the instrument's id.
#' @param weight is an array containing the instrument's weights in the portfolio.
#' @return Convexity of the Portfolio.
#' @export
PortfolioConvexity <- function(date=Sys.Date()-1,instruments,weight){
  Bonos <- get_bonds(instruments)
  Bonos$Peso <- weight
  Bonos$TasaCupon <- Bonos$TasaCupon/100
  Precios <- t(get_prices(date,instruments))[,1]
  Bonos$Precio <- as.numeric(Precios[2:length(Precios)])
  Bonos$FechaHoy <- rep(Sys.Date(),length(Bonos$id))
  Bonos$YTM <- mapply(YTM,Bonos$FechaVencimiento,Bonos$FechaHoy,Bonos$TasaCupon,Bonos$SobreTasa,Bonos$Precio,
                      Bonos$Frecuencia)
  Bonos$Convexidad <- mapply(Convexity,Bonos$FechaVencimiento,Bonos$FechaHoy,Bonos$TasaCupon,Bonos$SobreTasa,
                             Bonos$YTM,Bonos$Frecuencia)
  conv <- sum(Bonos$Convexidad*Bonos$Peso)
  return(conv)
}
