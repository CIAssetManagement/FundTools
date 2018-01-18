#' @title Dates for inflation
#' @description Calculates the date a month ago, a quarter ago, a semester ago and a yera ago.
#' @param fecha is the date for which the dates will be calculated, format = YYYY-mm-dd.
#' @return the inflation rate for the given dates and type.
#' @export
fechas_inflacion <- function(fecha){
  day <- as.numeric(substr(fecha,9,10))
  month <- as.numeric(substr(fecha,6,7))
  year <- as.numeric(substr(fecha,1,4))

  if(day == 15){
    inicio <- paste0(year-1,"-",ifelse(month<10,paste0("0",month),month),"-",ifelse(day<10,paste0("0",day),day))
    fin <- fecha
    fechas <- seq(as.Date(inicio),as.Date(fin),by = "1 month")
    fechas <- fechas[c(12,10,7,1)]
  } else {
    inicio <- paste0(year-1,"-",ifelse(month<10,paste0("0",month),month),"-","01")
    fechas <- seq(as.Date(inicio),length = 13,by = "1 month")-1
    fechas <- fechas[c(13,11,8,2)]
  }

  return(as.character(fechas))
}

#' @title Inflation rate over a given period of time
#' @description Calculates the inflation rate for the selected date.
#' @param inicio starting period for inflation calculation.
#' @param fin ending period for inflation calculation,
#' @param tipo it can be: general, subyacente, nosubyacente.
#' @return the inflation rate for the given dates and type.
#' @export
Inflacion1 <- function(inicio,fin,tipo='general'){
  con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="127.0.0.1",user="root", password="CIBANCO.00", dbname="mydb")
  query <- paste0("SELECT fecha,nivel FROM inpc WHERE id='","INPC-",tipo,"' AND fecha IN ('",inicio,"','",fin,"')")
  niveles <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  dato <- 100*((niveles$nivel[2]/niveles$nivel[1])-1)
  return(dato)
}

#' @title Inflation rate within a given window for selected dates.
#' @description Calculates the Monthly, Quarterly, Semianually and Anually inflation rate.
#' @param fechas is a vector with the dates on which inflation will be calculated, they must be in "YYYY-mm" format.
#' @param tipo it can be: general, subyacente, nosubyacente.
#' @return the inflation rate for the given dates and type.
#' @export
Inflacion2 <- function(fechas,tipo='general'){
  fechas <- sort(fechas)
  fecha1 <- paste(fechas,"-15",sep = "")
  year <- as.numeric(strsplit(fechas[2],"-")[[1]])
  month <- ifelse(year[2]+1<10,paste0("0",year[2]+1),year[2]+1)
  fecha2 <- seq(as.Date(paste(fechas[1],"-01",sep = "")),as.Date(paste0(year[1],"-",month,"-01")),by="1 month")-1
  fecha2 <- as.character(fecha2[-1])

  fechas <- sort(c(fecha1,fecha2))
  df1 <- sapply(fechas,fechas_inflacion)
  datos <- c()
  for(i in seq(1,length(colnames(df1)),1)){
    datos <- c(datos,mapply(inflacion1,df1[,i],rep(colnames(df1)[i],length(df1[,i]))))
  }
  df <- data.frame(matrix(datos,ncol = 4),row.names = c('Mensual','Trimestral','Semestral','Anual'))
  colnames(df) <- fechas

  return(df)
}
