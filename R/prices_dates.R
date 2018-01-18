devtools::use_package("dplyr")
devtools::use_package("magrittr")
devtools::use_package("RMySQL")
devtools::use_package("tidyr")
devtools::use_package("DBI")
devtools::use_package("stats")

#'@import dplyr
#'
#'@export
magrittr::'%>%'

#' @title Get Prices
#' @description Function that does a query from a mysql database in order to obtain prices of instruments.
#' @param fecha is a date vector which can be build with seq_Date() function.
#' @param id is the id vector of which the prices must be obtained.
#' @return a dataframe with dates and prices as requested.
#' @export
get_prices <- function(fecha = NULL, id = NULL){
  con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="127.0.0.1",user="root", password="CIBANCO.00", dbname="mydb")
  query <- paste("SELECT fecha, id, Precio_limpio FROM prices")
  if(!is.null(fecha) | !is.null(id)) {
    query <- paste(query, "WHERE")
    if(!is.null(fecha)) {
      query <- paste0(query, " fecha IN ('", paste(fecha, collapse = "','"), "')")
      if (!is.null(id)) {
        query <- paste(query, "AND")
      }
    }
    if(!is.null(id)) {
      query <- paste0(query, " id IN ('", paste(id, collapse="','"), "')")
    }
  }
  precios <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  precios %>%
    #as date
    mutate(fecha = as.Date(fecha)) %>%
    #repo
    tidyr::spread(id, Precio_limpio) %>%
    data.frame(check.names = FALSE)
}

#' @title Sequence of Dates
#' @description Function that obtains a sequence of dates.
#' @param fecha is a character: "20150101/" makes a sequence of dates begining at 2015-01-01, "20150101/20160101" makes a sequence of dates begining at 2015-01-01 ending at 2016-01-01
#' @return a dataframe with dates and prices as requested.
#' @export
seq_Date <- function(fecha = NULL){
  fechas <- NULL
  first <- as.Date(substring(fecha,1,8),format='%Y%m%d')
  last <- as.Date(substring(fecha,10,17),format='%Y%m%d')
  if(is.na(last) == TRUE){fechas <- seq.Date(first,Sys.Date(),1)}
  else {fechas <- seq.Date(first,last,1)}
  return(fechas)
}

#' @title Get Bonds
#' @description Function that does a query from a mysql database in order to obtain characteristics of bonds.
#' @param id is the id vector of which bonds information must be obtained.
#' @return a dataframe with Issuing date, Maturity date, Coupon rate, Type of rate, Spread and Grade of the selected Bonds.
#' @export
get_bonds <- function(id = NULL){
  con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="127.0.0.1",user="root", password="CIBANCO.00", dbname="mydb")
  query <- paste("SELECT id,FechaEmision,FechaVencimiento,TasaCupon,TipoTasa,SobreTasa,Frecuencia FROM bonds ")
  if(!is.null(id)) {
    query <- paste0(query, " WHERE ", " id IN ('", paste(id, collapse="','"),"')")
    }
  datos <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  datos
}

#' @title Get Rates
#' @description Function that does a query from a mysql database in order to obtain the node of some rates: CETES-28,CETES-91,CETES-182,CETES-364,Fondeo-BancarioUS,FondeoBancarioMX,LIBOR-1m,LIBOR-3m,LIBOR-6m,LIBOR-12m,Tasa-FED,Tasa-Banxico,TIIE-28,TIIE-91,TIIE-182,Treasury-2y,Treasury-5y,Treasury-10y,UDI.
#' @param fecha is the dates vector of which the nodes are needed.
#' @param id is the id vector of which nodes information must be obtained.
#' @return a dataframe with nodes id, date and value.
#' @export
get_rates <- function(fecha = NULL, id = NULL){
  con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="127.0.0.1",user="root", password="CIBANCO.00", dbname="mydb")
  query <- paste("SELECT id,fecha,nivel FROM tasas")
  if(!is.null(fecha) | !is.null(id)) {
    query <- paste(query, "WHERE")
    if(!is.null(fecha)) {
      query <- paste0(query, " fecha IN ('", paste(fecha, collapse = "','"), "')")
      if (!is.null(id)) {
        query <- paste(query, "AND")
      }
    }
    if(!is.null(id)) {
      query <- paste0(query, " id IN ('", paste(id, collapse="','"), "')")
    }
  }
  niveles <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  niveles %>%
    #as date
    mutate(fecha = as.Date(fecha)) %>%
    #repo
    tidyr::spread(id, nivel) %>%
    data.frame(check.names = FALSE)
}

#' @title Get Indices
#' @description Function that does a query from a mysql database in order to obtain a level of an index.
#' @param fecha is the dates vector of which the nodes are needed.
#' @param id is the id vector of which the level information must be obtained.
#' @return a dataframe with nodes id, date and value.
#' @export
get_indices <- function(fecha = NULL, id = NULL){
  con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="127.0.0.1",user="root", password="CIBANCO.00", dbname="mydb")
  query <- paste("SELECT id,fecha,nivel FROM indices")
  if(!is.null(fecha) | !is.null(id)) {
    query <- paste(query, "WHERE")
    if(!is.null(fecha)) {
      query <- paste0(query, " fecha IN ('", paste(fecha, collapse = "','"), "')")
      if (!is.null(id)) {
        query <- paste(query, "AND")
      }
    }
    if(!is.null(id)) {
      query <- paste0(query, " id IN ('", paste(id, collapse="','"), "')")
    }
  }
  niveles <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  niveles %>%
    #as date
    mutate(fecha = as.Date(fecha)) %>%
    #repo
    tidyr::spread(id, nivel) %>%
    data.frame(check.names = FALSE)
}
