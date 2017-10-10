#' @import dplyr
#'
#' @export
magrittr::'%>%'

#' @title Get Prices
#' @description Function that does a query from a mysql database in order to obtain prices of instruments.
#' @param fecha is a date vector which can be build with seq_Date() function.
#' @param id is the id vector of which the prices must be obtained.
#' @return a dataframe with dates and prices as requested.
#' @export
get_prices <- function(fecha = NULL, id = NULL){
  con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="127.0.0.1",user="root", password="CIBANCO.00", dbname="mydb")
  query <- paste("SELECT fecha, id, precio FROM prices")
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
  cat(sprintf("fetching query: %s\n", query))
  precios <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  precios %>%
    #as date
    mutate(fecha = as.Date(fecha)) %>%
    #repo
    tidyr::spread(id, precio) %>%
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
