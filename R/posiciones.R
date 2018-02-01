#' @title Fetch holdings data
#' @description Construct a mysql query for the selected dates and contrato and sends it to the sql database posiciones
#' @details This function will return a data.frame with holdings information from one or several dates
#' When no contrato is given, it will fetch the information of every contrato
#' @param fecha A vector of dates that can be of class character or Date but is expected to be in the format aaaa-mm-dd
#' @param contrato A character or integer vector with the number of contratos
#' @return A data.frame with the holding of the given contratos in the selected dates. If no contrato is given, it will return the information of every contrato.
#' @examples
#' get_position("2015-12-31")
#' get_position("2015-12-31", contrato = "25774")
#' @export
get_position <- function(fecha, contrato = NULL){

  con <- DBI::dbConnect(RMySQL::MySQL(), host='127.0.0.1', username="root", password="CIBANCO.00", dbname="mydb")

  query <- paste("select * from posiciones where ",
                 "fecha in ('",
                 paste(lubridate::ymd(fecha), collapse = "','"),
                 "') ",
                 sep="")

  if(!is.null(contrato)) {
    query <- paste(query,
                   "and contrato in ('",
                   paste(contrato, collapse ="','"),
                   "') ",
                   sep="")
  }
  pos <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  pos[pos$tit != 0, ]
}


#' @title Summary statistics for holdings data
#' @description Compute basic summary statistics for holdings data for each strategy, date and id
#' @param position A data.frame object containing holdings information. It is assumed to be generated using \code{\link{get_position}}
#' @param groups  character vector of the names of the variables that will be used with grouping and computing statistics
#' Variable names must be quoted (an inconvenience used to avoid confusion with the commands passed to \code{\link{summarize}}).
#' If no grouping is desired then it can be set to NULL
#' @param ... Commands to be passed to \code{\link{summarize}} from \code{\link{dplyr}} for computing statistics within groups
#' #' @details The function will group by investment strategy (carteramodelo), date and asset id and compute
#' basic summary statistics and any other command specified by the user.
#' @return A data.frame object with the summary statistics. By default it returns a data.frame indicating the group and columns
#' \itemize{
#'   \item{\strong{sum_mon}:} {The sum of the valuation of the group}
#'   \item{\strong{perc_median}:} {The median of the percentage of the total valuation}
#'   \item{\strong{perc_mean}:} {The mean of the percentage of the total valuation}
#'   \item{\strong{perc_sd}:} {The standard deviation of the percentage of the total valuation}
#'   \item{}{The result of additional commands supplied by the user}
#' }
#' @examples
#' position <- get_position(fecha = c("2016-07-28", "2016-07-29"))
#' summarize_position(position, groups = c("fecha", "carteramodelo", "id"), count = length(contrato))
#' summarize_position(position, groups = NULL)
#' @export
summarize_position <- function(position, groups = NULL, ...) {
  position <- position %>%
    tidyr::complete(
      tidyr::nesting(fecha, contrato, tot),
      tidyr:: nesting(id, reporto, tipo, emisora, serie),
      fill = list(tit=0, mon = 0)
    ) %>%
    as.data.frame()
  if (!is.null(groups)) {
    position <- position %>%
      group_by_(.dots = lapply(paste0("~", groups), as.formula))
  }
  position %>%
    summarize(...)  %>%
    as.data.frame()
}


#' @title returns of the total valuation per contrato
#' @description Create return data per contract for each contrato
#' @details This function takes a position data.frame and uses it to compute the return of the contrato by taking the percent change
#' of each date's valuation and its previous valuation indicated by the column named \code{tot}.
#' @param position A position data.frame object assumed to be generated using \code{\link{get_position}}
#' @param cumulative A boolean indicated whether returns are cumulative or daily. Defaults to FALSE
#' @return A \code{\link{xts}} (time series) object indexed by the date and with each column representing the daily return of the contrato.
#' Returns are give in the form of valuation ratio (final_price/initial_price)
#' @examples
#' pos <- get_position(c("2015-12-31", "2016-01-04", "2016-01-05"), contrato = c("25774", "25773"))
#' get_position_returns(pos)
#' @export
get_contrato_returns <- function(position, cumulative = FALSE) {
  tot_df <- position %>%
    select(fecha, contrato, tot) %>%
    distinct() %>%
    tidyr::spread(contrato, tot)
  tot_df <- xts::xts(tot_df[ ,-1], order.by = as.Date(tot_df$fecha))
  ret_df <- tot_df/xts::lag.xts(tot_df)
  if (cumulative) {
    coredata(ret_df) <- apply(coredata(ret_df), 2, function(row) row <- row / ret_df[1, ])
  }
}

#' @title DiaH
#' @description Function that tells if the given date is business date or returns the last business date.
#' @param fecha is a date in %Y-%m-%d format.
#' @param tipo "Last" returns the last business day, any other value returns if the given date is business date.
#' @return a dataframe with dates and prices as requested.
#' @export
diah <-  function(fecha, tipo = "Habil"){
  load(file = ".RData",envir = globalenv())
  festivos$dias <- as.Date(festivos$dias,format="%d/%m/%Y")
  fechabase0 <- as.Date("2017-08-06")
  if(tipo == "Last"){
    if(as.integer(fecha - fechabase0 ) %% 7 == 6 | as.integer(fecha - fechabase0 ) %% 7 == 0 | fecha %in% festivos$dias){
      return(diah(fecha-1))
    } else {
        return(fecha)
      }
  } else {
    dia <- "Habil"
    if(as.integer(fecha - fechabase0 ) %% 7 == 6){dia <- "Inhabil"}
    if(as.integer(fecha - fechabase0 ) %% 7 == 0){dia <- "Inhabil"}
    if(fecha %in% festivos$dias){dia <- "Inhabil"}
    return(dia)
  }
}
