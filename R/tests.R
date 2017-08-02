#' @title Greet someone
#' @description Receives a name a greets the person
#' @param name a string with the name of whom to greet
#' @return a string with the greeting
#' @export
greet <- function(name = "") {
  paste("hello", name)
}

#' @title Say goodbye
#' @description Receives a name and says goodbye
#' @param name a string
#' @return a string with the goodbye
#' @export
goodbye <- function(name = "") {
  paste("goodbye", name)
}
