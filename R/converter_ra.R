#' Check brazilian documents.
#'
#' \code{converter_ra} returns a brazilian document without special characters and as a numeric.
#'
#'
#' @param entrada Caracter or numeric of the document that will be validated.
#'
#' @return doc converted.
#'
#' @export
#'
converter_ra <- function(entrada){
  converter_ra_(entrada)
}

#' Check brazilian documents.
#'
#' \code{doc2integer64} returns a brazilian document without special characters and as a numeric.
#'
#'
#' @param entrada Caracter or numeric of the document that will be validated.
#'
#' @return a bit64 integer
#' @import bit64
#'
#' @export
#'
doc2integer64 <- function(entrada){
  require(bit64)
  converter_ra_(entrada)
}
