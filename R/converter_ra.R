#' Convert brazilian documents.
#'
#' \code{doc2integer64} returns a brazilian document without special characters and as an integer64.
#'
#'
#' @param entrada Caracter or numeric that will be converted.
#'
#' @return an bit64 integer
#' @import bit64
#'
#' @export
#'
doc2integer64 <- function(entrada){
  converter_ra_(entrada)
}
