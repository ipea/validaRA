#' Convert Brazilian documents.
#'
#' \code{doc2integer64} returns a Brazilian document without special characters and as an integer64.
#'
#'
#' @param entrada Character or numeric that will be converted.
#' @import bit64
#' @return an bit64 integer
#'
#' @examples
#'    doc2integer64("086.090.705-65")
#' @export
#'
doc2integer64 <- function(entrada){
  requireNamespace("bit64")
  converter_ra_(entrada)
}
