#' Correct Brazilian cpf document.
#'
#' \code{trada_cpf} The aim is transform the entrada to the pattern of Brazilian document cpf.
#'                  It must have 11 numeric characters. When a dataset is read as a numeric,
#'                  it lost the 0 character on the left. This function fix that lost and transform
#'                  entrada to character.
#'
#'
#' @param entrada Character or numeric of the document that will be adjusted.
#'
#' @return Entrada adjusted.
#' @import stringr
#'
#' @examples
#'    trata_cpf(52145795)
#' @export
#'
trata_cpf <- function(entrada){
  stringr::str_pad(entrada,11, side = "left", pad = "0")
}



