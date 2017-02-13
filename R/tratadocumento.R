#' Correct brazilian cpf document.
#'
#' \code{trada_cpf} The aim is transform the entrada to the pattern of brazilian document cpf.
#'                  It must have 11 numeric characters. When a dataset is read as a numeric,
#'                  it lost the 0 caracter on the left. This function fix that lost and transoform
#'                  entrada to character.
#'
#'
#' @param entrada Caracter or numeric of the document that will be ajusted.
#'
#' @return Entrada adjusted.
#' @import stringr
#'
#' @export
#'
trata_cpf <- function(entrada){
  require(stringr)
  stringr::str_pad(entrada,11, side = "left", pad = "0")
}



