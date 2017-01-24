#' Check brazilian documents.
#'
#' \code{trada_cpf} returns true or false if the number of document is correct.
#'
#'
#' @param entrada Caracter or numeric of the document that will be validated.
#' @param type Caracter, it could be cpf, cnpf, pis e titulo de eleitor.
#' @param log Output erros found on entrada
#'
#' @return True or False. if log is giving return a data frame.
#'
#' @export
#'
trata_cpf <- function(entrada){
  require(stringr)
  result <- str_pad(entrada,11, side = "left", pad = "0")
  result
}



