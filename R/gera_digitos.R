#' Check brazilian documents.
#'
#' \code{gera_digito} generate the last digit of a id.
#'
#'
#' @param entrada Caracter or numeric of the document that will be validated.
#' @param type Caracter, it could be cpf, cnpf, pis e titulo de eleitor.
#'
#' @useDynLib validaRA, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @return it will alter the number by reference.
#'
#' @examples
#'  gera_digito("529.982.247-2", type = "cpf")
#'  gera_digito("60.149.443/0001-7", type = "cnpj")
#' @export
#'
gera_digito <- function(entrada, type = "cpf"){
  generate_digit(entrada, type)
}

