#' Check brazilian documents.
#'
#' \code{valida_doc} returns true or false if the number of document is correct.
#'
#'
#' @param entrada Caracter or numeric of the document that will be validated.
#' @param type Caracter, it could be cpf, cnpf, pis e titulo de eleitor.
#' @param log Output erros found on entrada
#'
#' @return True or False. if log is giving return a data frame.
#'
#' @examples
#'  valida_doc("529.982.247-25", type = "cpf")
#'  valida_doc("60.149.443/0001-70", type = "cnpj")
#' @export
#'
valida_doc <- function(entrada, type = "cpf", log = FALSE){
  if(!(is.character(entrada) | is.numeric(entrada))){
    stop("Arguments have to be character or numeric")
  }
  if(type == "cpf"){
    if(log == FALSE){
      result <- valida_cpf(entrada)
    }else{
      result <- valida_cpf_log(entrada)
    }
  }
  if(type == "cnpj"){
    result <- valida_cnpj(entrada)
  }
  if(type == "pis"){
    result <- valida_pis(entrada)
  }
  result
}

#' Check brazilian documents.
#'
#' \code{valida_doc} returns true or false if the number of document is correct.
#'
#'
#' @param data Data frame with data to be validate.
#' @param column Column of data that will be evaluated.
#' @param type Caracter, it could be cpf, cnpf, pis e titulo de eleitor.
#' @param log Output erros found on entrada
#'
#' @return True or False. if log is giving return a data frame.
#'
#' @examples
#'  valida_doc("529.982.247-25", type = "cpf")
#'  valida_doc("60.149.443/0001-70", type = "cnpj")
#' @export
#'
valida_doc_df <- function(data, column, type = "cpf", log = FALSE)
{
  arguments <- as.list(match.call())
  column = eval(arguments$column, data)
  valida_doc(column, type = type, log = log)
}



