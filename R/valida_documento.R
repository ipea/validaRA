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
    if(log == FALSE){
      result <- valida_pis(entrada)
    }else{
      result <- valida_pis_log(entrada)
    }
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

#' Check brazilian documents.
#'
#' \code{relatorioDOC} returns true or false if the number of document is correct.
#'
#'
#' @param data Data frame with data to be validate.
#' @param columns Column of data that will be evaluated.
#' @param types Caracter, it could be cpf, cnpf, pis e titulo de eleitor.
#'
#' @return True or False. if log is giving return a data frame.
#'
#' @export
#'
relatorioDOC <- function(base = NULL,data = NULL, columns, types){
  if(length(columns) != length(types)){
    stop("Columns and types have to be of the same size.")
  }
  require(data.table)
  #ifelse(is.null(data) & !is.null(base) , data <- fread(base), stop("You did not pass data to the function"))
  estatistica <- diagnostica_RA(data, nomes_colunas = columns, types = types)
  estatistica

}

#' Check brazilian documents.
#'
#' \code{graficoDOC} returns true or false if the number of document is correct.
#'
#'
#' @param data Data frame with data to be validate.
#' @param columns Column of data that will be evaluated.
#' @param types Caracter, it could be cpf, cnpf, pis e titulo de eleitor.
#'
#' @return True or False. if log is giving return a data frame.
#'
#' @export
#'
graficoDOC <- function(base = NULL,data = NULL, columns, types, filename){
    if(file.exists(filename)){
      warning("The file already exists so it will overwrite")
    }
    resultado <- relatorioDOC(base = base,data = data, columns = columns, types = types)
    render("R/resultado.Rmd",output_file = basename(filename), output_dir = dirname(filename))

}


