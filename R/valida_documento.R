#' Check brazilian documents.
#'
#' \code{valida_doc} returns true or false if the number of document is correct.
#'
#'
#' @param entrada Caracter or numeric of the document that will be validated.
#' @param type Caracter, it could be cpf, cnpf, pis e titulo de eleitor.
#' @param log Output erros found on entrada
#'
#' @importFrom Rcpp sourceCpp
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
      result <- valida_ra(entrada, type, log)
    }else{
      result <- valida_cpf_log(entrada)
    }
  }
  if(type == "cnpj"){
    if(log == FALSE){
      result <- valida_cnpj(entrada)
    }else{
      result <- valida_cnpj_log(entrada)
    }
  }
  if(type == "pis"){
    if(log == FALSE){
      result <- valida_ra(entrada, type, log)
    }else{
      result <- valida_pis_log(entrada)
    }
  }
  if(type == "tituloeleitor"){
    if(log == FALSE){
      result <- verificar_titulo_eleitor(entrada)
    }else{
      result <- verificar_titulo_eleitor(entrada, log = TRUE)
    }
  }
  result
}

#' Check brazilian documents.
#'
#' \code{valida_doc_df} returns true or false if the number of document is correct.
#'
#'
#' @param data Data frame with data to be validate.
#' @param column Column of data that will be evaluated.
#' @param type Caracter, it could be cpf, cnpf, pis e titulo de eleitor.
#' @param log Output erros found on entrada
#'
#' @return True or False. if log is giving return a data frame.
#'
#' @export
#'
valida_doc_df <- function(data, column, type = "cpf", log = FALSE){
  arguments <- as.list(match.call())
  column = eval(arguments$column, data)
  valida_doc(column, type = type, log = log)
}

#' Check brazilian documents.
#'
#' \code{tabulacaoDOC} returns a data frame that summarize the output of validaDOC.
#'                     It is require a file where the data is located or the data itself.
#'                     If neither, input and data, is giving the function stop. Also,
#'                     columns and types must the same size.
#'
#' @param input_file file where the data is located.
#' @param data Data frame with data to be validate.
#' @param columns Columns of data that will be evaluated.
#' @param types Caracter, it could be cpf, cnpf, pis e titulo de eleitor.
#'
#' @return return a data frame.
#'
#' @export
#'
tabulacaoDOC <- function(input_file = NULL,data = NULL, columns, types){
  require("data.table")
  if(length(columns) != length(types)){
    stop("Columns and types have to be of the same size.")
  }

  if(is.null(input_file) & is.null(data)){
    stop("You have to give a input_file or a data")
  }
  if(!is.null(input_file) & is.null(data)){
    if(file.exists(input_file)){
      data <- data.table::fread(input_file)
    }else{
      stop("It is not possible to read the input_file")
    }
  }
  diagnostica_RA(data, nomes_colunas = columns, types = types)
}

#' Check brazilian documents.
#'
#' \code{relatorioDOC} output on pdf a data frame that summarize the output of validaDOC.
#'                     It is require a file where the data is located or the data itself.
#'                     If neither, input and data, is giving the function stop. Also,
#'                     columns and types must the same size.
#'
#' @param input_file file where the data is located.
#' @param data Data frame with data to be validate.
#' @param columns Columns of data that will be evaluated.
#' @param types Caracter, it could be cpf, cnpf, pis e titulo de eleitor.
#' @param output_filename character, where the output will be save.
#' @param tipo_relatorio character, grafico ou tabela, indicates the kind of output
#' @return return a data frame.
#'
#' @export
#'
relatorioDOC <- function(input_file = NULL, data = NULL, columns, types, output_filename, tipo_relatorio = "tabela"){
    require("rmarkdown")
    if(file.exists(output_filename)){
      warning("The file already exists so it will overwrite")
    }
    resultado <- tabulacaoDOC(input_file = input_file,data = data, columns = columns, types = types)
    if(tipo_relatorio == "tabela"){
      render("R/resultado.Rmd",output_file = basename(output_filename), output_dir = dirname(output_filename), encoding = "utf-8")
    }else{
      rmarkdown::render("R/resultadoGrafico.Rmd",output_file = basename(output_filename), output_dir = dirname(output_filename), encoding = "utf-8")
    }
}


