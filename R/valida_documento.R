gerar_vetor_verificacao_cpf <- function(digito){
  c(digito:2, rep(0, 12 - digito))
}

gerar_vetor_verificacao_cnpj <- function(digito){
  c(seq(digito-8,2), 9:2, rep(0, 15-digito))
}

gerar_vetor_verificacao_pis <- function(digito){
  c(3,2, 9:2, 0)
}

verifica_digito_pessoa <- function(entrada, digito, vetor_de_verificacao, log = FALSE){
  entrada <- as.numeric(entrada)
  digito_verificador <- entrada[digito]
  vetor_de_validacao <- vetor_de_verificacao(digito)
  resultado <- (sum(entrada * vetor_de_validacao)*10) %% 11
  if(resultado == 10) resultado <- 0
  if(digito_verificador == resultado){
    saida <- TRUE
  }else{
    saida <- FALSE
    if(log == TRUE) log_digito_errado(digito)
  }
  saida
}

valida_id <- function(entrada, tamanho, primeiro_digito, segundo_digito, vetor_de_verificacao, log = FALSE){
  if(is.integer(entrada) | is.numeric(entrada)){
    entrada <- sprintf("%011.0f", entrada)
  }else{
    if(is.factor(entrada)){
      entrada <- as.character(entrada)
    }
    if(grepl("[^0-9]", entrada) == TRUE){
      entrada<- gsub("[^0-9]", "", entrada)
    }
  }
  entrada_separada<- unlist(strsplit(entrada, ""))
  tamanho_entrada <- length(entrada_separada)
  if(tamanho_entrada != tamanho){
    if(log == TRUE)log_numero_caracters_invalido(tamanho_entrada)
    saida <- FALSE
  }else{
    saida <- verifica_digito_pessoa(entrada_separada, primeiro_digito, vetor_de_verificacao, log = log )
    if(saida == TRUE & is.numeric(segundo_digito)){
      saida <- verifica_digito_pessoa(entrada_separada, segundo_digito, vetor_de_verificacao, log = log)
    }
  }
  saida
}

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
  num <- length(entrada)
  result <- rep_len(FALSE, length.out = num)
  cria_env(num)
  if(type == "cpf"){
    for(i in 1:num){
      log_env$nr_line <- i
      result[i] <- valida_id(entrada[i], 11, 10, 11, gerar_vetor_verificacao_cpf, log = log )
    }
  }
  if(type == "cnpj"){
    for(i in 1:num){
      log_env$nr_line <- i
      result[i] <- valida_id(entrada[i], 14, 13, 14, gerar_vetor_verificacao_cnpj, log = log )
    }
  }
  if(type == "pis"){
    for(i in 1:num){
      log_env$nr_line <- i
      result[i] <- valida_id(entrada[i], 11, 11, FALSE, gerar_vetor_verificacao_pis, log = log )
    }
  }
  if(type == "tituloeleitor"){
    for(i in 1:num){
      log_env$nr_line <- i
      result[i] <- verificar_titulo_eleitor(entrada[i])
    }
  }
  if(log == TRUE) result <- data.frame("dado" = entrada, "resultado" = result, "erros" = log_env$erros)
  rm(log_env, envir = as.environment(1))
  result
}

valida_documento_df <- function(y, data)
{
  arguments <- as.list(match.call())
  y = eval(arguments$y, data)
  valida_doc(y)
}



