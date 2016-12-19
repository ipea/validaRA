verificar_titulo_eleitor <- function(entrada){
  if(is.integer(entrada) | is.numeric(entrada)){
    entrada <- as.character(entrada)
  }
  entrada<- gsub("[^0-9]", "", entrada)
  entrada <- as.numeric(unlist(strsplit(entrada, "")))
  tamanho_entrada <- length(entrada)
  vetor_de_validacao_sequencial <- c(seq(14 - tamanho_entrada, 9), rep(0,4))
  digito_verificador <- entrada[tamanho_entrada - 1]
  resultado <- sum(entrada * vetor_de_validacao_sequencial) %% 11
  if(resultado == 10) resultado <- 0
  if(resultado == digito_verificador){
    saida <- TRUE
  }else{
    saida <- FALSE
  }
  if(saida == TRUE){
    vetor_de_validacao_sequencial <- c(rep(0,tamanho_entrada-4), 7:9, 0)
    digito_verificador <- entrada[tamanho_entrada]
    resultado <- sum(entrada * vetor_de_validacao_sequencial) %% 11
    if(resultado == 10) resultado <- 0
    if(resultado == digito_verificador){
      saida <- TRUE
    }else{
      saida <- FALSE
    }
  }
  saida
}
gerar_vetor_verificacao_cpf <- function(digito){
  c(digito:2, rep(0, 12 - digito))
}

gerar_vetor_verificacao_cnpj <- function(digito){
  c(seq(digito-8,2), 9:2, rep(0, 15-digito))
}

gerar_vetor_verificacao_pis <- function(digito){
  c(3,2, 9:2, 0)
}

verifica_digito_pessoa <- function(entrada, digito, vetor_de_verificacao){
  entrada <- as.numeric(entrada)
  digito_verificador <- entrada[digito]
  vetor_de_validacao <- vetor_de_verificacao(digito)
  resultado <- (sum(entrada * vetor_de_validacao)*10) %% 11
  if(resultado == 10) resultado <- 0
  if(digito_verificador == resultado){
    saida <- TRUE
  }else{
    saida <- FALSE
  }
  saida
}

valida_id <- function(entrada, tamanho, primeiro_digito, segundo_digito, vetor_de_verificacao){
  if(is.integer(entrada) | is.numeric(entrada)){
    entrada <- as.character(entrada)
  }
  entrada<- gsub("[^0-9]", "", entrada)
  entrada_separada<- unlist(strsplit(entrada, ""))
  if(length(entrada_separada) != tamanho){
    saida <- FALSE
  }else{
    saida <- verifica_digito_pessoa(entrada_separada, primeiro_digito, vetor_de_verificacao )
    if(saida == TRUE & is.numeric(segundo_digito)){
      saida <- verifica_digito_pessoa(entrada_separada, segundo_digito, vetor_de_verificacao)
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
#'
#' @return True or False.
#'
#' @examples
#'  valida_doc("529.982.247-25", type = "cpf")
#'  valida_doc("60.149.443/0001-70", type = "cnpj")
#' @export
#'
valida_doc <- function(entrada, type = "cpf"){
  num <- length(entrada)
  result <- rep_len(FALSE, length.out = num)
  if(type == "cpf"){
    for(i in 1:num){
      result[i] <- valida_id(entrada[i], 11, 10, 11, gerar_vetor_verificacao_cpf )
    }
  }
  if(type == "cnpj"){
    for(i in 1:num){
      result[i] <- valida_id(entrada[i], 14, 13, 14, gerar_vetor_verificacao_cnpj )
    }
  }
  if(type == "pis"){
    for(i in 1:num){
      result[i] <- valida_id(entrada[i], 11, 11, FALSE, gerar_vetor_verificacao_pis )
    }
  }
  if(type == "tituloeleitor"){
    for(i in 1:num){
      result[i] <- verificar_titulo_eleitor(entrada[i])
    }
  }
  result
}

valida_documento_df <- function(y, data)
{
  arguments <- as.list(match.call())
  y = eval(arguments$y, data)
  valida_doc(y)
}

