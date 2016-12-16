gerar_vetor_verificacao_cpf <- function(digito){
  c(digito:2, rep(0, 12 - digito))
}

gerar_vetor_verificacao_cnpj <- function(digito){
  c(seq(digito-8,2), 9:2, rep(0, 15-digito))
}

verifica_digito_pessoa <- function(entrada, digito, vetor_de_verificacao){
  entrada <- as.numeric(unlist(strsplit(entrada, "")))
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
  if(length(unlist(strsplit(entrada, ""))) != tamanho){
    saida <- FALSE
  }else{
    saida <- verifica_digito_pessoa(entrada, primeiro_digito, vetor_de_verificacao )
    if(saida == TRUE){
      saida <- verifica_digito_pessoa(entrada, segundo_digito, vetor_de_verificacao)
    }
  }
  saida
}

valida_documento <- function(entrada, type = "cpf"){
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
  result
}

valida_documento_df <- function(y, data)
{
  arguments <- as.list(match.call())
  y = eval(arguments$y, data)
  valida_documento(y)
}

