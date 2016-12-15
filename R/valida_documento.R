verifica_digito_cpf <- function(entrada, digito){
  entrada <- as.numeric(unlist(strsplit(entrada, "")))
  digito_verificador <- entrada[digito]
  vetor_de_validacao <- c(digito:2, rep(0, 12 - digito))
  resultado <- (sum(entrada * vetor_de_validacao)*10) %% 11
  if(resultado == 10) resultado <- 0
  if(digito_verificador == resultado){
    saida <- TRUE
  }else{
    saida <- FALSE
  }
  saida
}

verifica_digito_cnpj <- function(entrada, digito){
  entrada <- as.numeric(unlist(strsplit(entrada, "")))
  digito_verificador <- entrada[digito]
  vetor_de_validacao <- c(seq(digito-8,2), 9:2, rep(0, 15-digito))
  resultado <- (sum(entrada * vetor_de_validacao)*10) %% 11
  if(resultado == 10) resultado <- 0
  if(digito_verificador == resultado){
    saida <- TRUE
  }else{
    saida <- FALSE
  }
  saida
}

valida_cpf <- function(entrada){
  if(is.integer(entrada) | is.numeric(entrada)){
    entrada <- as.character(entrada)
  }
  entrada<- gsub("[^0-9]", "", entrada)
  if(length(unlist(strsplit(entrada, ""))) != 11){
    saida <- FALSE
  }else{
    saida <- verifica_digito_cpf(entrada, 10)
    if(saida == TRUE){
      saida <- verifica_digito_cpf(entrada, 11)
    }
  }
  saida
}

valida_cnpj <- function(entrada){
  if(is.integer(entrada) | is.numeric(entrada)){
    entrada <- as.character(entrada)
  }
  entrada<- gsub("[^0-9]", "", entrada)
  if(length(unlist(strsplit(entrada, ""))) != 14){
    saida <- FALSE
  }else{
    saida <- verifica_digito_cnpj(entrada, 13)
    if(saida == TRUE){
      saida <- verifica_digito_cnpj(entrada, 14)
    }
  }
  saida
}

valida_documento <- function(entrada, type = "cpf"){
  num <- length(entrada)
  result <- rep_len(FALSE, length.out = num)
  if(type == "cpf"){
    for(i in 1:num){
      result[i] <- valida_cpf(entrada[i])
    }
  }
  if(type == "cnpj"){
    for(i in 1:num){
      result[i] <- valida_cnpj(entrada[i])
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

