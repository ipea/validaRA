verifica_digito_cpf <- function(entrada, digito){
  entrada <- as.numeric(unlist(strsplit(entrada, "")))
  digito_verificador <- entrada[digito]
  vetor_de_validacao <- c(digito:2, rep(0, 12 - digito))
  resultado <- (sum(entrada * vetor_de_validacao)*10) %% 11
  if(digito_verificador == resultado){
    saida <- TRUE
  }else{
    saida <- FALSE
  }
  saida
}

valida_cpf <- function(entrada, saida){
  if(is.integer(entrada) | is.numeric(entrada)){
    if(length(entrada) == 11){
      entrada <- paste(entrada, collapse = "")
    }
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

valida_documento <- function(entrada, saida, type = "cpf"){
  if(type == "cpf"){
    result<-valida_cpf(entrada, saida)
  }
  eval.parent(saida <- result)
}

