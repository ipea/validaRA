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
