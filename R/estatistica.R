estatisticas_amostra <- function(dados, columnName,  type = "cpf"){
  erros <- NULL
  setDT(dados)
  dados_validados <- valida_doc(dados[[columnName]], type = type, log = TRUE)
  setDT(dados_validados)
  log <- dados_validados[, .N, by=erros]
  #t <- data.table("Corretos"=0,"Primeiro digito errado"=1,"Segundo Digito errado"=2,"Todos os digitos iguais"=3)
  #log[, erros:=names(t)[match(erros, t)]]
  return(log)

}


diagnostica_RA <- function(df, nomes_colunas, types){
  if(length(types) != length(nomes_colunas)){
    stop("Types e nomes_colunas devem ter o mesmo tamanho")
  }
  todas <- mapply(function(column, type){
    result <- estatisticas_amostra(df, column, type)
    result[, type:= type]
    return(result)
  }, nomes_colunas, types, SIMPLIFY = F)
  todas <- rbindlist(todas)
  return(todas)
}
