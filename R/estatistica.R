estatisticas_amostra <- function(df, columnName,  type = "cpf"){
  require(data.table)
  library(data.table)
  setDT(df)
  dados_validados <- valida_doc(df[, columnName, with, F], type = type, log = TRUE)
  setDT(dados_validados)
  return(sumarizando_dados(dados_validados))

}
sumarizando_dados <- function(dados){
  log <- dados[, .N, by=erros]
  print(log)
  t <- data.table("Corretos"=0,"Primeiro digito errado"=1,"Segundo Digito errado"=2)
  log[, erros:=names(t)[match(erros, t)]]
  return(log)
}

diagnostica_RA <- function(df, nomes_colunas, types){
  if(length(types) != length(nomes_colunas)){
    stop("Types e nomes_colunas devem ter o mesmo tamanho")
  }
  estatistica <- data.frame(tipo = character(0),
                            validos = integer(0),
                            invalidos = integer(0),
                            sem_character = integer(0),
                            characters_invalido = integer(0),
                            zeros = integer(0),
                            characters_iguais = integer(0),
                            primeiro_digito_invalido = integer(0),
                            segundo_digito_invalido = integer(0)
                            )

  todas <- mapply(function(column, type){
    result <- estatisticas_amostra(df, column, type)
    result[, type:= type]
    retun(result)
  }, nomes_colunas, types)
  todas <- rbindlist(todas)
  return(todas)
  # estatistica$tipo <- as.character(estatistica$tipo)
  # for(i in 1:length(nomes_colunas)){
  #   x <- estatisticas_amostra(df, nomes_colunas[i], type = types[i])
  #   x["tipo"] <- as.character(types[i])
  #   estatistica <- dplyr::bind_rows(estatistica, x)
  # }
  # #estatistica <- estatistica[c(7, 1, 2, 3, 4, 5, 7,8)]
  # estatistica
}
