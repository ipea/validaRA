estatisticas_amostra <- function(df, columnName,  type = "cpf"){
  library("dplyr")
  if(is.character(columnName) & (columnName %in% colnames(df))){
    columnName <- df[,columnName]
  }else{
    arguments <- as.list(match.call())
    columnName <- eval(arguments$columnName, df)
  }
  result <- valida_doc(columnName, type = type, log = TRUE)
  characters_invalido <- nrow(dplyr::filter(result, grepl("Numero de caracters invalido", erros)))
  characters_iguais <- nrow(dplyr::filter(result, grepl("Caracters iguais", erros)))
  primeiro_digito_invalido <- nrow(dplyr::filter(result, grepl("Primero digito errado", erros)))
  segundo_digito_invalido <- nrow(dplyr::filter(result, grepl("segundo digito errado", erros)))
  sem_character <- nrow(dplyr::filter(result, grepl("Sem characters", erros)))
  validos <- nrow(dplyr::filter(result, resultado == TRUE))
  invalidos <- nrow(dplyr::filter(result, resultado == FALSE))
  e <- data.frame(characters_iguais = characters_iguais,
                            primeiro_digito_invalido = primeiro_digito_invalido,
                            segundo_digito_invalido = segundo_digito_invalido,
                            characters_invalido = characters_invalido,
                            sem_character = sem_character,
                            validos = validos,
                            invalidos = invalidos)
  return(e)
}

diagnostica_RA <- function(df, nomes_colunas, types){
  if(length(types) != length(nomes_colunas)){
    stop("Types e nomes_colunas devem ter o mesmo tamanho")
  }
  estatistica <- data.frame(characters_iguais = integer(0),
                            primeiro_digito_invalido = integer(0),
                            segundo_digito_invalido = integer(0),
                            characters_invalido = integer(0),
                            sem_character = integer(0),
                            tipo = character(0),
                            validos = integer(0),
                            invalidos = integer(0))
  estatistica$tipo <- as.character(estatistica$tipo)
  for(i in 1:length(nomes_colunas)){
    x <- estatisticas_amostra(df, nomes_colunas[i], type = types[i])
    x["tipo"] <- as.character(types[i])
    estatistica <- dplyr::bind_rows(estatistica, x)
  }
  estatistica <- estatistica[c(6, 1, 2, 3, 4, 5, 7,8)]
  estatistica
}
