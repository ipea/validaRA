estatisticas_amostra <- function(df, columnName,  type = "cpf"){
  library("dplyr")
  if(is.character(columnName) & (columnName %in% colnames(df))){
    columnName <- df[,columnName]
  }else{
    arguments <- as.list(match.call())
    columnName <- eval(arguments$columnName, df)
  }
  result <- valida_doc(columnName, type = type, log = TRUE)
  vazio <- nrow(dplyr::filter(result, grepl("Sem caracters", erros)))
  tamanho_errado <- nrow(dplyr::filter(result, grepl("Insuficiente", erros)))
  digito_invalido <- nrow(dplyr::filter(result, grepl("Digito", erros)))
  estatistica <- data.frame(tamanho_errado = tamanho_errado, digito_invalido = digito_invalido, vazio = vazio )
  estatistica
}

diagnostica_RA <- function(df, nomes_colunas, types){
  if(length(types) != length(nomes_colunas)){
    stop("Types e nomes_colunas devem ter o mesmo tamanho")
  }
  estatistica <- data.frame(tamanho_errado = integer(0), digito_invalido = integer(0), vazio = integer(0), tipo = character(0))
  estatistica$tipo <- as.character(estatistica$tipo)
  for(i in 1:length(nomes_colunas)){
    x <- estatisticas_amostra(df, nomes_colunas[i], type = types[i])
    x["tipo"] <- as.character(types[i])
    estatistica <- dplyr::bind_rows(estatistica, x)
  }
  estatistica <- estatistica[c(4, 1, 2, 3)]
  estatistica
}
