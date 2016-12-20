crialogging<- function(file_log = "erros.log"){
  library("logging")
  basicConfig(level='INFO')
  addHandler(writeToFile, file= file_log, level='INFO')
  removeHandler('writeToConsole')
  removeHandler('basic.stdout')
}

cria_env <- function(num){
  log_env <<- new.env(parent = emptyenv())
  attr(log_env, "name") = "log_env"
  log_env$erros <- rep_len("", length.out = num)
}

log_numero_caracters_invalido <- function(nr_caracters){
  log_env$erros[log_env$nr_line] <- sprintf('Possui %d caracters. Insuficiente', nr_caracters)
}

log_digito_errado <- function(digito){
  log_env$erros[log_env$nr_line] <- sprintf('Digito %d errado', digito)
}
rm_erros<- function(){
  log_env$erros <- NULL
}
