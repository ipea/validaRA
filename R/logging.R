crialogging<- function(file_log = "erros.log"){
  library("logging")
  basicConfig(level='INFO')
  addHandler(writeToFile, file= file_log, level='INFO')
  removeHandler('writeToConsole')
  removeHandler('basic.stdout')
}

cria_env <- function(){
  log_env <<- new.env(parent = emptyenv())
  attr(log_env, "name") = "log_env"
}

set_type <- function(type = "cpf"){
  assign("type", type, envir = log_env)
}

set_line <- function(nr_line){
  assign("nr_line", nr_line, envir = log_env)
}
log_numero_caracters_invalido <- function(nr_caracters){
  type <- get("type", envir = log_env)
  nr_line <- get("nr_line", envir = log_env)
  loginfo('10 %d %s "%s possui numero de caracters %d"', nr_line, type,type, nr_caracters)
}

log_digito_errado <- function(digito, digito_gerado){
  type <- get("type", envir = log_env)
  nr_line <- get("nr_line", envir = log_env)
  loginfo('20 %d %s "o digito do %s e %d foi gerado %d"', nr_line, type,type, digito, digito_gerado)
}
