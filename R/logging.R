cria_env <- function(num){
  log_env <<- new.env(parent = emptyenv())
  log_env$erros <- rep_len("", length.out = num)
}

log_numero_caracters_invalido <- function(nr_caracters){
  if(nr_caracters > 0){
    log_env$erros[log_env$nr_line] <- sprintf('Possui %d caracters. Insuficiente', nr_caracters)
  }else{
    log_env$erros[log_env$nr_line] <- "Sem caracters"
  }
}

log_digito_errado <- function(digito){
  log_env$erros[log_env$nr_line] <- sprintf('Digito %d errado', digito)
}

log_todos_caracters_iguais <- function(){
  log_env$erros[log_env$nr_line] <- sprintf('Todos os caracters são iguais')
}
