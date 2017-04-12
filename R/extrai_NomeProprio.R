#' Check Brazilian names
#'
#' \code{extrai_NomeProprio} Parse brazlian names and returns given names, surnames and gender
#'
#' @param x List, character or factor with names to be parsed.
#' @param sobrenome If TRUE, the list of surnames is returned.
#' @param sexo If TRUE, the list of gender based on the names is returned.
#'
#' importFrom data.table stringr
#' return Returns a data.table
#'
#' @examples
#' extrai_NomeProprio(x = c("Maria Conceicao da Costa", "Mario Silva"), sobrenome = TRUE, sexo = FALSE)
#' @export
#'
extrai_NomeProprio <- function(x, sobrenome = FALSE, sexo = FALSE){

  #Carrega bases necessárias e variáveis--------------------------
  base_nomes <- system.file("extdata", "base_nomes.csv", package = "validaRA")
  patternOneName <- "^[a-zA-Z0-9_]+"
  patternTwoNames <- "^[a-zA-Z0-9_]+\\s[a-zA-Z0-9_]+"
  patternThreeNames <- "^[a-zA-Z0-9_]+\\s[a-zA-Z0-9_]+\\s[a-zA-Z0-9_]+"

  #Trata os dados de entrada --------------------------------------
  names <- setDT(data.frame(nome = toupper(x)))

  #Separa nomes---------------------------------------------------
  names[,dois_primeiros := str_extract(nome,patternTwoNames)]
  names[,tres_primeiros := str_extract(nome,patternThreeNames)]

  names[, NomeProprio := ifelse(tres_primeiros %in% base_nomes$nome, tres_primeiros, ifelse(dois_primeiros %in% base_nomes$nome, dois_primeiros, str_extract(names$nome,patternOneName)))]

  if (sobrenome == TRUE){
    names[,sobrenome := str_trim(str_replace(nome, NomeProprio, ""))]
  }
  if (sexo == TRUE){
    names[,sexo := base_nomes[NomeProprio,,on="nome"][,.(sexo)]]

  }
  names <- names[,-"dois_primeiros"]
  names <- names[,-"tres_primeiros"]
}
