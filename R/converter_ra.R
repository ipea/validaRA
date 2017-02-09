#' Check brazilian documents.
#'
#' \code{converter_ra} returns a brazilian document without special characters and as a numeric.
#'
#'
#' @param entrada Caracter or numeric of the document that will be validated.
#'
#' #' @return True or False. if log is giving return a data frame.
#'
#' @examples
#'  converter_ra("529.982.247-25")
#' @export
#'
converter_ra <- function(entrada){
  converter_ra_(entrada)
}
