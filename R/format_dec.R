#' Formate les nombres decimaux au format texte
#'
#' @param x vecteur
#'
#' @return
#' @export
#'
#' @examples
#' format_dec(102.65)
format_dec <- function(x) {
  return(format(x,
                decimal.mark = ",",
                big.mark = "."))
}
