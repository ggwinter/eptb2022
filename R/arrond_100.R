#' arrondi a la centaine.
#'
#' @param x vecteur numerique
#'
#' @return vecteur numerique
#' @export
#'
#' @examples
#' arrond_100(1198L)
arrond_100 <- function(x) {
  return(100 * round(x / 100, digits = 0))
}
