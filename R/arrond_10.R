#' arrondi à la dizaine.
#'
#' @param x vecteur numerique
#'
#' @return vecteur numerique
#' @export
#'
#' @examples
#' arrond_10(56)
arrond_10 <- function(x) {
  return(10 * round(x / 10, digits = 0))
}
