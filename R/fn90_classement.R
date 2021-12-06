#' classement
#'
#' @param x un vecteur numerique
#'
#' @return un vecteur numerique
#' @export
#'
#' @examples
#' fn90_classement(c(5,4,6,8,1,3))
fn90_classement <-
  function(x) {
    return(rank(x, na.last = TRUE, ties.method = c("max")))
  }
