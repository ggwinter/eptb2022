#' Difference entre deux annees
#'
#' @param x une table avec une colonne annee et une colonne pour x
#' @param annee une colonne annee
#'
#' @return
#' @importFrom dplyr lag
#' @export
#'
fn96_diff_an <- function(x, annee) {
  x - dplyr::lag(x, order_by = annee) #default = first(x), remplacer le NA par 0
}

