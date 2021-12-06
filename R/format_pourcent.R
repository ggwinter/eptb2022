#' Pourcentage au format texte
#'
#' @param x vecteur
#'
#' @return
#' @export
#'
#' @examples
#' format_pourcent(0.05)
format_pourcent <- function(x) {
  return(sprintf("%.1f %%", 100 * x) %>%
           stringr::str_replace("\\.", ",") %>%
           stringr::str_replace("%", "\\%"))
}
