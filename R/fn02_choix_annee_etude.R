#' Choix annee etude
#'
#' La fonction enregistre dans une liste l'annee d'etude et calcule l'annee
#' precedente au format texte
#'
#' @param x un vecteur numerique de longueur 4 caracteres
#' @examples
#' fn02_choix_annee_etude(2018)
#'
#' @export
fn02_choix_annee_etude <- function(x = 2019) {
  stopifnot(is.numeric(x))
  stopifnot(nchar(x)== 4)

  list(
    'annee_etude' = as.character(x),
    'annee_prec' = as.character(x- 1L),
    'annee_debut' = "2010"
  )-> ls_dates
  return(ls_dates)
}
