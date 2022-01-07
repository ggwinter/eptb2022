#' Choix annee etude
#'
#' La fonction enregistre dans une liste l'annee d'etude et calcule l'annee
#' precedente au format texte
#'
#' @param annee un vecteur numerique de longueur 4 caracteres
#' @examples
#' fn02_choix_annee_etude(2018)
#'
#' @export
fn01_choix_annee_etude <- function(annee = 2019) {
  stopifnot(is.numeric(annee))
  stopifnot(nchar(annee)== 4)

  list(
<<<<<<< HEAD:R/fn02_choix_annee_etude.R
    'annee_etude' = as.character(x),
    'annee_prec' = as.character(x - 1L),
=======
    'annee_etude' = as.character(annee),
    'annee_prec' = as.character(annee- 1L),
>>>>>>> test:R/fn01_choix_annee_etude.R
    'annee_debut' = "2010"
  )-> ls_dates
  return(ls_dates)
}
