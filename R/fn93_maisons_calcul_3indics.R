#' Maisons calcule des 3 indicateurs de la plaquette
#'
#' @param data tableau
#'
#' @return tableau modifié
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export
fn93_maisons_calcul_3indics <- function(data) {
  data %>% dplyr::mutate(
    mai_surfmoy_m2  = round(.data$mai_surftot / .data$mai_nb , digits = 0),
    mai_prixmoy  = round(.data$mai_prixtot / .data$mai_nb , digits = 0),
    mai_prixmoy_m2  = round(.data$mai_prixmoy / .data$mai_surfmoy_m2 , digits = 0)
  )-> data
  return(data)
}
