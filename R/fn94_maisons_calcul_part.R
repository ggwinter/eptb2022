#' Maisons calcul de la part de chaque territoire
#' (en nombre de terrains etudies)
#'
#' @param data tableau
#'
#' @return tableau modifié
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export
fn94_maisons_calcul_part <- function(data) {
  data %>% dplyr::mutate(mai_nbt = sum(.data$mai_nb),
                              part = round(.data$mai_nb / .data$mai_nbt, digits = 3))-> data
  return(data)
}
