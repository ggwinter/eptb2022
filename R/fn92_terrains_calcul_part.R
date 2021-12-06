#' Terrains calcul de la part de chaque territoire
#' (en nombre de terrains etudies)
#'
#' @param data tableau
#'
#' @return tableau modifié
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export
fn92_terrains_calcul_part <- function(data) {
  data %>% dplyr::mutate(terr_nbt = sum(.data$terr_nb),
                         part = round(.data$terr_nb / .data$terr_nbt, digits = 3)) -> data
  return(data)
}
