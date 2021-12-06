#' Terrains calcule des 5 indicateurs de la plaquette
#'
#' @param data tableau
#'
#' @return tableau modifié
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export
fn91_terrains_calcul_5indics <-
  function(data) {
    data %>% dplyr::mutate(
      terr_surfmoy_m2  = round(.data$terr_surftot / .data$terr_nb , digits = 0),
      terr_prixmoy  = round(.data$terr_prixtot / .data$terr_nb , digits = 0),
      terr_prixmoy_m2  = round(.data$terr_prixmoy / .data$terr_surfmoy_m2 , digits = 0),
      part_prixterrain  = round(.data$terr_prixtot / .data$proj_mtt , digits = 3),
      proj_mttmoy  =  round(.data$proj_mtt / .data$terr_nb , digits = 0)
    ) -> data
    return(data)
  }
