#' utilitaire_importScribus
#'
#' enregistre le modele scribus dans data
#'
#' @param x le chemin d'acces au modele de plaquette
#'
#' @return nothing
#' @importFrom usethis use_data
fn99_utilitaire_importScribus <- function(x) {
  # "E:/A_Bureau/R/EPTB/3_tables/modeles_ne_pas_modifier/p4p_eptb_V.sla"
  readLines(file.path(x))-> txt_plaquette
  paste(txt_plaquette, collapse = " ")-> txt_plaquette
  usethis::use_data(txt_plaquette, overwrite = TRUE)

}
