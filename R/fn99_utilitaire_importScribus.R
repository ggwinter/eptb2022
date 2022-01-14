#' utilitaire_importScribus
#'
#' enregistre le modele scribus dans data
#'
#' @param x le chemin d'acces au modele de plaquette
#'
#' @return nothing
#' @importFrom devtools use_data
fn99_utilitaire_importScribus <- function(x) {

  readLines(file.path(x))-> txt_plaquette
  usethis::use_data(txt_plaquette, overwrite = TRUE)

}
