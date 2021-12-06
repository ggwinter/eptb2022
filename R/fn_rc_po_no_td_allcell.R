#' Scribus interroge le fichier sla pour extraire tous les nodes des cellules
#' d'un tableau.
#'
#' @param num_objet numéro du tableau
#' @importFrom xml2 xml_find_all
#' @return nodes avec uniquement les cellules du tableau
fn_rc_po_no_td_allcell <- function(num_objet = 6) {
  ls_modele$pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
    .[[num_objet]] %>%
    xml2::xml_find_all("TableData") %>%
    xml2::xml_find_all("Cell") -> ph
  return(ph)
}
