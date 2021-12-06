#' Scribus interroge le fichier sla pour extraire tous les nodes d'une cellule
#' d'un tableau.
#'
#' @param num_objet id du tableau
#' @param num_cell id de la cellule
#' @importFrom xml2 xml_find_all
#' @return nodes de la cellule
fn_rc_po_no_td_numcell <- function(num_objet = 6,
                                   num_cell = 2) {
  ls_modele$pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
    .[[num_objet]] %>%
    xml2::xml_find_all("TableData") %>%
    xml2::xml_find_all("Cell") %>% .[[num_cell]] -> ph
  return(ph)
}
