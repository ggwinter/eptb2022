#' Scibus : texte, remplace le texte du titre
#'
#' @param x la date de l'annee d'etude
#'
#' @return
#' @importFrom xml2 xml_set_attr
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_children
#' @importFrom stringr  str_detect
#' @importFrom dplyr pull
#' @importFrom utf8 as_utf8
#' @export
fn42b_remplace_txt_titre <- function(x = ls_dates$annee_etude) {
  ls_modele$t_objets_numero %>%
    dplyr::filter(stringr::str_detect(nom_objet, "_titre$|_Titre$")) %>%
    pull(num_objet) -> num_node_titre

  titre_new <- utf8::as_utf8(c("Prix des terrains à b\u00e2tir", paste0("en ", x)))#"Prix des terrains \u00e0 b\u00e2tir"


  xml_set_attr(
    ls_modele$pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
      .[[num_node_titre]]  %>%
      xml2::xml_children() %>%
      xml2::xml_find_all("ITEXT"),
    "CH",
    titre_new
  )

}
