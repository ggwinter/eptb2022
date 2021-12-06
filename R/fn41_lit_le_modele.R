#' Scribus : texte, , lit la plaquette modele
#'
#' @param x date annee etude
#'
#' @return
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_find_all
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr tibble
#' @importFrom here here
#' @importFrom purrr map
#' @importFrom purrr map_dfr
#' @importFrom purrr set_names
#' @importFrom stats na.omit
#' @importFrom stringr str_detect
#' @importFrom stringr str_which
#' @export
#'
fn41_lit_le_modele <- function(x = ls_dates$annee_etude) {
  # Lecture du texte avant modification de la plaquette ---------

  data("txt_plaquette", package = "eptb2022")
  xml2::read_xml(txt_plaquette, encoding = 'UTF-8') -> pg


  # liste des objets nommés
  pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
    xml2::xml_attr("ANNAME") -> noms_objets


  pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
    xml2::xml_attr("ItemID") -> id_objets

  id_objets[-c(which(is.na(noms_objets)))] -> id_objets

  stats::na.omit(noms_objets) -> noms_objets

  purrr::map(
    noms_objets,
    ~ stringr::str_which(
      pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
        xml2::xml_attr("ANNAME"),
      .x
    )
  ) %>% unlist() -> nums_objets



  dplyr::tibble(nom_objet = noms_objets,
                id_objet = id_objets,
                num_objet = nums_objets) %>%
    dplyr::arrange(nom_objet) -> t_objets_numero







  # les paragraphes ----------

  t_objets_numero %>%
    dplyr::filter(stringr::str_detect(nom_objet, "_texte|_resume")) -> t_paragraphes

  # x <- t_paragraphes$nom_objet[1]


  fn_lit_paragraphe <- function(x) {
    stringr::str_which(pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
                         xml2::xml_attr("ANNAME"),
                       x) -> num_node

    pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
      .[[num_node]]  %>%
      xml2::xml_children() %>%
      xml2::xml_find_all("ITEXT") %>%
      xml2::xml_attr("CH")  -> txt_non_modifie
    return(txt_non_modifie)
  }

  t_paragraphes %>% dplyr::pull(nom_objet) %>%
    purrr::set_names() -> paragraphes



  purrr::map(paragraphes, fn_lit_paragraphe) -> texte_plaquette

  purrr::map_dfr(texte_plaquette,
                 ~ dplyr::tibble("nblt_p" = length(.x)), .id = "par") %>%
    dplyr::inner_join(ls_newtxt$eff, by = "par") %>%
    dplyr::mutate(diff = nblt_p - nblt_r) %>%
    dplyr::inner_join(t_paragraphes %>% dplyr::select(nom_objet, num_objet),
                      by = c("par" = "nom_objet")) -> t_compare_texte


  fn_cree_table_pour_map <- function(parametre = parametre[1]) {
    t_compare_texte %>% dplyr::filter(num_objet %in% parametre) -> eff2
    dplyr::tibble(
      "num_objet" = rep(parametre, times = eff2 %>% dplyr::pull(nblt_r)),
      "par" = rep(eff2 %>% dplyr::pull(par), times = eff2 %>% dplyr::pull(nblt_r)),
      "lgn" = 1:(eff2 %>% dplyr::pull(nblt_r))
    ) -> toto
    return(toto)
  }

  t_paragraphes %>% dplyr::pull(num_objet) %>%
    purrr::set_names(t_paragraphes %>% dplyr::pull(nom_objet)) -> parametre

  purrr::map_dfr(parametre, fn_cree_table_pour_map) -> t_parametres

  return(list("t_parametres" = t_parametres,
       "t_compare_texte" = t_compare_texte,
       "texte_plaquette" = texte_plaquette,
       "paragraphes" = paragraphes,
       "t_objets_numero" = t_objets_numero,
       "pg" = pg))

}
