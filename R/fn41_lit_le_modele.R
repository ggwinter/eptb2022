#' Scribus : texte, lit la plaquette modele, supprime les lignes et en cree
#' autant de vides que dans le nouveau texte
#'
#' @param x date annee etude
#'
#' @return
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr tibble
#' @importFrom purrr map_dfr
#' @importFrom purrr map2
#' @importFrom purrr set_names
#' @importFrom purrr walk
#' @importFrom stats na.omit
#' @importFrom stringr str_detect
#' @importFrom stringr str_which
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_child
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_remove
#' @export
#'
fn41_lit_le_modele <- function(x = ls_dates$annee_etude) {
  # Lecture du texte avant modification de la plaquette ---------

  # Lecture du texte avant modification de la plaquette ---------

  data("txt_plaquette", package = "eptb2022")
  xml2::read_xml(txt_plaquette, encoding = 'UTF-8') -> pg


  # liste des objets nommés
  purrr::map_dfr(eff, ~.x[c("ANNAME", "ItemID")], .id = "num_objet") %>%
    dplyr::rename(c("nom_objet" = "ANNAME", "id_objet"= "ItemID"))-> t_objets_numero

  # les paragraphes ----------

  t_objets_numero %>%
    dplyr::filter(stringr::str_detect(nom_objet, "_texte|_resume")) -> t_paragraphes

  # x <- t_paragraphes$nom_objet[1]

  t_paragraphes %>% dplyr::inner_join(ls_newtxt$eff, by =c("nom_objet" = "par"))-> t_paragraphes



  # Supprimer le contenu existant des paragraphes du modèle
  #
  # test num_node = 4
  fn_supprime_text0 <- function(x = "p1_bloc2_resume") {
    stringr::str_which(pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
                         xml2::xml_attr("ANNAME"),
                       x) -> num_node

    pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
      .[[num_node]]  %>%
      xml2::xml_child("StoryText")  %>%
      xml2::xml_children() %>% length() -> nb_lgn0


    xml2::xml_remove(
      pg %>%
        xml2::xml_find_all(".//PAGEOBJECT") %>%
        .[[num_node]] %>%
        xml2::xml_child("StoryText")  %>%
        xml2::xml_children() %>%
        .[2:nb_lgn0],
      free = TRUE
    )
  }


  t_paragraphes %>% dplyr::pull(nom_objet) %>%
    purrr::set_names() -> noms_paragraphes

  purrr::walk(noms_paragraphes, fn_supprime_text0)

  # ajouter le nombre de lignes vides correspondant au nombre de lignes du
  # nouveau texte

  fn_ajoute_2_nodes <- function(x = 4) {
    pg %>%
      xml2::xml_find_all(".//PAGEOBJECT") %>%
      .[[x]] %>%
      xml2::xml_child("StoryText") %>%
      xml2::xml_add_child(., xml2::read_xml('<ITEXT CH=""/>'))

    pg %>%
      xml2::xml_find_all(".//PAGEOBJECT") %>%
      .[[x]] %>%
      xml2::xml_child("StoryText") %>%
      xml2::xml_add_child(., xml2::read_xml('<para/>'))
  }

  # test
  # fn_ajoute_2_nodes()

  purrr::map2(t_paragraphes %>% dplyr::pull(num_objet),
              t_paragraphes %>% dplyr::pull(nblt_r), ~rep(.x, .y)) %>%
    unlist()-> iterations

  # Verification que tout a bien fonctionné
  # test
  # purrr::walk(c(4,4), fn_ajoute_2_nodes)

  # pg %>%
  #   xml2::xml_find_all(".//PAGEOBJECT") %>%
  #   .[[4]] %>%
  #   xml2::xml_child("StoryText")


  purrr::walk(iterations, fn_ajoute_2_nodes)


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

  # Verification que tout a bien fonctionné
  purrr::map(texte_plaquette, ~all(nchar(.x)==0))

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
              "paragraphes" = paragraphes,
              "t_objets_numero" = t_objets_numero,
              "pg" = pg))

}
