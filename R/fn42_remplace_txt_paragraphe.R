#' Scibus : texte, remplace le texte du paragraphe
#'
#' @param data une tibble avec les colonnes num_objet, par, lgn
#'
#' @return
#' @importFrom xml2 xml_set_attr
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_child
#' @importFrom purrr pwalk
#' @importFrom dplyr pull
#' @export
#'
fn42_remplace_txt_paragraphe <- function(data = t_parametres) {
  fn_remplace_paragraphe_nb <-
    function(num_objet = 19,
             par = "p1_bloc6_texte",
             lgn = 2) {
      xml2::xml_set_attr(
        ls_modele$pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
          .[[num_objet]] %>%
          xml2::xml_child("StoryText") %>%
          xml2::xml_find_all("ITEXT") %>% .[[lgn]] ,
        "CH",
        ls_newtxt$mon_texte[[par]][lgn],
        ns = character()
      )

    }

  # fn_remplace_paragraphe_nb()

  purrr::pwalk(
    list(
      "num_objet" = data %>% dplyr::pull(num_objet),
      "par" = data %>% dplyr::pull(par),
      "lgn" = data %>% dplyr::pull(lgn)
    ),
    fn_remplace_paragraphe_nb
  )
  xml2::write_xml(ls_modele$pg,
                  here::here("4_resultats", ls_dates$annee_etude, "p4p_eptb.sla"),
                  overwrite = TRUE)
}
