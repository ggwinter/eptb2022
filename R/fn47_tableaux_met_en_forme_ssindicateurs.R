#' Scribus, tableaux, mise en forme des sous-indicateurs
#'
#' Enregistre aussi toutes les modifications en sortie
#'
#' @param x df
#'
#' @return rien
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom here here
#' @importFrom purrr flatten_chr
#' @importFrom purrr walk2
#' @importFrom utf8 as_utf8
#' @importFrom xml2 write_xml
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_set_attr
#' @export
fn47_tableaux_met_en_forme_ssindicateurs <-
  function(x = toto) {
    x %>% dplyr::filter(col == 0,
                           txt_new %in% utf8::as_utf8(
                             c(
                               "Age de l'acquéreur",
                               "Catégorie socio-proféssionnelle",
                               "Résidence principale",
                               "Viabilisation du terrain",
                               "Mode de chauffage",
                               "Finition",
                               "Ma\u00eetre d'oeuvre"
                             )
                           )) %>%
      dplyr::distinct(num_objet, num_cell) -> t_tableaux_cell_ssindicateurs

    fn_tableaux_mef_ssindicateurs <-
      function(num_objet = 20 ,
               num_cell = 8) {
        xml2::xml_set_attr(
          fn_rc_po_no_td_numcell(num_objet, num_cell) %>%
            xml2::xml_find_all("StoryText") %>%
            xml2::xml_find_all("DefaultStyle"),
          "PARENT",
          "sty_tab_centre"
        )

        xml2::xml_set_attr(
          fn_rc_po_no_td_numcell(num_objet, num_cell) %>%
            xml2::xml_find_all("StoryText") %>%
            xml2::xml_find_all("DefaultStyle"),
          "CPARENT",
          "tab_normal"
        )

        xml2::xml_set_attr(
          fn_rc_po_no_td_numcell(num_objet, num_cell) %>%
            xml2::xml_find_all("StoryText") %>%
            xml2::xml_find_all("ITEXT"),
          "CPARENT",
          NULL
        )

      }

    purrr::walk2(
      t_tableaux_cell_ssindicateurs %>% dplyr::pull(num_objet),
      t_tableaux_cell_ssindicateurs %>% dplyr::pull(num_cell),

      .f = fn_tableaux_mef_ssindicateurs
    )

    xml2::write_xml(
      ls_modele$pg,
      here::here("4_resultats", ls_dates$annee_etude, "p4p_eptb.sla"),
      overwrite = TRUE
    )
  }
