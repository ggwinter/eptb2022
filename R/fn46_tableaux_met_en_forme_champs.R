#' Scribus, tableaux, mise en forme de la première ligne du tableau
#'
#'
#' @param x liste
#'
#' @return rien
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom here here
#' @importFrom purrr flatten_chr
#' @importFrom purrr walk2
#' @importFrom xml2 write_xml
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_set_attr
#' @export
fn46_tableaux_met_en_forme_champs <-
  function(x = ls_tableaux0$t_tableaux_cell) {
    x %>%
      dplyr::filter(lgn == 0) %>%
      dplyr::distinct(num_objet, num_cell) -> t_tableaux_cell_prem_ligne

    fn_tableaux_mef_prem_ligne <-
      function(num_objet = 20 ,
               num_cell = 8) {
        xml2::xml_set_attr(fn_rc_po_no_td_numcell(num_objet, num_cell),
                           "TextDistLeft",
                           "1")

        xml2::xml_set_attr(fn_rc_po_no_td_numcell(num_objet, num_cell),
                           "TextDistTop",
                           "1")

        xml2::xml_set_attr(fn_rc_po_no_td_numcell(num_objet, num_cell),
                           "TextDistBottom",
                           "1")

        xml2::xml_set_attr(fn_rc_po_no_td_numcell(num_objet, num_cell),
                           "TextDistRight",
                           "1")

        xml2::xml_set_attr(fn_rc_po_no_td_numcell(num_objet, num_cell),
                           "TextVertAlign",
                           "1")



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

        xml2::xml_set_attr(fn_rc_po_no_td_allcell(num_objet),
                           "LeftPadding",
                           1)

        xml2::xml_set_attr(fn_rc_po_no_td_allcell(num_objet),
                           "RightPadding",
                           1)

        xml2::xml_set_attr(fn_rc_po_no_td_allcell(num_objet),
                           "TopPadding",
                           1)

        xml2::xml_set_attr(
          fn_rc_po_no_td_numcell(num_objet, num_cell) %>%
            xml2::xml_find_all("StoryText")  %>%
            xml2::xml_find_all("trail"),
          "PARENT",
          NULL
        )

      }

    purrr::walk2(
      t_tableaux_cell_prem_ligne %>% dplyr::pull(num_objet),
      t_tableaux_cell_prem_ligne %>% dplyr::pull(num_cell),

      .f = fn_tableaux_mef_prem_ligne
    )


  }
