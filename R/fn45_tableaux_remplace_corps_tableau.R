#' scribus, Tableaux, ajoute les valeurs issues des calculs dans les tableaux
#'
#' @param x liste
#'
#' @return rien
#' @importFrom dplyr pull
#' @importFrom purrr pwalk
#' @importFrom purrr walk
#' @importFrom xml2 read_xml
#' @importFrom xml2 write_xml
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_set_attr
#' @export
fn45_tableaux_remplace_corps_tableau <-
  function(x = resultats[3:6]) {

    # read_xml(chem_fich, encoding = 'UTF-8') -> pg

    fn_complete_tableau <-
      function(num_objet = 6 ,
               num_cell = 8,
               txt_new = "Corse-du-Sud") {
        purrr::walk(
          c(
            "LeftPadding",
            "RightPadding",
            "TopPadding",
            "BottomPadding"
          ),
          ~ xml2::xml_set_attr(fn_rc_po_no_td_allcell(num_objet),
                               .x,
                               1)
        )


        if (fn_rc_po_no_td_numcell(num_objet, num_cell) %>%
            xml2::xml_find_all("StoryText")  %>%
            xml2::xml_find_all("trail") %>%
            xml2::xml_attr(., "PARENT") %>% is.na() == FALSE) {
          xml2::xml_set_attr(
            fn_rc_po_no_td_numcell(num_objet, num_cell) %>%
              xml2::xml_find_all("StoryText")  %>%
              xml2::xml_find_all("trail"),
            "PARENT",
            NULL
          )
        }

        purrr::walk(
          c(
            "TextDistLeft",
            "TextDistTop",
            "TextDistBottom",
            "TextDistRight",
            "TextVertAlign"
          ),
          ~ xml2::xml_set_attr(fn_rc_po_no_td_numcell(num_objet, num_cell),
                               .x,
                               "1")
        )


        # if (ls_modele$pg %>% xml_find_all(".//PAGEOBJECT") %>% .[[num_objet]] %>%
        #     xml_find_all("TableData") %>%
        #     xml_find_all("Cell") %>% .[[num_cell]] %>%
        #     xml_find_all("StoryText") %>%
        #     xml_find_all("DefaultStyle") %>%
        #     xml_attr("PARENT") %>% na.omit() %>% length() == 0) {
        xml2::xml_set_attr(
          fn_rc_po_no_td_numcell(num_objet, num_cell) %>%
            xml2::xml_find_all("StoryText") %>%
            xml2::xml_find_all("DefaultStyle"),
          "PARENT",
          "sty_tab_droite"
        )

        xml2::xml_set_attr(
          fn_rc_po_no_td_numcell(num_objet, num_cell) %>%
            xml2::xml_find_all("StoryText") %>%
            xml2::xml_find_all("DefaultStyle"),
          "CPARENT",
          "tab_normal"
        )
        # }

        if (fn_rc_po_no_td_allcell(num_objet) %>%
            .[[num_cell]] %>%
            xml2::xml_find_all("StoryText") %>%
            xml2::xml_find_all("ITEXT") %>% length() == 0) {
          xml2::xml_add_child(
            fn_rc_po_no_td_numcell(num_objet, num_cell) %>%
              xml2::xml_find_all("StoryText"),
            xml2::read_xml("<ITEXT CH=''/>")# complete les cellules sans itext
          )


        }
        xml2::xml_set_attr(
          fn_rc_po_no_td_numcell(num_objet, num_cell) %>%
            xml2::xml_find_all("StoryText") %>%
            xml2::xml_find_all("ITEXT"),
          "CH",
          txt_new
        )
      }

    purrr::pwalk(
      list(
        "num_objet" = toto %>% dplyr::pull(num_objet),
        "num_cell" = toto %>% dplyr::pull(num_cell),
        "txt_new" = toto %>% dplyr::pull(txt_new)
      ),
      .f = fn_complete_tableau
    )

  }

