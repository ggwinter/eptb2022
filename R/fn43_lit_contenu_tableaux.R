#' Scribus : tableaux, lit le contenu des tableaux du modele
#'
#' @param x vecteur
#' @return liste
#' @importFrom dplyr arrange
#' @importFrom dplyr count
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr tibble
#' @importFrom purrr map_chr
#' @importFrom purrr map_dfr
#' @importFrom purrr map2_dfr
#' @importFrom purrr walk2
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom xml2 read_xml
#' @importFrom xml2 write_xml
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_find_all
#' @export
fn43_lit_contenu_tableaux <- function(x = 2020) {
  # tableaux existants ------
  #
  ls_modele$t_objets_numero %>%
    dplyr::filter(stringr::str_detect(nom_objet, "tab")) %>%
    dplyr::arrange(num_objet) %>%
    dplyr::mutate(num_objet = as.integer(num_objet))-> t_tableaux

  t_tableaux  %>%
    dplyr::mutate(tab = stringr::str_extract(nom_objet, "(?<=p[:digit:][:lower:]_).*$")) %>%
    dplyr::arrange(nom_objet)-> t_tableaux


  # tableau existant

  # ajout des infos manquantes à la table t_tableaux

  fn_tableaux_infos <- function(x = 6) {
    dplyr::tibble(
      num_objet = x,
      nb_cell = ls_modele$pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>% .[[x]] %>%
        xml2::xml_find_all("TableData") %>%
        xml2::xml_find_all("Cell") %>% length(),
      nb_lgn = ls_modele$pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>% .[[x]] %>%
        xml2::xml_attr("Rows") %>% as.integer(),
      nb_col = ls_modele$pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>% .[[x]] %>%
        xml2::xml_attr("Columns") %>% as.integer()
    ) -> t_tableaux_comp
    return(t_tableaux_comp)
  }
  fn_tableaux_infos()

  purrr::map_dfr(t_tableaux %>% dplyr::pull(num_objet), fn_tableaux_infos) -> eff
  t_tableaux %>% dplyr::inner_join(eff, by = "num_objet") -> t_tableaux
  rm(eff)

  purrr::map2_dfr(
    t_tableaux %>% dplyr::pull(num_objet),
    t_tableaux %>% dplyr::pull(nb_cell),
    ~ dplyr::tibble("num_objet" = .x,
                    "num_cell" = 1:.y)
  ) -> t_tableaux_cell



  fn_tableaux_lit_cell <- function(x = 20, y = 2) {
    dplyr::tibble(
      "num_objet" = x,
      "num_cell" = y,

      "lgn" = ls_modele$pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
        .[[x]] %>%
        xml2::xml_find_all("TableData") %>%
        xml2::xml_find_all("Cell") %>% .[[y]] %>%
        xml2::xml_attr("Row") %>% as.integer(),
      "col" = ls_modele$pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
        .[[x]] %>%
        xml2::xml_find_all("TableData") %>%
        xml2::xml_find_all("Cell") %>% .[[y]] %>%
        xml2::xml_attr("Column") %>% as.integer()
    ) -> df
    return(df)
  }

  purrr::map2_dfr(
    t_tableaux_cell %>% dplyr::pull(num_objet),
    t_tableaux_cell %>% dplyr::pull(num_cell),
    fn_tableaux_lit_cell
  ) -> t_tableaux_cell


  fn_tableaux_trouve_cell_vides <- function(x = 20) {
    dplyr::tibble(num_objet = x,
                  cell_vides = which(
                    purrr::map_chr(
                      .x = x,
                      .f = ~ ls_modele$pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>% .[[x]] %>%
                        xml2::xml_find_all("TableData") %>%
                        xml2::xml_find_all("Cell") %>%
                        xml2::xml_find_all("StoryText") %>%
                        xml2::xml_find_all("ITEXT") %>% length()# on verifie pour chaque objet qu'il y a bien un Itext
                    ) %in% "0"
                  )) -> df
    return(df)
  }
  fn_tableaux_trouve_cell_vides()

  purrr::map_dfr(t_tableaux_cell %>% dplyr::pull(num_objet),
                 fn_tableaux_trouve_cell_vides) -> t_tableaux_cell_vides


  x <- 20
  y <- 1

  fn_remplie_tableau_node_itext <- function(x, y) {
    xml2::xml_add_child(
      ls_modele$pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>% .[[x]] %>%
        xml2::xml_find_all("TableData") %>%
        xml2::xml_find_all("Cell") %>% .[[y]] %>%
        xml2::xml_find_all("StoryText"),

      xml2::read_xml("<ITEXT CH=''/>")# complete les cellules sans itext
    )
  }
  if (nrow(t_tableaux_cell_vides) > 0) {
    purrr::walk2(
      t_tableaux_cell_vides %>% dplyr::pull(num_objet),
      t_tableaux_cell_vides %>% dplyr::pull(cell_vides),
      fn_remplie_tableau_node_itext
    )
  }




  fn_tableaux_lit_cell_txt <- function(x = 20, y = 2) {
    dplyr::tibble(
      "num_objet" = x,
      "num_cell" = y,

      "txt" = ls_modele$pg %>% xml2::xml_find_all(".//PAGEOBJECT") %>%
        .[[x]] %>%
        xml2::xml_find_all("TableData") %>%
        xml2::xml_find_all("Cell") %>% .[[y]] %>%
        xml2::xml_find_all("StoryText") %>%
        xml2::xml_find_all("ITEXT") %>%
        xml2::xml_attr("CH"),

    ) -> df
    return(df)
  }

  purrr::map2_dfr(
    t_tableaux_cell %>% dplyr::pull(num_objet),
    t_tableaux_cell %>% dplyr::pull(num_cell),
    fn_tableaux_lit_cell_txt
  ) -> t_tableaux_cell_txt

  t_tableaux_cell %>%
    dplyr::left_join(t_tableaux_cell_txt, by = c("num_objet", "num_cell")) -> t_tableaux_cell




  t_tableaux_cell %>% dplyr::filter(!lgn %in% 0) %>%
    dplyr::count(num_objet, name = "nbp") %>%
    dplyr::mutate(noms_objets2 = c("tab1", "tab2", "tab3", "tab4")) -> t_tableaux_nb_col_lgn

  ls_tableaux0 <- list(
    "t_tableaux" = t_tableaux,
    "t_tableaux_cell" = t_tableaux_cell,
    "t_tableaux_nb_col_lgn" = t_tableaux_nb_col_lgn
  )

  return(ls_tableaux0)

}
