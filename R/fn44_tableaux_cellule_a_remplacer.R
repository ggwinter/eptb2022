#' Scribus : tableaux, tableau des cellules a remplacer
#'
#' @param x liste
#'
#' @return df
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr tibble
#' @importFrom purrr flatten_chr
#' @importFrom purrr imap_dfr
#' @importFrom purrr map
#' @importFrom purrr map_dfr
#' @importFrom purrr map2_dfr
#' @importFrom stringr str_squish
#' @importFrom tidyr expand_grid
#' @importFrom tidyr fill
#' @importFrom utf8 as_utf8
#' @importFrom utf8 utf8_valid
#' @export
fn44_tableaux_cellule_a_remplacer <- function(x = resultats[3:6]){
  # Modification des tableaux --------
  #

 # valeurs de remplacement ------
  #

  ls_tab <- x

  fn_tableaux_cree_prem_ligne <- function(df, x) {
    df %>% dplyr::filter(type %in% x) %>% dplyr::select(-type) -> eff0
    eff0 %>% dplyr::slice(0) -> eff1
    eff1[1, 1] <- x
    rbind(eff1, eff0) -> df
    return(df)
  }


  fn_tableaux_compte_type <- function(df) {
    df %>% dplyr::pull(type) %>% unique() -> v_types
    return(v_types)
  }



  ls_tab[["tab2"]] %>%
    dplyr::mutate(
      'type' = c(
        "Age de l'acquéreur",
        rep(NA_character_, 4),
        "Catégorie socio-proféssionnelle",
        rep(NA_character_, 7),
        "Résidence principale",
        rep(NA_character_, 1),
        "Viabilisation du terrain",
        rep(NA_character_, 1)
      )
    ) %>% dplyr::select(type, dplyr::everything()) %>%
    tidyr::fill(type) -> ls_tab[["tab2"]]

  fn_tableaux_compte_type(ls_tab[["tab2"]]) -> param

  purrr::map_dfr(df = ls_tab[["tab2"]],
                 .x = param,
                 .f = fn_tableaux_cree_prem_ligne) -> ls_tab[["tab2"]]


  ls_tab[["tab4"]] %>%
    dplyr::mutate('type' = c(
      "Mode de chauffage",
      rep(NA_character_, 5),
      "Finition",
      rep(NA_character_, 2),
      "Ma\u00eetre d'oeuvre",
      rep(NA_character_, 4)
    )) %>% dplyr::select(type, dplyr::everything()) %>%
    tidyr::fill(type) -> ls_tab[["tab4"]]

  fn_tableaux_compte_type(ls_tab[["tab4"]]) -> param

  purrr::map_dfr(df = ls_tab[["tab4"]],
                 .x = param,
                 .f = fn_tableaux_cree_prem_ligne) -> ls_tab[["tab4"]]

  purrr::map(ls_tab,
             ~ .x %>% dplyr::mutate_if(.predicate = is.factor,  .funs = as.character)) -> ls_tab

  fn_tableau_transforme_tabw_to_tabl <- function(x = ls_tab[[4]]) {
    x -> df0
    nrow(df0) -> nblgn
    ncol(df0) -> nbcol
    tidyr::expand_grid("lgn" = 1:nblgn,
                       "col" = 1:nbcol - 1) %>%
      dplyr::mutate(
        "txt_new" = purrr::map(1:nblgn, ~ df0 %>% dplyr::slice(.x) %>%
                                 unlist() %>% unname()) %>%
          purrr::flatten_chr() %>% stringr::str_squish()
      ) -> df1
    return(df1)
  }
  # fn_tableau_transforme_tabw_to_tabl()
  purrr::map(.x = ls_tab, .f = fn_tableau_transforme_tabw_to_tabl) -> ls_tab_contenu
  # ls_tab_contenu[["tab4"]] -> toto




  purrr::imap_dfr(ls_tab_contenu,
                  ~ dplyr::tibble(noms_objets2 = .y, nbr = nrow(.x))) -> eff

  ls_tableaux0$t_tableaux_nb_col_lgn %>%
    dplyr::left_join(eff, by = "noms_objets2") -> t_tableaux_compare_valeurs

  # purrr::map(ls_tab_contenu, ~ utf8::utf8_valid(.x$txt_new))

  purrr::map(ls_tab_contenu, ~ .x %>%
               dplyr::mutate(txt_new = utf8::as_utf8(txt_new))) -> ls_tab_contenu

  purrr::map2_dfr(ls_tab_contenu,
                  c(6, 20, 25, 28),
                  ~ .x %>%
                    dplyr::mutate(num_objet = .y),
                  ncol = as.integer(ncol)) -> df_tab_contenu


  ls_tableaux0$t_tableaux_cell %>%
    dplyr::left_join(df_tab_contenu, by = c("num_objet", "lgn", "col")) -> ls_tableaux0$t_tableaux_cell

  ls_tableaux0$t_tableaux_cell %>%
    dplyr::filter(stats::complete.cases(txt_new)) -> toto

  return(toto)


}
