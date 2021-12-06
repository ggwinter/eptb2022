#' Maisons : applique le secret statistique et masque les donnees sensibles
#' @param data une liste de tables
#' @return
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr group_split
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom purrr map_df
#' @importFrom purrr map_dfr
#' @importFrom purrr map2
#' @export
fn17_tab_ssfiltre_applique_secstat <- function(data = tli_reg_indics_ssfiltre) {

  fn_efface_data <- function(z) {
    z -> toto
    toto[which(toto$secret_stat), 6:(ncol(toto) - 1)] <- NA
    toto %>% dplyr::arrange(indic_cat) -> toto
    return(toto)
  }
  # x <- 4

  met_en_forme_cor_indics_ssfiltre <- function(x = 1) {
    eff <- tli_reg_indics_ssfiltre %>%
      .[[x]] %>%
      dplyr::filter(annee <= ls_dates[['annee_etude']], reg_cd %in% "94")

    unique(eff$indic_cat) %>% length() -> nb_lignes

    eff %>% dplyr::group_split(annee) %>%
      purrr::map(., ~ .x %>% dplyr::arrange(nb_obsnp)) -> ls_test

    purrr::map(ls_test, ~ .x %>%
                 dplyr::filter(secret_stat == TRUE) %>% nrow) %>%
      purrr::map(., ~ dplyr::if_else((.x %% 2 == 1), .x + 1, as.numeric(.x))) -> toto

    purrr::map2(ls_test,
                toto,
                ~ .x %>% dplyr::mutate("secret_stat" = c(
                  rep(TRUE, time = .y), rep(FALSE, time = nb_lignes - .y)
                ))) -> ls_test


    purrr::map(ls_test,  fn_efface_data) %>%
      purrr::map_dfr(., dplyr::bind_rows) -> eff



    eff %>%
      dplyr::select(dplyr::one_of(ls_import[["maisons_champs_1"]])) %>%
      dplyr::rename("prix_m2" = "mai_prixmoy_m2",
                    "surf_m2" = "mai_surfmoy_m2",
                    "prix" = "mai_prixmoy") %>%
      dplyr::mutate(prix = arrond_100(prix),
                    prix_m2 = arrond_10(prix_m2)) -> eff

    return(eff)
  }
  tab4_maisons <-
    purrr::map_df(seq_along(data),
                  met_en_forme_cor_indics_ssfiltre) %>%
    dplyr::arrange(indic, annee, indic_cat)

  return(tab4_maisons)

}
