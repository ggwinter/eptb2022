#' Maisons : verifie si le secret statistique s'applique
#' @param data une liste de tables
#' @return
#' @importFrom dplyr across
#' @importFrom dplyr count
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom purrr map
#' @importFrom tidyr replace_na
#' @export
fn16_tab_ssfiltre_verifie_secstat <- function(data = tli_reg_indics_ssfiltre) {
  which(purrr::map(
    data,
    ~ (
      .x %>% dplyr::count(indic, indic_cat, name = "nb") %>%
        dplyr::pull(nb) %>% sum()
    ) %% 10
  ) != 0) -> onglet_pb

  x <- 1
  fn_traite_onglets_pb_ssfiltre <- function(x) {
    tli_reg_indics_ssfiltre[[x]] -> eff
    eff %>% dplyr::distinct(indic, indic_cat) -> gfr
    eff %>% dplyr::pull(annee) %>% unique() -> plage_ans
    purrr::map_dfr(plage_ans, ~ gfr %>% dplyr::mutate(annee = .x)) -> gfr
    gfr %>% dplyr::left_join(eff, by = c("annee", "indic", "indic_cat")) %>%
      dplyr::mutate(dplyr::across(c(part, mai_nb:mai_prixmoy_m2),
                                  ~ tidyr::replace_na(.x, 0))) %>%
      dplyr::mutate(
        secret_stat = tidyr::replace_na(secret_stat, FALSE),
        reg_cd = "94",
        reg_lib = "Corse"
      ) -> eff
    return(eff)

  }

  purrr::map(onglet_pb, fn_traite_onglets_pb_ssfiltre) -> ls_modif
  c(tli_reg_indics_ssfiltre[-c(onglet_pb)], ls_modif) -> tli_reg_indics_ssfiltre
  return(tli_reg_indics_ssfiltre)
}





