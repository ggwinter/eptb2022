#' Maisons : cree une liste de tableau par modalites csp moe chauffage etc
#'
#' @param fich_ssfiltre un vecteur de caractere avec toutes les modalites
#'
#' @return liste
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @export
fn15_extrait_tab_ssfiltre_modalites <-
  function(fich_ssfiltre = c(
    "ssfiltre_csp",
    "ssfiltre_rp",
    "ssfiltre_age",
    "ssfiltre_finition",
    "ssfiltre_moe",
    "ssfiltre_chauffage"
  )) {
    tli_reg_indics_ssfiltre <-
      purrr::map(fich_ssfiltre, fn14_lit_fich_ssfiltre_var) %>%
      purrr::set_names(fich_ssfiltre)

    return(tli_reg_indics_ssfiltre)
  }
