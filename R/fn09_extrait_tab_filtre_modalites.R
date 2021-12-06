#' Terrains : cree une liste de tableau par modalites csp etc
#'
#'
#' @param fich_filtre un vecteur de caractere avec toutes les modalites
#'
#' @return liste
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @export
#'
fn09_extrait_tab_filtre_modalites <-
  function(fich_filtre = c("filtre_csp",
                           "filtre_rp",
                           "filtre_age",
                           "filtre_viabilisation")) {
    tli_reg_indics_filtre <-
      purrr::map(fich_filtre, fn08_lit_fich_filtre_var) %>%
      purrr::set_names(fich_filtre)

    return(tli_reg_indics_filtre)
  }

