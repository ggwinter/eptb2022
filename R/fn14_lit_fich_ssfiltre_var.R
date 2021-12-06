#' Maisons : lit les tableaux geokit par modalite csp etc...
#'
#' @param x un caractere specifiant la modalite
#'
#' @return
#' @importFrom dplyr all_of
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr rename_at
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom dplyr vars
#' @importFrom readxl read_excel
#' @export
#'
fn14_lit_fich_ssfiltre_var <- function(x = "ssfiltre_csp") {
  tab <- readxl::read_excel(ls_import[["path_eptb"]], x)
  champs0 <- names(tab)
  champs <-
    ls_import[["T_onglets_champs"]]$champs[which(ls_import[["T_onglets_champs"]]$champs0 %in% champs0 &
                                                   ls_import[["T_onglets_champs"]]$onglet %in% x)]

  tab <- tab %>%
    dplyr::rename_at(dplyr::vars(champs0), function(x) champs) %>%
    fn93_maisons_calcul_3indics() %>%
    dplyr::mutate(
      secret_stat = dplyr::case_when(nb_obsnp < 11 ~ TRUE,
                                     nb_obsnp > 10 ~ FALSE),
      indic = ls_import[["T_onglets_usage"]]$champ_sup[which(ls_import[["T_onglets_usage"]]$onglet %in% x)]
    ) %>%
    dplyr::group_by(reg_cd, reg_lib, annee) %>%
    fn94_maisons_calcul_part() %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(
      c(
        "reg_cd",
        "reg_lib",
        "annee",
        "indic",
        "indic_cat",
        "part",
        "nb_obsnp",
        "mai_nb",
        "mai_surftot",
        "mai_surfmoy_m2",
        "mai_prixtot",
        "mai_prixmoy",
        "mai_prixmoy_m2",
        "secret_stat"
      )
    ))
  return(tab)
}
