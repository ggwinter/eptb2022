#' Terrains : lit les tableaux geokit par modalite csp etc...
#'
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
fn08_lit_fich_filtre_var <- function(x = "filtre_csp") {
  tab <- readxl::read_excel(path_eptb, x)
  champs0 <- names(tab)
  champs <-
    ls_import[["T_onglets_champs"]]$champs[which(ls_import[["T_onglets_champs"]]$champs0 %in% champs0 &
                                                   ls_import[["T_onglets_champs"]]$onglet %in% x)]


  tab <- tab %>%
    dplyr::rename_at(dplyr::vars(dplyr::all_of(champs0)), function(x)
      champs) %>%
    fn91_terrains_calcul_5indics %>%
    dplyr::mutate(
      secret_stat = dplyr::case_when(nb_obsnp < 11 ~ TRUE,
                                     nb_obsnp > 10 ~ FALSE),
      indic = ls_import[["T_onglets_usage"]]$champ_sup[which(ls_import[["T_onglets_usage"]]$onglet %in% x)]
    ) %>%
    dplyr::group_by(reg_cd, reg_lib, annee) %>%
    fn92_terrains_calcul_part() %>%
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
        "terr_nb",
        "terr_surftot",
        "terr_surfmoy_m2",
        "terr_prixtot",
        "terr_prixmoy",
        "terr_prixmoy_m2",
        "part_prixterrain",
        "proj_mtt",
        "proj_mttmoy",
        "secret_stat"
      )
    ))
  return(tab)
}
