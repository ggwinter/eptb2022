#' Plaquette : enregistre les tableaux de calculs complémentaires
#'
#' @param x annee etude en caracteres
#'
#' @return
#' @importFrom rio export
#' @importFrom readr write_rds
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom purrr map_dfr
#' @importFromd plyr pull
#' @export
#'
fn21_enregistre_tableaux <- function(x = ls_dates$annee_etude) {
  # I Export tableau avec les valeurs comparatives ------------

  # tab_calculs_metadonnees <-
  #   readr::read_csv('E:/A_Bureau/R/EPTB/3_tables/tab_calculs_metadonnees.csv')
  #

  # tableau comparatif terrains dep cor vs corse et france
  #

  tli_frcordepcor_filtre_ssfiltre[["evol_an"]][["doc_frcordep_filtre"]] %>%
    dplyr::select(territoire, indic, value) %>%
    dplyr::mutate(type = dplyr::case_when(
      indic %in% c(ind_dec, "cout_projet", "surf_m2") ~ "pc",
      indic %in% c("part_projet") ~
        "diff"
    )) %>%
    dplyr::filter(complete.cases(type)) -> eff1

  c("pc", "diff") -> mes_param

  fn_diff_fr_cor <- function(x = "pc") {
    eff1 %>% dplyr::filter(type %in% x) %>%
      dplyr::filter(territoire %in% c("Corse-du-Sud", "Haute-Corse")) -> eff1_dep
    eff1 %>% dplyr::filter(type %in% x) %>%
      dplyr::filter(territoire %in% c("France")) %>%
      dplyr::select(-territoire) %>% dplyr::rename(c("fr" = "value")) -> eff1_fr
    eff1 %>% dplyr::filter(type %in% x) %>%
      dplyr::filter(territoire %in% c("Corse")) %>%
      dplyr::select(-territoire) %>% dplyr::rename(c("cor" = "value")) -> eff1_cor

    eff1_dep %>% dplyr::left_join(eff1_fr, by = c("indic", "type")) %>%
      dplyr::left_join(eff1_cor, by = c("indic", "type")) %>%
      dplyr::mutate(d_dep_cor = value - cor,
                    d_dep_fr = value - fr) -> eff1_dep
    if (unique(eff1_dep$type) == "pc")
      eff1_dep %>%
      dplyr::mutate(dt_dep_cor = d_dep_cor / cor,
                    dt_dep_fr = d_dep_fr / fr) -> eff1_dep
    return(eff1_dep)
  }

  purrr::map_dfr(mes_param %>% dplyr::pull(type), fn_diff_fr_cor) -> tab_terrains_compare_dep_fr
  rm(eff1_dep, eff1_fr, eff1_cor, eff1)

  # tableau comparatif maisons dep cor vs corse et france
  #

  tli_frcordepcor_filtre_ssfiltre[["evol_an"]][["doc_frcordep_ssfiltre"]] %>%
    dplyr::select(territoire, indic, value) %>%
    dplyr::mutate(type = dplyr::case_when(indic %in% c(ind_dec, "surf_m2") ~
                                            "pc",
                                          TRUE ~ NA_character_)) %>%
    dplyr::filter(complete.cases(type)) -> eff1

  x = "pc"
  eff1 %>% dplyr::filter(type %in% x) %>%
    dplyr::filter(territoire %in% c("Corse-du-Sud", "Haute-Corse")) -> eff1_dep
  eff1 %>% dplyr::filter(type %in% x) %>%
    dplyr::filter(territoire %in% c("France")) %>%
    dplyr::select(-territoire) %>% dplyr::rename(c("fr" = "value")) -> eff1_fr
  eff1 %>% dplyr::filter(type %in% x) %>%
    dplyr::filter(territoire %in% c("Corse")) %>%
    dplyr::select(-territoire) %>% dplyr::rename(c("cor" = "value")) -> eff1_cor

  eff1_dep %>% dplyr::left_join(eff1_fr, by = c("indic", "type")) %>%
    dplyr::left_join(eff1_cor, by = c("indic", "type")) %>%
    dplyr::mutate(d_dep_cor = value - cor,
                  d_dep_fr = value - fr) %>%
    dplyr::mutate(dt_dep_cor = d_dep_cor / cor,
                  dt_dep_fr = d_dep_fr / fr) -> tab_maisons_compare_dep_fr

  rm(eff1_dep, eff1_fr, eff1_cor, eff1)


  # tableau bilan
  tab_calculs <-
    list(
      "meta_donnees" = tab_calculs_metadonnees,
      "terrains_an" = tli_frcordepcor_filtre_ssfiltre[["evol_an"]][["doc_frcordep_filtre"]],
      "terrains_depuis2010" = tli_frcordepcor_filtre_ssfiltre[["evol_depuis2010"]][["doc_frcordep_filtre"]],
      "terrains_themes" = tab2_terrains,
      "terrains_autres_reg" = ls_beyond$terrains,
      "terrains_autres_reg_clt" = ls_beyond$terrains_clt,
      "terrains_depcor_compare_fr" = tab_terrains_compare_dep_fr,
      "maisons_an" = tli_frcordepcor_filtre_ssfiltre[["evol_an"]][["doc_frcordep_ssfiltre"]],
      "maisons_depuis2010" = tli_frcordepcor_filtre_ssfiltre[["evol_depuis2010"]][["doc_frcordep_ssfiltre"]],
      "maisons_themes" = tab4_maisons,
      "maisons_autres_reg" = ls_beyond$maisons,
      "maisons_autres_reg_clt" = ls_beyond$maisons_clt,
      "maisons_depcor_compare_fr" = tab_maisons_compare_dep_fr
    )

  rm(ls_beyond)

  if (exists("tab2_terrains_fr") == TRUE) {
    tab_calculs[["terrains_fr_indics_an"]] <- tab2_terrains_fr
  }
  if (exists("tab4_maisons_fr") == TRUE)
    tab_calculs[["maisons_fr_indics_an"]] <- tab4_maisons_fr

  rio::export(
    tab_calculs,
    file.path("4_resultats",
              ls_dates[["annee_etude"]],
              "Tableaux",
              "tab_calculs.xlsx"),
    overwrite = TRUE
  )

  readr::write_rds(tab_calculs,
                   file.path("4_resultats",
                             ls_dates[["annee_etude"]],
                             "Tableaux",
                             "tab_calculs.RDS"))
  return(tab_calculs)
}
