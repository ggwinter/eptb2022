#' Plaquette : enregistre les tableaux de calculs complémentaires
#'
#' @param x annee etude en caracteres
#'
#' @return
#' @importFrom rio export
#' @importFrom readr write_rds
#' @export
#'
fn21_enregistre_tableaux <- function(x = ls_dates$annee_etude) {
  # I Export tableau avec les valeurs comparatives ------------

  # tab_calculs_metadonnees <-
  #   readr::read_csv('E:/A_Bureau/R/EPTB/3_tables/tab_calculs_metadonnees.csv')

  # tableau bilan
  tab_calculs <-
    list(
      "meta_donnees" = tab_calculs_metadonnees,
      "terrains_an" = tli_frcordepcor_filtre_ssfiltre[["evol_an"]][["doc_frcordep_filtre"]],
      "terrains_depuis2010" = tli_frcordepcor_filtre_ssfiltre[["evol_depuis2010"]][["doc_frcordep_filtre"]],
      "terrains_themes" = tab2_terrains,
      "terrains_autres_reg" = ls_beyond$terrains,
      "terrains_autres_reg_clt" = ls_beyond$terrains_clt,
      "maisons_an" = tli_frcordepcor_filtre_ssfiltre[["evol_an"]][["doc_frcordep_ssfiltre"]],
      "maisons_depuis2010" = tli_frcordepcor_filtre_ssfiltre[["evol_depuis2010"]][["doc_frcordep_ssfiltre"]],
      "maisons_themes" = tab4_maisons,
      "maisons_autres_reg" = ls_beyond$maisons,
      "maisons_autres_reg_clt" = ls_beyond$maisons_clt
    )

  rm(ls_beyond)

  if (exists("tab2_terrains_fr") == TRUE) {
    tab_calculs[["terrains_fr_indics_an"]] <- tab2_terrains_fr
  }
  if (exists("tab4_maisons_fr") == TRUE)
    tab_calculs[["maisons_fr_indics_an"]] <- tab4_maisons_fr

  rio::export(tab_calculs,
              file.path("4_resultats",
                        ls_dates[["annee_etude"]],
                        "Tableaux",
                        "tab_calculs.xlsx"),
              overwrite = TRUE)

  readr::write_rds(tab_calculs,
                   file.path("4_resultats",
                             ls_dates[["annee_etude"]],
                             "Tableaux",
                             "tab_calculs.RDS"))
  return(tab_calculs)
}
