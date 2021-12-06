#' Terrains : Modifie les tableau pour la plaquette
#'
#' @param data liste de tableaux
#'
#' @return
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom purrr imap
#' @importFrom purrr reduce
#' @importFrom stringr str_subset
#' @export
fn07_cree_doc_frcordep_filtre <- function(data = tli_regdep_filtre) {
  c("territoire_lib", ls_import[["terrains_champs_1"]], "type") %>%
    stringr::str_subset(pattern = "^indic", negate = TRUE) -> eff


  doc_frregdep_filtre <-
    purrr::imap(data,
                fn06_met_en_forme_liste) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::select(dplyr::one_of(eff)) %>%
    dplyr::rename(
      "territoire" = "territoire_lib",
      "prix_m2" = "terr_prixmoy_m2",
      "surf_m2" = "terr_surfmoy_m2",
      "prix" = "terr_prixmoy",
      "part_projet" = "part_prixterrain",
      "cout_projet" = "proj_mttmoy"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prix = arrond_100(prix),
                  cout_projet = arrond_100(cout_projet)) %>%
    dplyr::select(annee, territoire, dplyr::everything()) %>%
    dplyr::filter(annee <= ls_dates[['annee_etude']])# modif a tester

  doc_frcordep_filtre <- doc_frregdep_filtre %>%
    dplyr::filter(territoire %in% c("Corse-du-Sud",
                                    "Haute-Corse",
                                    "Corse",
                                    "France"))

  doc_frcordep_filtre$part[doc_frcordep_filtre$territoire %in% "Corse"] <-
    1
  doc_frcordep_filtre$part[doc_frcordep_filtre$territoire %in% "France"] <-
    NA
  return(doc_frcordep_filtre)
}


