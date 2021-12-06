#' Maisons : modifie les tableaux maisons pour la plaquette
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
fn13_cree_doc_frcordep_ssfiltre <- function(data = tli_regdep_ssfiltre) {
  c("territoire_lib", ls_import[["terrains_champs_1"]], "type") %>%
    stringr::str_subset(pattern = "^indic", negate = TRUE) -> eff


  doc_frregdep_ssfiltre <-
    purrr::map2(tli_regdep_ssfiltre,
                names(tli_regdep_ssfiltre),
                fn06_met_en_forme_liste) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::rename("territoire" = "territoire_lib") %>%
    dplyr::select(dplyr::one_of(
      c(
        "annee",
        "territoire",
        "part",
        "mai_prixmoy_m2",
        "mai_surfmoy_m2",
        "mai_prixmoy",
        "type"
      )
    ))  %>%
    dplyr::rename("prix_m2" = "mai_prixmoy_m2",
                  "surf_m2" = "mai_surfmoy_m2",
                  "prix" = "mai_prixmoy") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prix = arrond_100(prix),
                  prix_m2 = arrond_10(prix_m2)) %>%
    dplyr::filter(annee <= ls_dates[["annee_etude"]])


  doc_frcordep_ssfiltre <- doc_frregdep_ssfiltre %>%
    dplyr::filter(territoire %in% c("Corse-du-Sud",
                                    "Haute-Corse",
                                    "Corse",
                                    "France"))

  doc_frcordep_ssfiltre$part[doc_frcordep_ssfiltre$territoire %in% "Corse"] <-
    1
  doc_frcordep_ssfiltre$part[doc_frcordep_ssfiltre$territoire %in% "France"] <-
    NA
  return(doc_frcordep_ssfiltre)
}


