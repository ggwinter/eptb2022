#' Mons et terrains : calcule les evolutions sur un an et depuis 2010
#'
#' @param data une data frame
#'
#' @return
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr na_if
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom purrr transpose
#' @importFrom stringr str_detect
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @export
#'
fn18_calcul_evolution_maisons_terrains <-
  function(data = list("doc_frcordep_filtre" = doc_frcordep_filtre,
                       "doc_frcordep_ssfiltre" = doc_frcordep_ssfiltre)) {
    fn97_calcul_evolution <- function(data = doc_frcordep_filtre) {
      data %>%
        dplyr::select(-type) %>%
        dplyr::filter(annee %in% c(ls_dates[['annee_etude']], ls_dates[['annee_prec']])) %>%
        tidyr::gather(key = indic, value = value, -territoire, -annee) %>%
        tidyr::spread(key = annee, value = value) %>%
        dplyr::rename("value0" = ls_dates[['annee_prec']],
                      "value" = ls_dates[['annee_etude']]) %>%
        dplyr::mutate(diff = value - value0,
                      taux  = round(100 * (diff / value0), 1)) %>%
        dplyr::mutate(
          value = dplyr::case_when(
            stringr::str_detect(indic, "part") == TRUE ~ round(100 * value, 1),
            stringr::str_detect(indic, "part") == FALSE ~ value
          ),
          value0 = dplyr::case_when(
            stringr::str_detect(indic, "part") == TRUE ~ round(100 * value0, 1),
            stringr::str_detect(indic, "part") == FALSE ~ value0
          ),
          diff = dplyr::case_when(
            stringr::str_detect(indic, "part") == TRUE ~ round(100 * diff, 1),
            stringr::str_detect(indic, "part") == FALSE ~ diff
          ),
          taux = dplyr::if_else(stringr::str_detect(indic, "part"), 9999, taux) %>%
            dplyr::na_if(., 9999)
        ) -> Tab_evol_an


      Tab_evol_an$taux[str_detect(Tab_evol_an$indic, "part")] <- NA

      Tab_evol_an %>% dplyr::mutate(tri = dplyr::case_when(
        is.na(value) == TRUE & is.na(diff) == TRUE ~ 1,
        is.na(value) == FALSE |
          is.na(diff) == FALSE ~ 0
      )) %>%
        dplyr::filter(tri < 1) %>%
        dplyr::mutate(tri = NULL) %>%
        dplyr::arrange(indic, territoire) %>%
        dplyr::select(territoire, indic, value, value0, diff, taux) -> Tab_evol_an

      data %>%
        dplyr::select(-type) %>%
        tidyr::gather(key = indic, value = value, -territoire, -annee) -> Tab_evol_depuis2010

      Tab_evol_depuis2010 %>%
        dplyr::filter(annee %in% "2010") %>%
        dplyr::rename("value0" = "value") %>%
        dplyr::select(-annee) -> eff3

      Tab_evol_depuis2010 %>%
        dplyr::left_join(eff3, by = c("territoire", "indic")) %>%
        dplyr::mutate(
          nb_annee = as.integer(annee) - 2010,
          diff = value - value0,
          taux  = 100 * (exp(1 / nb_annee * log(value / value0)) - 1),
          taux = round(taux, 2)
        ) %>%
        dplyr::filter(annee != "2010", !(territoire %in% c("France", "Corse") &
                                           indic %in% "part")) %>%
        dplyr::mutate(
          value = dplyr::case_when(
            stringr::str_detect(indic, "part") == TRUE ~ round(100 * value, 1),
            stringr::str_detect(indic, "part") == FALSE ~ value
          ),
          value0 = dplyr::case_when(
            stringr::str_detect(indic, "part") == TRUE ~ round(100 * value0, 1),
            stringr::str_detect(indic, "part") == FALSE ~ value0
          ),
          diff = dplyr::case_when(
            stringr::str_detect(indic, "part") == TRUE ~ round(100 * diff, 1),
            stringr::str_detect(indic, "part") == FALSE ~ diff
          ),
          taux = dplyr::if_else(stringr::str_detect(indic, "part"), 9999, taux) %>%
            dplyr::na_if(., 9999)
        ) -> Tab_evol_depuis2010


      Tab_evol_depuis2010 %>% dplyr::select(-nb_annee) -> Tab_evol_depuis2010

      return(list("evol_an" = Tab_evol_an, "evol_depuis2010" = Tab_evol_depuis2010))
    }

    tli_frcordepcor_filtre_ssfiltre <-
      purrr::map(data, fn97_calcul_evolution) %>% purrr::transpose()
    return(tli_frcordepcor_filtre_ssfiltre)
  }
