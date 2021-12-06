#' Terrains et maisons : ajoute la comparaison avec France entière
#'
#'
#' @param data une dataframe
#'
#' @return
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom stringr str_detect
#' @export
#'
fn19_calcul_evolution_maisons_terrains_mod <-
  function(data = tli_frcordepcor_filtre_ssfiltre$evol_an) {
    fn19_tab1_3_compare_fr_cor <- function(data) {
      data %>%
        dplyr::select(.data$territoire:.data$value) %>%
        dplyr::filter(!stringr::str_detect(.data$indic, "part")) -> eff

      eff %>% dplyr::filter(territoire %in% "France") %>%
        dplyr::select(-territoire) %>%
        dplyr::rename(c("france" = "value")) -> eff_F

      eff %>% dplyr::filter(territoire %in% "Corse") %>%
        dplyr::select(-territoire) %>%
        dplyr::rename(c("corse" = "value")) -> eff_C

      eff %>% dplyr::filter(!territoire %in% "France") %>%
        dplyr::left_join(eff_F, by = "indic") %>%
        dplyr::left_join(eff_C, by = "indic") %>%
        dplyr::mutate(
          tx_comp_fr = 100 * round((value - france) / france, 4),
          tx_comp_cor = 100 * round((value - corse) / corse, 4)
        ) %>%
        dplyr::select(-france,  -corse) -> eff

      data %>%
        dplyr::left_join(eff, by = c("territoire", "indic", "value")) -> data
      return(data)
    }
    purrr::map(data, fn19_tab1_3_compare_fr_cor) -> data
    return(data)

  }
