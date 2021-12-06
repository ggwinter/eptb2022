#' Maisons : autres graphiques non utilises dans la plaquette
#'
#' @param data dataframe
#'
#' @return
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_minimal
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom purrr imap
#' @importFrom purrr set_names
#' @importFrom purrr walk2
#' @importFrom stringr str_c
#' @importFrom tidyr pivot_longer
#' @export
#'
fn35_autres_graphiques_maisons <-
  function(data = tab4_maisons) {
    # G5 Autres graphiques ---------
    #
    data %>% dplyr::pull(indic) %>% unique() %>%
      purrr::set_names() -> v_mai_indics

    if (as.integer(ls_dates[["annee_etude"]]) %% 2 > 0) {
      v_an <- as.integer(ls_dates[["annee_etude"]]) + 1
    } else{
      v_an <- as.integer(ls_dates[["annee_etude"]])
    }

    fn_plot_maisons <- function(x = "age", y = "age") {
      tab4_maisons %>% dplyr::filter(indic %in% x) %>%
        dplyr::mutate(part = 100 * part) %>%
        tidyr::pivot_longer(cols = part:prix,
                            names_to = "indic2",
                            values_to = "valeur") %>%
        dplyr::filter(valeur > 0) %>%
        ggplot2::ggplot(ggplot2::aes(
          as.integer(annee),
          valeur,
          group = indic_cat,
          color = indic_cat
        )) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~ indic2, scales = "free_y") +
        ggplot2::theme_minimal() +
        ggplot2::scale_x_continuous(breaks = seq(2010, v_an, 2),
                                    limits = c(2010, 2020)) +
        ggplot2::labs(title = stringr::str_c("Les maisons : ", y), x = "") -> p
      return(p)
    }

    purrr::imap(v_mai_indics, fn_plot_maisons) -> lsg_maisons

    file.path(
      "4_resultats",
      ls_dates[["annee_etude"]],
      "Graphes",
      stringr::str_c("serie_longue_maisons_", names(v_mai_indics), ".png")
    ) -> v_noms

    purrr::walk2(
      lsg_maisons,
      v_noms,
      ~ ggplot2::ggsave(
        plot = .x,
        filename = .y,
        type = "cairo",
        width = 26,
        height = 14,
        unit = "cm",
        dpi = 300
      )
    )
  }
