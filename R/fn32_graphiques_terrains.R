#' Terrains : les graphiques
#'
#' @param data dataframe
#'
#' @return
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_size
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @export
fn32_graphiques_terrains <- function(data = doc_frcordep_filtre) {
  # G graphiques ----------
ls_cowplot <- list()
  # G1 terrains prix moyen au m2 ---------
  #

  max_y <- 10 * ceiling(data %>%
                          dplyr::filter(territoire %in% c("Corse", "France")) %>%
                          dplyr::pull(prix_m2) %>% max() /
                          10)

  data %>%
    dplyr::filter(territoire %in% c("Corse", "France"))  %>%
    ggplot2::ggplot(
      ggplot2::aes(
        annee,
        prix_m2,
        group = territoire,
        shape = territoire,
        color = territoire
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_size(guide = "none") +
    ggplot2::scale_y_continuous(
      name = "Prix moyen en euros par m\u00b2",
      limit = c(40, max_y),
      breaks = seq(40, max_y, 10)
    ) +
    ggplot2::scale_color_manual(values = c("darkorange3", "darkgoldenrod1")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.position = c(0.55, 0.75),
      legend.justification = c(1.2, 0)
    ) +
    ggplot2::labs(title = "Prix moyen des terrains",
                  caption = "Source :  SDES enqu\u00eete EPTB") -> plot_terr_prixm2

  plot_terr_prixm2

  ggplot2::ggsave(
    file.path(
      "4_resultats",
      ls_dates[["annee_etude"]],
      "Graphes",
      "corfrm_prixmoy_terrains_m2.png"
    ),
    type = "cairo",
    width = 22,
    height = 20,
    unit = "cm",
    dpi = 300
  )
  ls_cowplot$plot_terr_prixm2 <- plot_terr_prixm2

  # G2 terrains surface moyenne ---------
  #


  max_y <- 100 * ceiling(data %>%
                           dplyr::filter(territoire %in% c("Corse", "France")) %>%
                           dplyr::pull(surf_m2) %>% max() /
                           100)

  data %>%
    dplyr::filter(territoire %in% c("Corse", "France"))  %>%
    ggplot2::ggplot(
      ggplot2::aes(
        annee,
        surf_m2,
        group = territoire,
        shape = territoire,
        color = territoire
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_size(guide = "none") +
    ggplot2::scale_y_continuous(
      name = "Surface moyenne en m\u00b2",
      limit = c(800, max_y),
      breaks = seq(800, max_y, 200)
    ) +
    ggplot2::scale_color_manual(values = c("darkorange3", "darkgoldenrod1")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.position = c(0.95, 0.75),
      legend.justification = c(1.2, 0)
    ) +
    ggplot2::labs(title = "Surface moyenne des terrains achetés",
                  caption = "Source :  SDES enqu\u00eete EPTB") -> plot_terr_surf

  plot_terr_surf

  ggplot2::ggsave(
    file.path(
      "4_resultats",
      ls_dates[["annee_etude"]],
      "Graphes",
      "corfrm_surfmoy_terrains.png"
    ),
    type = "cairo",
    width = 22,
    height = 20,
    unit = "cm",
    dpi = 300
  )
  return(ls_cowplot)
}
