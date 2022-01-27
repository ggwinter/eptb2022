#' Maisons : les graphiques
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
fn33_graphiques_maisons <- function(data = doc_frcordep_ssfiltre) {
  # G3 maisons prix moyen au m2 de plancher ---------
#
max_y <- 100 * ceiling(data %>%
                         dplyr::filter(territoire %in% c("Corse", "France")) %>%
                         dplyr::pull(prix_m2) %>% max() /
                         100)

data %>%
  dplyr::filter(territoire %in% c("Corse", "France"))  %>%
  ggplot2::ggplot(ggplot2::aes(
    annee,
    prix_m2,
    group = territoire,
    shape = territoire,
    color = territoire
  )) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::scale_size(guide = "none") +
  ggplot2::scale_y_continuous(
    name = "Prix moyen au m\u00b2 de surface de plancher",
    limit = c(1000, max_y),
    breaks = seq(1000, max_y, 100)
  ) +

  ggplot2::scale_color_manual(values = c("darkorange3", "darkgoldenrod1")) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = c(0.55, 0.75),
    legend.justification = c(1.2, 0)
  ) +
  ggplot2::labs(title = "Prix moyen des maisons individuelles",
                caption = "Source :  SDES enqu\u00eate EPTB") -> plot_mai_prixm2

plot_mai_prixm2

ggplot2::ggsave(
  file.path("4_resultats",
            ls_dates[["annee_etude"]],
            "Cartes",
            "corfrm_prixmoy_maisons_m2.png"),
  type = "cairo",
  width = 22,
  height = 20,
  unit = "cm",
  dpi = 300
)
ls_cowplot$plot_mai_prixm2 <- plot_mai_prixm2

# G4 maisons surface moyenne de plancher ---------
#
max_y <- 10 * ceiling(data %>%
                        dplyr::filter(territoire %in% c("Corse", "France")) %>%
                        dplyr::pull(surf_m2) %>% max() /
                        10)

data %>%
  dplyr::filter(territoire %in% c("Corse", "France"))  %>%
  ggplot2::ggplot(ggplot2::aes(
    annee,
    surf_m2,
    group = territoire,
    shape = territoire,
    color = territoire
  )) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::scale_size(guide = "none") +
  ggplot2::scale_y_continuous(
    name = "Surface moyenne de plancher (m\u00b2)",
    limit = c(100, max_y),
    breaks = seq(100, max_y, 10)
  ) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = c("darkorange3", "darkgoldenrod1")) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = c(0.35, 0.75),
    legend.justification = c(1.2, 0)
  ) +
  ggplot2::labs(title = "Surface moyenne de plancher des maisons individuelles",
                caption = "Source :  SDES enqu\u00eate EPTB") -> plot_mai_surf

plot_mai_surf

ggplot2::ggsave(
  file.path("4_resultats",
            ls_dates[["annee_etude"]],
            "Cartes",
            "corfrm_surfmoy_maisons.png"),
  type = "cairo",
  width = 22,
  height = 20,
  unit = "cm",
  dpi = 300
)
return(ls_cowplot)
}
