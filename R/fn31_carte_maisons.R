#' Maisons : cartographie
#'
#' @param data dataframe
#' @return nothing
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_sf
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 theme_void
#' @importFrom dplyr left_join
#' @importFrom sf st_crs
#' @export
fn31_carte_maisons <-
  function(data = tli_regdep_ssfiltre[["ssfiltre_reg"]]) {

    map_ssfiltre <- ls_carto$map_reg %>%
      dplyr::left_join(data, by = c("CODE_REG" = "reg_cd"))

    p <- ggplot2::ggplot(ls_carto$map_eu) +
      ggplot2::geom_sf(ggplot2::aes(group = NAME),
                       colour = "lightgrey",
                       size = 0.1) +
      ggplot2::coord_sf(crs = sf::st_crs(ls_carto$map_reg),
                        datum = NA) +
      ggplot2::geom_sf(ggplot2::aes(fill = mai_prixmoy_m2), data =
                         map_ssfiltre[map_ssfiltre$annee %in% ls_dates[["annee_etude"]], ]) +
      # scale_fill_continuous(type = "viridis") +
      # scale_fill_gradient(low = "darkorange4", high = "gold") +
      ggplot2::scale_fill_gradientn(colors = c("darkorange4", "darkorange2", "goldenrod2", "gold")) +
      ggplot2::labs(title = "Prix moyen des maisons \npar m2 de surface de plancher",
                    fill = "")

    p + ggplot2::geom_text(
      data = ls_carto$map_reg_pt,
      ggplot2::aes(
        x = X,
        y = Y,
        group = NULL,
        fill = NULL,
        label = reg_lib
      ),
      size = 3.0,
      fontface = "bold",
      color = "chartreuse4"
    ) +
      ggplot2::theme_void() -> p

    p

    ggplot2::ggsave(
      file.path(
        "4_resultats",
        ls_dates[["annee_etude"]],
        "Cartes",
        "carto_eu_reg_mai_prixmoy_m2.png"
      ),
      type = "cairo",
      width = 14,
      height = 14,
      unit = "cm",
      dpi = 300
    )
  }
