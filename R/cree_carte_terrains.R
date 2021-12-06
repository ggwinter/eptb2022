#' Dessine une carte
#'
#'
#' @param data dataframe
#' @param x caractere axe des x
#' @param y caractere axe des y
#' @return plot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_sf
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom grid unit
#' @importFrom sf st_crs
cree_carte_terrains <-
  function(data = toto, x = "terr_prixmoy_m2", y = "Prix moyen des terrains en euros") {

    data("ls_carto", package = "eptb2022")
    map_reg_pt <- ls_carto$map_reg %>% sf::st_drop_geometry()

    data %>% dplyr::filter(indic2 %in% x) -> ma_table


    eff4 <- stats::quantile(ma_table$valeur,
                            seq(0, 1, by = 0.25))
    if (eff4[5] < 100) {
      eff4[1] - 0.1 -> eff4[1]
      eff4[5] + 0.1 -> eff4[5]
    } else{
      if (eff4[5] < 5000) {
        10 * round(eff4 / 10, 0) -> eff4
        eff4[1] - 10 -> eff4[1]
        eff4[5] + 10 -> eff4[5]
      } else{
        100 * round(eff4 / 100, 0) -> eff4
        eff4[1] - 100 -> eff4[1]
        eff4[5] + 100 -> eff4[5]
      }
    }



    etiquettes <-
      c(
        paste0("de ", eff4[1], " \u00e3 ", eff4[2]),
        paste0("de ", eff4[2], " \u00e3 ", eff4[3]),
        paste0("de ", eff4[3], " \u00e3 ", eff4[4]),
        paste0("de ", eff4[4], " \u00e3 ", eff4[5])
      )

    # eff5 <- c(unname(floor(100 * eff4[1]) / 100), 0.29, 0.33, 0.37, Inf)
    # etiquettes2 <-
    #   c(
    #     paste0("de ", 100 * eff5[1], " à ", 100 * eff5[2] - 1, "%"),
    #     paste0("de ", 100 * eff5[2], " à ", 100 * eff5[3] - 1, "%"),
    #     paste0("de ", 100 * eff5[3], " à ", 100 * eff5[4] - 1, "%"),
    #     paste0("de ", 100 * eff5[4], " à ", floor(100 * eff4[5] + 1), "%")
    #   )
    ma_table <- ma_table %>%
      dplyr::mutate(
        ind_test =
          cut(
            valeur,
            breaks = eff4,
            labels = etiquettes,
            include.lowest = FALSE,
            right = TRUE,
            ordered_result = TRUE
          )
      )

    eff <- ma_table %>%
      dplyr::select(reg_cd,
                    indic2,
                    valeur,
                    ind_test)

    map1 <- ls_carto$map_reg %>%
      dplyr::left_join(eff, by = c("CODE_REG" = "reg_cd"))

    ggplot2::ggplot(ls_carto$map_eu) +
      ggplot2::geom_sf(ggplot2::aes(group = NAME),
                                               colour = "lightgrey", size = 0.1) +
      ggplot2::coord_sf(crs = sf::st_crs(map1), datum = NA) +
      ggplot2::geom_sf(
        ggplot2::aes(fill = ind_test),
        colour = "lightgrey",
        size = 0.1,
        data = map1
      ) +
      ggplot2::coord_sf(crs = sf::st_crs(map1), datum = NA) +
      ggplot2::scale_fill_manual(values = c("darkorange4", "darkorange2", "goldenrod2", "gold")) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          direction = "horizontal",
          title.position = "top",
          label.position = "bottom",
          label.hjust = 0.5,
          label.vjust = 0.5,
          keyheight = 0.3,
          keywidth = 1.0,
          default.unit = "cm"
        )
      ) +
      ggplot2::labs(title = y) +
      ggplot2::geom_text(
        data = map_reg_pt,
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
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.spacing.x =  grid::unit(1.0, 'cm'),
        legend.position = "bottom"
      ) -> p_carto


    return(p_carto)
  }
