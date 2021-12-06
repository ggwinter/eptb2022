#' Terrains : cartographie
#'
#' F1 Préparation de la cartographie pour les terrains des régions
#' métropolitaines filtre.
#' calcul pour l'annee d'étudiée des étiquettes de la carte représentée
#' sous forme de quartile
#' @param data dataframe
#'
#' @return
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_sf
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 ggsave
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer
#' @importFrom purrr set_names
#' @importFrom purrr map2
#' @importFrom purrr walk2
#' @importFrom stringr str_c
#'
#'
#' @export
fn30_carto_terrains_indics <-
  function(data = tli_regdep_filtre[["filtre_reg"]]) {

    # graphe des regions
    data %>%
      dplyr::filter(annee %in% ls_dates[["annee_etude"]], reg_cd > "06") %>%
      dplyr::select(reg_cd, reg_lib, annee, terr_prixmoy_m2, terr_surfmoy_m2) %>% # -> eff
      ggplot2::ggplot(
        ggplot2::aes(
          x = terr_surfmoy_m2,
          y = terr_prixmoy_m2,
          label = reg_lib,
          group = reg_cd,
          colour = reg_cd
        )
      ) +
      ggplot2::geom_point() +
      ggrepel::geom_text_repel() +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(
        title = stringr::str_c(
          "Terrains achetés - Comparaisons régionales pour ",
          ls_dates[["annee_etude"]],
          "\nSurface moyenne et prix moyen"
        ),
        x  = "Surface moyenne en m\u00b2",
        y  = "Prix moyen en € par m\u00b2"
      ) -> p_terr_reg


    p_terr_reg

    ggplot2::ggsave(
      file.path("4_resultats",
                ls_dates[["annee_etude"]],
                "Graphes",
                "regfrm_surf_prix_terrain.png"),
      type = "cairo",
      width = 14,
      height = 14,
      unit = "cm",
      dpi = 300
    )

    # cartes des regions
    toto <-
      data %>%
      dplyr::filter(annee %in% ls_dates[['annee_etude']] &
                      reg_cd > "06") %>%
      dplyr::select(-c(secret_stat, nb_obsnp, terr_surftot, terr_prixtot, proj_mtt)) %>%
      dplyr::mutate(part = part * 100,
                    part_prixterrain = part_prixterrain * 100) %>%
      tidyr::pivot_longer(
        cols = part:proj_mttmoy,
        names_to = "indic2",
        values_to = "valeur"
      )

    unique(toto$indic2) %>% purrr::set_names() -> v_indics2
    v_indics2_titre <-
      c(
        "Part du nombre de terrains - France entière",
        "Nombre de terrains",
        "Surface moyenne des terrains (m\u00b2)",
        "Prix moyen des terrains en euros",
        "Prix moyen au m\u00b2 des terrains en euros",
        "Part du prix du terrain dans le coût total du projet en %",
        "Montant moyen du projet en euros"
      )

    purrr::map2(v_indics2, v_indics2_titre,
                ~cree_carte_terrains(data = toto, .x, .y)) -> plot_ls_carto

    file.path("4_resultats",
              ls_dates[["annee_etude"]],
              "Cartes",
              stringr::str_c("carto_", names(plot_ls_carto), ".png")) -> v_noms

    purrr::walk2(
      plot_ls_carto,
      v_noms,
      ~ ggplot2::ggsave(
        plot = .x,
        filename = .y,
        type = "cairo",
        width = 14,
        height = 14,
        unit = "cm",
        dpi = 300
      )
    )
    return(plot_ls_carto)
  }
