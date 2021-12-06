#' Terrains et maisons : on colle deux graphiques
#'
#' @param x un plot
#' @param y un plot
#'
#' @return
#' @importFrom cowplot plot_grid
#' @importFrom cowplot save_plot
#' @export
#'
fn33b_cowplot_maison_terrain <-
  function(x = ls_cowplot$plot_terr_prixm2,
           y = ls_cowplot$plot_mai_prixm2) {

    plot1by1 <-
      cowplot::plot_grid(x, y, align = "h")

    cowplot::save_plot(
      file.path(
        "4_resultats",
        ls_dates[["annee_etude"]],
        "Graphes",
        "corfrm_prix_terrains_maisons.png"
      ),
      plot1by1,
      ncol = 2,
      nrow = 1,
      # each individual subplot should have an aspect ratio of 1.3
      base_aspect_ratio = 1.3
    )
    return(plot1by1)
  }
