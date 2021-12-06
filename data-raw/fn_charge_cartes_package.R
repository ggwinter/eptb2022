#' fn_charge_cartes_package
#'
#' @param chem_sig chemin vers le geopackage
#'
#' @return liste de cartes
#' @importFrom sf st_layers
#' @importFrom sf st_read
#' @importFrom sf st_drop_geometry
fn_charge_cartes_package <-
  function(chem_sig = 'E:/A_Bureau/R/EPTB/2_data/maps/atlas.gpkg') {
    sf::st_layers(chem_sig)
    sf::st_read(chem_sig, "europe") -> map_eu
    sf::st_read(chem_sig, "regions") -> map_reg
    map_reg_pt <- map_reg %>% sf::st_drop_geometry()

    list('map_eu' = map_eu,
         'map_reg' = map_reg,
         'map_reg_pt' = map_reg_pt) -> ls_carto

    # usethis::use_data(ls_carto)
    #
    return(ls_carto)

  }

