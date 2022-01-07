#' Lit les deux tableaux issus de beyond
#'
#'
#' Lit le repertoire, cherche les deux fichiers csv extraits manuellement
#' par l'utilisateur sur le site du sdes au format beyond puis les transforme
#' NB pourrait etre ajoute à la requete sous geokit
#'
#' @param x nom du repertoire ou se trouvent les 2 fichiers csv (format caractere)
#' @importFrom stringr str_replace
#' @importFrom purrr set_names
#' @importFrom purrr map
#' @importFrom readr write_csv
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr ungroup
#' @importFrom here here
#' @importFrom magick image_read
#' @importFrom magick image_write
#' @importFrom tibble tribble
#' @importFrom magrittr %>%
#' @return une liste de tables
#' @export
fn03_import_tab_beyond <- function(x  = "2_data") {
  if (exists("ls_dates", envir = .GlobalEnv)) {
    if (dir.exists(file.path(getwd(), "4_resultats",
                             ls_dates[["annee_etude"]])) == FALSE) {
      dir.create(file.path(getwd(), "4_resultats", ls_dates[["annee_etude"]]))
      dir.create(file.path(getwd(), "4_resultats", ls_dates[["annee_etude"]], "Cartes"))
      dir.create(file.path(getwd(), "4_resultats", ls_dates[["annee_etude"]], "Charte_graphique"))
      dir.create(file.path(getwd(), "4_resultats", ls_dates[["annee_etude"]], "Graphes"))
      dir.create(file.path(getwd(), "4_resultats", ls_dates[["annee_etude"]], "Images"))
      dir.create(file.path(getwd(), "4_resultats", ls_dates[["annee_etude"]], "Tableaux"))


      magick::image_read(system.file("qrcode.png", package = "eptb2022"))-> qrcode
      magick::image_write(
        qrcode,
        path = file.path(
          getwd(),
          "4_resultats",
          ls_dates[["annee_etude"]],
          "Charte_graphique",
          "qrcode.png"
        ),
        format = "png"
      )


      magick::image_read(system.file("logo.png", package = "eptb2022"))-> logo
      magick::image_write(
        logo,
        path = file.path(
          getwd(),
          "4_resultats",
          ls_dates[["annee_etude"]],
          "Charte_graphique",
          "logo.png"
        ),
        format = "png"
      )

      magick::image_read(system.file("image.png", package = "eptb2022"))-> image

      magick::image_write(
        image,
        path = file.path(
          getwd(),
          "4_resultats",
          ls_dates[["annee_etude"]],
          "Charte_graphique",
          "image.png"
        ),
        format = "png"
      )
      rm(qrcode, logo, image)

    }

    if (exists(file.path(getwd(), "3_tables", "t_noms_reg.csv")) == FALSE) {
      t_noms_reg <- tibble::tribble(
        ~ reg_cd,        ~ reg_lib,        ~ reg,
        11L,        "\u00cele-de-France",        "ILE-DE-FRANCE",
        24L,        "Centre-Val de Loire",        "CENTRE-VAL DE LOIRE",
        27L,        "Bourgogne-Franche-Comt\u00e9",        "BOURGOGNE-FRANCHE-COMTE",
        28L,        "Normandie",        "NORMANDIE",
        32L,        "Hauts-de-France",        "NORD-PAS-DE-CALAIS-PICARDIE",
        44L,        "Grand Est",        "ALSACE-CHAMPAGNE-ARDENNE-LORRAINE",
        52L,        "Pays de la Loire",        "PAYS DE LA LOIRE",
        53L,        "Bretagne",        "BRETAGNE",
        75L,        "Nouvelle-Aquitaine",        "AQUITAINE-LIMOUSIN-POITOU-CHARENTES",
        76L,        "Occitanie",        "LANGUEDOC-ROUSSILLON-MIDI-PYRENEES",
        84L,        "Auvergne-Rh\u00f4ne-Alpes",        "AUVERGNE-RHONE-ALPES",
        93L,        "Provence-Alpes-C\u00f4te d'Azur",        "PROVENCE-ALPES-COTE D'AZUR",
        94L,        "Corse",        "CORSE",
        00L,        "DOM",        "DOM",
        999L,        "France",        "FRANCE ENTIERE"
      )

      readr::write_csv(t_noms_reg,
                       file.path(getwd(), "3_tables", "t_noms_reg.csv"))

    }
  }

  # import des fichiers beyond
  list.files(here::here(x, ls_dates[["annee_etude"]]), pattern = "csv") -> fich_beyond
  if (length(fich_beyond) == 2) {
    fich_beyond %>% stringr::str_replace(".csv", "") %>%
      stringr::str_replace("(?<=eptb-d[:digit:]).*", "") %>%
      stringr::str_replace("eptb-d5", "terrains") %>%
      stringr::str_replace("eptb-d6", "maisons") -> eff

    fich_beyond %>% purrr::set_names(eff) -> fich_beyond
    rm(eff)

    purrr::map(
      fich_beyond,
      ~ readr::read_delim(
        file.path("2_data", ls_dates[["annee_etude"]], .x),
        delim = ";",
        locale = readr::locale("fr", encoding = "latin1"),
        skip = 3,
        col_names = FALSE,
        show_col_types = FALSE
      ) %>% purrr::set_names(
        c(
          "annee",
          "reg",
          "nb",
          "prix_m2",
          "prix_m2q1",
          "prix_m2q2",
          "prix_m2q3",
          "surf_m2",
          "prix"
        )
      ) %>% dplyr::filter(annee %in% ls_dates[["annee_etude"]]) %>%
        dplyr::left_join(t_noms_reg, by = 'reg') %>%
        dplyr::select(annee, reg_lib, nb:prix)
    ) -> ls_beyond


    purrr::map(
      ls_beyond,
      ~ .x %>% dplyr::filter(!reg_lib %in% c("DOM", "France")) %>%
        dplyr::group_by(annee) %>%
        dplyr::mutate(dplyr::across(nb:prix, fn90_classement)) %>%
        dplyr::mutate(dplyr::across(nb:prix, ~ 14 - .x)) %>%
        dplyr::ungroup()
    ) %>%
      purrr::set_names(c("terrains_clt", "maisons_clt")) -> ls_beyond_classt

    c(ls_beyond, ls_beyond_classt) -> ls_beyond
    rm(ls_beyond_classt)

  } else{
    ls_beyond <- list()
  }
  if(length(ls_beyond)==0) print("Copier les fichier beyond dans 2_data")

  return(ls_beyond)

}
