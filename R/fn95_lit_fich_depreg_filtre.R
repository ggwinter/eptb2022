#' Terrains lit les onglets du fichier excel
#'
#' @param x un vecteur avec les references des onglets
#'
#' @return une liste
#' @importFrom dplyr all_of
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr rename_at
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom dplyr vars
#' @importFrom readxl read_excel
#' @export
fn95_lit_fich_depreg_filtre <- function(x) {
  tab <- readxl::read_excel(path_eptb, x)
  champs0 <- names(tab)
  champs <-
    T_onglets_champs$champs[which(T_onglets_champs$champs0 %in% champs0 &
                                    T_onglets_champs$onglet %in% x)]

  tab <- tab %>%
    dplyr::rename_at(dplyr::vars(dplyr::all_of(champs0)), function(x)
      champs) %>%
    terrains_calcul_5indics() %>%
    dplyr::mutate(secret_stat = dplyr::case_when(nb_obsnp < 11 ~ TRUE,
                                                 nb_obsnp > 10 ~ FALSE))

  if (x %in% "filtre_fr") {
    tab <- tab %>%
      dplyr::mutate(
        reg_cd = "NA",
        reg_lib = "NA",
        dep_cd = "NA",
        dep_lib = "NA"
      ) %>%
      dplyr::group_by(annee) %>%
      terrains_calcul_part() %>%
      dplyr::mutate(type = "fr") %>%
      dplyr::ungroup()

  } else{
    if (x %in% "filtre_reg") {
      tab <- tab %>%
        dplyr::mutate(dep_cd = "NA", dep_lib = "NA") %>%
        dplyr::group_by(annee) %>%
        terrains_calcul_part() %>%
        dplyr::mutate(type = "reg") %>%
        dplyr::ungroup()
    } else{
      if (x %in% "filtre_dep") {
        tab <- tab %>%
          dplyr::group_by(annee, reg_cd) %>%
          terrains_calcul_part() %>%
          dplyr::mutate(type = "dep") %>%
          dplyr::ungroup()
      } else{
        print("pb sur le fichier excel")
      }
    }
  }
  tab <- tab %>%
    dplyr::select(dplyr::one_of(
      c(
        "reg_cd",
        "reg_lib",
        "dep_cd",
        "dep_lib",
        "annee",
        "part",
        "nb_obsnp",
        "terr_nb",
        "terr_surfmoy_m2",
        "terr_prixmoy",
        "terr_prixmoy_m2",
        "part_prixterrain",
        "proj_mttmoy",
        "secret_stat",
        "terr_surftot",
        "terr_prixtot",
        "proj_mtt",
        "type"
      )
    ))
  return(tab)
}
