#' Normalise les tableaux geokit terrains pour les differents territoires
#' meme nombre de colonnes et memes champs
#' @param fich_frregdep_filtre vecteur de caracteres
#' @return
#' @export
#' @importFrom dplyr all_of
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr rename_at
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom dplyr vars
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom readxl read_excel
#'
fn05_transforme_tab_gk_filtre_frregdep <-
  function(fich_frregdep_filtre = c("filtre_fr", "filtre_reg", "filtre_dep")) {
    fn_lit_fich_depreg_filtre <- function(x) {
      tab <-
        readxl::read_excel(ls_import[["path_eptb"]], x)
      champs0 <- names(tab)
      champs <-
        ls_import[["T_onglets_champs"]]$champs[which(ls_import[["T_onglets_champs"]]$champs0 %in% champs0 &
                                                       ls_import[["T_onglets_champs"]]$onglet %in% x)]

      tab <- tab %>%
        dplyr::rename_at(dplyr::vars(dplyr::all_of(champs0)), function(x)
          champs) %>%
        fn91_terrains_calcul_5indics() %>%
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
          fn92_terrains_calcul_part() %>%
          dplyr::mutate(type = "fr") %>%
          dplyr::ungroup()

      } else{
        if (x %in% "filtre_reg") {
          tab <- tab %>%
            dplyr::mutate(dep_cd = "NA", dep_lib = "NA") %>%
            dplyr::group_by(annee) %>%
            fn92_terrains_calcul_part() %>%
            dplyr::mutate(type = "reg") %>%
            dplyr::ungroup()
        } else{
          if (x %in% "filtre_dep") {
            tab <- tab %>%
              dplyr::group_by(annee, reg_cd) %>%
              fn92_terrains_calcul_part() %>%
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
      tab
    }

    # test fonction
    # x <- "filtre_dep"
    # fn_lit_fich_depreg_filtre(x)

    tli_regdep_filtre <-
      purrr::map(fich_frregdep_filtre, fn_lit_fich_depreg_filtre) %>%
      purrr::set_names(fich_frregdep_filtre)

    return(tli_regdep_filtre)
  }
