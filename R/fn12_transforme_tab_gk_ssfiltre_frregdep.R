#' Maisons : normalise les tableaux geokit maisons pour les differents territoires.
#' toto.
#' @param fich_frregdep_ssfiltre un vecteur de caracteres
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
fn12_transforme_tab_gk_ssfiltre_frregdep <-
  function(fich_frregdep_ssfiltre = c("ssfiltre_fr", "ssfiltre_reg", "ssfiltre_dep")) {
    fn_lit_fich_depreg_ssfiltre <- function(x = "ssfiltre_reg") {
      tab <- readxl::read_excel(ls_import[["path_eptb"]], x)
      champs0 <- names(tab)
      champs <-
        ls_import[["T_onglets_champs"]]$champs[which(ls_import[["T_onglets_champs"]]$champs0 %in% champs0 &
                                        ls_import[["T_onglets_champs"]]$onglet %in% x)]

      tab <- tab %>%
        dplyr::rename_at(dplyr::vars(dplyr::all_of(champs0)), function(x)
          champs) %>%
        fn93_maisons_calcul_3indics() %>%
        dplyr::mutate(secret_stat = dplyr::case_when(nb_obsnp < 11 ~ TRUE,
                                                     nb_obsnp > 10 ~ FALSE))

      if (x %in% "ssfiltre_fr") {
        tab <- tab %>%
          dplyr::mutate(
            reg_cd = "NA",
            reg_lib = "NA",
            dep_cd = "NA",
            dep_lib = "NA"
          ) %>%
          dplyr::group_by(annee) %>%
          fn94_maisons_calcul_part() %>%
          dplyr::mutate(type = "fr") %>%
          dplyr::ungroup()

      } else{
        if (x %in% "ssfiltre_reg") {
          tab <- tab %>%
            dplyr::mutate(dep_cd = "NA", dep_lib = "NA") %>%
            dplyr::group_by(annee) %>%
            fn94_maisons_calcul_part() %>%
            dplyr::mutate(type = "reg") %>%
            dplyr::ungroup()
        } else{
          if (x %in% "ssfiltre_dep") {
            tab <- tab %>%
              dplyr::group_by(annee, reg_cd) %>%
              fn94_maisons_calcul_part() %>%
              dplyr::mutate(type = "dep") %>%
              dplyr::ungroup()
          } else{
            print("pb sur le fichier excel")
          }
        }
      }

      tab <- tab %>%
        dplyr::select(
          dplyr::one_of(
            "reg_cd",
            "reg_lib",
            "dep_cd",
            "dep_lib",
            "annee",
            "part",
            "nb_obsnp",
            "mai_nb",
            "mai_surftot",
            "mai_prixtot",
            "mai_surfmoy_m2",
            "mai_prixmoy",
            "mai_prixmoy_m2",
            "secret_stat",
            "type"
          )
        )
      return(tab)
    }

    tli_regdep_ssfiltre <-
      purrr::map(fich_frregdep_ssfiltre, fn_lit_fich_depreg_ssfiltre) %>%
      purrr::set_names(fich_frregdep_ssfiltre)
    return(tli_regdep_ssfiltre)
  }
