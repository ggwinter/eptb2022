#' Plaquette  : Réalisation de la plaquette
#'
#' Enregistre le contenu des tableaux avec les indicateurs calcules
#'
#' @param data liste de dataframe
#'
#' @return
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr contains
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr vars
#' @importFrom rio export
#' @importFrom stringr str_replace
#' @importFrom tidyr separate
#' @export
fn20_mef_tableaux_plaquette <-
  function(data = list(
    "doc_frcordep_filtre" = doc_frcordep_filtre,
    "tab2_terrains" = tab2_terrains,
    "doc_frcordep_ssfiltre" = doc_frcordep_ssfiltre,
    "tab4_maisons" = tab4_maisons
  )) {
    resultats <- list()
    resultats <- list("annee_etude" = ls_dates[["annee_etude"]],
                      "annee_precedente" = ls_dates[["annee_prec"]])

    # H1 Tab 1 Terrains corse, departements, FR ---------
    #
    ind_dec = c("prix_m2", "prix", "surf_m2", "cout_projet")
    ind_pourcent = c("part", "part_projet")
    ind_part = c("Part")

    tab1_terrains_cordepfr <- data$doc_frcordep_filtre %>%
      dplyr::filter(annee %in% ls_dates[["annee_etude"]]) %>%
      dplyr::select(-annee) %>%
      dplyr::mutate_at(ind_dec, ~ format_dec(.x)) %>%
      dplyr::mutate_at(ind_pourcent, ~ format_pourcent(.x))


    tab1_terrains_cordepfr %>%
      dplyr::select(-type) %>%
      dplyr::rename(
        "Territoire" = "territoire",
        "Part" = "part",
        "Prix au m\u00b2" = "prix_m2",
        "Surface en m\u00b2" = "surf_m2",
        "Prix du terrain" = "prix",
        "Part terrain" = "part_projet",
        "Co\u00fbt du projet" = "cout_projet"
      )  %>%
      dplyr::mutate(Territoire = factor(
        Territoire,
        levels = c("Corse-du-Sud",
                   "Haute-Corse",
                   "Corse",
                   "France")
      )) %>%
      dplyr::arrange(Territoire) -> resultats[["tab1"]]

    resultats[["tab1"]] %>%
      dplyr::mutate(Part = dplyr::case_when(Territoire %in% "France" ~ "",
                                            TRUE ~ Part)) -> resultats[["tab1"]]

    # H2 Tab 2 Terrains corse pour 4 indicateurs --------


    tab2_terrains_4indics <- data$tab2_terrains %>%
      dplyr::filter(annee %in% ls_dates[["annee_etude"]]) %>%
      dplyr::mutate_at(ind_dec, ~ format_dec(.x)) %>%
      dplyr::mutate_at(ind_pourcent, ~ format_pourcent(.x)) %>%
      dplyr::arrange(indic, indic_cat) %>%
      tidyr::separate(
        indic_cat,
        into = c("indic_cat_cd", "indic_cat"),
        sep = " : ",
        remove = TRUE,
        convert = TRUE
      ) %>%
      dplyr::mutate(
        indic = dplyr::case_when(
          indic %in% "age" ~ "Age de l'acquéreur",
          indic %in% "csp" ~ "Catégorie socio-professionnelle",
          indic %in% "rp" ~ "Résidence principale",
          indic %in% "viabilisation" ~ "Viabilisation du terrain",
          !indic %in% c("age", "csp", "rp", "viabilisation") ~
            "Erreur d'analyse"

        )
      ) %>%
      dplyr::rename(
        c(
          "Indicateur" = "indic_cat",
          "Part" = "part",
          "Prix au m\u00b2" = "prix_m2",
          "Surface en m\u00b2" = "surf_m2",
          "Prix du terrain" = "prix",
          "Part terrain" = "part_projet",
          "Co\u00fbt du projet" = "cout_projet"
        )
      ) %>%
      dplyr::mutate_at(ind_part,
                       ~ stringr::str_replace(.x, "NA %", "NA"))

    resultats[["tab2"]] <-
      tab2_terrains_4indics %>% dplyr::select(-c(annee, indic, indic_cat_cd))



    #  H3 Tab 3 maisons par territoires --------
    ind_dec = c("prix_m2", "prix")
    ind_pourcent = c("part")
    ind_part = c("Part")


    tab3_maisons_cordepfr <- data$doc_frcordep_ssfiltre %>%
      dplyr::filter(annee %in% ls_dates[["annee_etude"]]) %>%
      dplyr::select(-annee) %>%
      dplyr::mutate_at(ind_dec, ~ format_dec(.x)) %>%
      dplyr::mutate_at(ind_pourcent, ~ format_pourcent(.x))

    tab3_maisons_cordepfr %>%
      dplyr::select(-type) %>%
      dplyr::rename(
        "Territoire" = "territoire",
        "Part" = "part",
        "Prix au m\u00b2" = "prix_m2",
        "Surface en m\u00b2" = "surf_m2",
        "Prix de la maison" = "prix"
      ) %>%
      dplyr::mutate(Territoire = factor(
        Territoire,
        levels = c("Corse-du-Sud",
                   "Haute-Corse",
                   "Corse",
                   "France")
      )) %>%
      dplyr::arrange(Territoire) -> resultats[["tab3"]]

    resultats[["tab3"]] %>%
      dplyr::mutate(Part = dplyr::case_when(Territoire %in% "France" ~ "",
                                            TRUE ~ Part)) -> resultats[["tab3"]]


    #  H4 Tab 4 maisons corse pour 4 indicateurs

    tab4_maisons_3indics <- data$tab4_maisons %>%
      dplyr::filter(annee %in% ls_dates[["annee_etude"]]) %>%
      dplyr::mutate_at(ind_dec, ~ format_dec(.x)) %>%
      dplyr::mutate_at(ind_pourcent, ~ format_pourcent(.x)) %>%
      dplyr::arrange(indic, indic_cat) %>%
      dplyr::filter(indic %in% c("chauffage", "moe", "finition")) %>%
      tidyr::separate(
        indic_cat,
        into = c("indic_cat_cd", "indic_cat"),
        sep = " : ",
        remove = TRUE,
        convert = TRUE
      ) %>%
      dplyr::mutate(
        indic = dplyr::case_when(
          indic %in% "chauffage" ~ "Mode de chauffage",
          indic %in% "moe" ~ "Ma\u00eetre d'\u0153uvre",#attention a oe code incertain
          indic %in% "finition" ~ "Finition",
          !indic %in% c("chauffage", "moe", "finition") ~
            "Erreur d'analyse"

        )
      )  %>%
      dplyr::rename(
        "Indicateur" = "indic_cat",
        "Part" = "part",
        "Prix au m\u00b2" = "prix_m2",
        "Surface en m\u00b2" = "surf_m2",
        "Prix de la maison" = "prix"
      ) %>%
      dplyr::mutate_at(ind_part,
                       ~ stringr::str_replace(.x, "NA %", "NA"))


    resultats[["tab4"]] <-
      tab4_maisons_3indics %>% dplyr::select(-c(annee, indic, indic_cat_cd))

    saveRDS(resultats,
            file.path("4_resultats",
                      ls_dates[["annee_etude"]],
                      "Tableaux",
                      "resultats.RDS"))

    rio::export(
      resultats,
      file.path("4_resultats",
                ls_dates[["annee_etude"]],
                "Tableaux",
                "resultats.xlsx"),
      overwrite = TRUE
    )

    return(resultats)

  }
