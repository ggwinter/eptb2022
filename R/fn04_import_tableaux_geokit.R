#' Lit les tableaux geokit
#'
#' Lit le repertoire, cherche le fichiers xlsx issus de geokit
#' ouvre le fichier, lit les onglets
#'
#' @param x nom du repertoire ou se trouvent les 2 fichiers csv (character)
#' @importFrom here here
#' @importFrom readxl excel_sheets
#' @importFrom readxl excel_sheets
#' @importFrom readr write_csv
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr ungroup
#' @importFrom here here
#' @importFrom tibble tribble
#' @importFrom magrittr %>%
#' @return une liste de tables
#' @export
fn04_import_tableaux_geokit <- function(x  = "2_data") {

  ls_import <- list()
  # import des fichiers beyond
  if (file.exists(here::here("2_data", ls_dates[["annee_etude"]], "EPTB_aggrege.xlsx"))) {
    path_eptb <- here::here("2_data", ls_dates[["annee_etude"]], "EPTB_aggrege.xlsx")

    ls_import[["path_eptb"]] <- path_eptb

    ongl_fich1 <- readxl::excel_sheets(ls_import[["path_eptb"]])


    # x <- 1
    lit_champs1 <- function(x) {
      tab <- readxl::read_xlsx(path = ls_import[["path_eptb"]],
                               sheet = x,
                               n_max = 1)
      chps0 <-
        dplyr::tibble("sheet" = ongl_fich1[x], "champs0" = names(tab))
    }

    chps0_ls1 <- purrr::map_df(seq_along(ongl_fich1), lit_champs1)

    champs0_liste1 <- chps0_ls1 %>%
      dplyr::distinct(champs0)

    # table de liaison noms des champs courts et longs
  T_champs1 <- tibble::tribble(
                                                                     ~champs0,           ~champs,
                                                   "eptb - Code de la région",             "reg",
                                                "eptb - Libellé de la région",         "reg_lib",
                                                 "eptb - Code du département",             "dep",
                                              "eptb - Libellé du département",         "dep_lib",
                                                               "eptb - Année",           "annee",
                                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp",
                           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb",
                                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot",
                                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot",
                                 "eptb - Nombre de terrains (achetés ou non)",         "terr_nb",
                                  "eptb - Prix total des terrains (en euros)",    "terr_prixtot",
                 "eptb - Superficie totale des terrains achetés (en m\u00b2)",    "terr_surftot",
                     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot",
                 "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt",
                                          "eptb - Période d'achat du terrain",         "periode",
                                 "eptb - Classe d'\u00e2ge du chef de ménage",             "age",
                                               "eptb - Achat du terrain en N",     "terr_achatn",
                                                   "eptb - Degré de finition",        "finition",
                                               "eptb - CSP du chef de ménage",             "csp",
                                                "eptb - Résidence principale",              "rp",
                                                    "eptb - MOE constructeur",             "moe",
                                            "eptb - Viabilisation du terrain",   "viabilisation",
                                                   "eptb - Mode de chauffage",       "chauffage"
                 )

  recode_champ1 <- function(x) {
    T_champs1$champs[which(T_champs1$champs0 %in% x)]
  }
  ls_import[["T_champs1"]] <- T_champs1
  chps0_ls1 <- chps0_ls1 %>%
    dplyr::left_join(T_champs1, by = "champs0")


  # lecture des 3_tables pour renommer les champs du fichier excel

  T_onglets_champs <-tibble::tribble(
                    ~onglet,                                                     ~champs0,          ~champs1,      ~type,           ~champs, ~tri,
               "filtre_dep",                                   "eptb - Code de la région",             "reg",   "filtre",          "reg_cd",   1L,
               "filtre_dep",                                "eptb - Libellé de la région",         "reg_lib",   "filtre",         "reg_lib",   2L,
               "filtre_dep",                                 "eptb - Code du département",             "dep",   "filtre",          "dep_cd",   3L,
               "filtre_dep",                              "eptb - Libellé du département",         "dep_lib",   "filtre",         "dep_lib",   4L,
               "filtre_dep",                                               "eptb - Année",           "annee",   "filtre",           "annee",   5L,
               "filtre_dep",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp",   "filtre",        "nb_obsnp",   6L,
               "filtre_dep",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb",   "filtre",          "mai_nb",   7L,
               "filtre_dep",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot",   "filtre",     "mai_surftot",   8L,
               "filtre_dep",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot",   "filtre",     "mai_prixtot",   9L,
               "filtre_dep",                 "eptb - Nombre de terrains (achetés ou non)",         "terr_nb",   "filtre",         "terr_nb",  10L,
               "filtre_dep",                  "eptb - Prix total des terrains (en euros)",    "terr_prixtot",   "filtre",    "terr_prixtot",  11L,
               "filtre_dep", "eptb - Superficie totale des terrains achetés (en m\u00b2)",    "terr_surftot",   "filtre",    "terr_surftot",  12L,
               "filtre_dep",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot",   "filtre", "terrass_surftot",  13L,
               "filtre_dep", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt",   "filtre",        "proj_mtt",  14L,
                "filtre_fr",                                               "eptb - Année",           "annee",   "filtre",           "annee",  15L,
                "filtre_fr",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp",   "filtre",        "nb_obsnp",  16L,
                "filtre_fr",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb",   "filtre",          "mai_nb",  17L,
                "filtre_fr",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot",   "filtre",     "mai_surftot",  18L,
                "filtre_fr",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot",   "filtre",     "mai_prixtot",  19L,
                "filtre_fr",                 "eptb - Nombre de terrains (achetés ou non)",         "terr_nb",   "filtre",         "terr_nb",  20L,
                "filtre_fr",                  "eptb - Prix total des terrains (en euros)",    "terr_prixtot",   "filtre",    "terr_prixtot",  21L,
                "filtre_fr", "eptb - Superficie totale des terrains achetés (en m\u00b2)",    "terr_surftot",   "filtre",    "terr_surftot",  22L,
                "filtre_fr",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot",   "filtre", "terrass_surftot",  23L,
                "filtre_fr", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt",   "filtre",        "proj_mtt",  24L,
               "filtre_reg",                                   "eptb - Code de la région",             "reg",   "filtre",          "reg_cd",  25L,
               "filtre_reg",                                "eptb - Libellé de la région",         "reg_lib",   "filtre",         "reg_lib",  26L,
               "filtre_reg",                                               "eptb - Année",           "annee",   "filtre",           "annee",  27L,
               "filtre_reg",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp",   "filtre",        "nb_obsnp",  28L,
               "filtre_reg",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb",   "filtre",          "mai_nb",  29L,
               "filtre_reg",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot",   "filtre",     "mai_surftot",  30L,
               "filtre_reg",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot",   "filtre",     "mai_prixtot",  31L,
               "filtre_reg",                 "eptb - Nombre de terrains (achetés ou non)",         "terr_nb",   "filtre",         "terr_nb",  32L,
               "filtre_reg",                  "eptb - Prix total des terrains (en euros)",    "terr_prixtot",   "filtre",    "terr_prixtot",  33L,
               "filtre_reg", "eptb - Superficie totale des terrains achetés (en m\u00b2)",    "terr_surftot",   "filtre",    "terr_surftot",  34L,
               "filtre_reg",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot",   "filtre", "terrass_surftot",  35L,
               "filtre_reg", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt",   "filtre",        "proj_mtt",  36L,
             "ssfiltre_dep",                                   "eptb - Code de la région",             "reg", "ssfiltre",          "reg_cd",  37L,
             "ssfiltre_dep",                                "eptb - Libellé de la région",         "reg_lib", "ssfiltre",         "reg_lib",  38L,
             "ssfiltre_dep",                                 "eptb - Code du département",             "dep", "ssfiltre",          "dep_cd",  39L,
             "ssfiltre_dep",                              "eptb - Libellé du département",         "dep_lib", "ssfiltre",         "dep_lib",  40L,
             "ssfiltre_dep",                                               "eptb - Année",           "annee", "ssfiltre",           "annee",  41L,
             "ssfiltre_dep",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp", "ssfiltre",        "nb_obsnp",  42L,
             "ssfiltre_dep",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb", "ssfiltre",          "mai_nb",  43L,
             "ssfiltre_dep",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot", "ssfiltre",     "mai_surftot",  44L,
             "ssfiltre_dep",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot", "ssfiltre",     "mai_prixtot",  45L,
             "ssfiltre_dep",                 "eptb - Nombre de terrains (achetés ou non)",         "terr_nb", "ssfiltre",         "terr_nb",  46L,
             "ssfiltre_dep",                  "eptb - Prix total des terrains (en euros)",    "terr_prixtot", "ssfiltre",    "terr_prixtot",  47L,
             "ssfiltre_dep", "eptb - Superficie totale des terrains achetés (en m\u00b2)",    "terr_surftot", "ssfiltre",    "terr_surftot",  48L,
             "ssfiltre_dep",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot", "ssfiltre", "terrass_surftot",  49L,
             "ssfiltre_dep","eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt", "ssfiltre",        "proj_mtt",  50L,
              "ssfiltre_fr",                                               "eptb - Année",           "annee", "ssfiltre",           "annee",  51L,
              "ssfiltre_fr",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp", "ssfiltre",        "nb_obsnp",  52L,
              "ssfiltre_fr",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb", "ssfiltre",          "mai_nb",  53L,
              "ssfiltre_fr",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot", "ssfiltre",     "mai_surftot",  54L,
              "ssfiltre_fr",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot", "ssfiltre",     "mai_prixtot",  55L,
              "ssfiltre_fr",                 "eptb - Nombre de terrains (achetés ou non)",         "terr_nb", "ssfiltre",         "terr_nb",  56L,
              "ssfiltre_fr",                  "eptb - Prix total des terrains (en euros)",    "terr_prixtot", "ssfiltre",    "terr_prixtot",  57L,
              "ssfiltre_fr", "eptb - Superficie totale des terrains achetés (en m\u00b2)",    "terr_surftot", "ssfiltre",    "terr_surftot",  58L,
              "ssfiltre_fr",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot", "ssfiltre", "terrass_surftot",  59L,
              "ssfiltre_fr","eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt", "ssfiltre",        "proj_mtt",  60L,
             "ssfiltre_reg",                                   "eptb - Code de la région",             "reg", "ssfiltre",          "reg_cd",  61L,
             "ssfiltre_reg",                                "eptb - Libellé de la région",         "reg_lib", "ssfiltre",         "reg_lib",  62L,
             "ssfiltre_reg",                                               "eptb - Année",           "annee", "ssfiltre",           "annee",  63L,
             "ssfiltre_reg",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp", "ssfiltre",        "nb_obsnp",  64L,
             "ssfiltre_reg",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb", "ssfiltre",          "mai_nb",  65L,
             "ssfiltre_reg",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot", "ssfiltre",     "mai_surftot",  66L,
             "ssfiltre_reg",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot", "ssfiltre",     "mai_prixtot",  67L,
             "ssfiltre_reg",                 "eptb - Nombre de terrains (achetés ou non)",         "terr_nb", "ssfiltre",         "terr_nb",  68L,
             "ssfiltre_reg",                  "eptb - Prix total des terrains (en euros)",    "terr_prixtot", "ssfiltre",    "terr_prixtot",  69L,
             "ssfiltre_reg", "eptb - Superficie totale des terrains achetés (en m\u00b2)",    "terr_surftot", "ssfiltre",    "terr_surftot",  70L,
             "ssfiltre_reg",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot", "ssfiltre", "terrass_surftot",  71L,
             "ssfiltre_reg","eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt", "ssfiltre",        "proj_mtt",  72L,
        "achat_terrain_dep",                                   "eptb - Code de la région",             "reg",    "autre",          "reg_cd",  73L,
        "achat_terrain_dep",                                "eptb - Libellé de la région",         "reg_lib",    "autre",         "reg_lib",  74L,
        "achat_terrain_dep",                              "eptb - Libellé du département",         "dep_lib",    "autre",         "dep_lib",  75L,
        "achat_terrain_dep",                                               "eptb - Année",           "annee",    "autre",           "annee",  76L,
        "achat_terrain_dep",                          "eptb - Période d'achat du terrain",         "periode",    "autre",         "periode",  77L,
        "achat_terrain_dep",                 "eptb - Nombre de terrains (achetés ou non)",         "terr_nb",    "autre",         "terr_nb",  78L,
        "achat_terrain_dep",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp",    "autre",        "nb_obsnp",  79L,
        "achat_terrain_reg",                                   "eptb - Code de la région",             "reg",    "autre",          "reg_cd",  80L,
        "achat_terrain_reg",                                "eptb - Libellé de la région",         "reg_lib",    "autre",         "reg_lib",  81L,
        "achat_terrain_reg",                                               "eptb - Année",           "annee",    "autre",           "annee",  82L,
        "achat_terrain_reg",                          "eptb - Période d'achat du terrain",         "periode",    "autre",         "periode",  83L,
        "achat_terrain_reg",                 "eptb - Nombre de terrains (achetés ou non)",         "terr_nb",    "autre",         "terr_nb",  84L,
        "achat_terrain_reg",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp",    "autre",        "nb_obsnp",  85L,
             "ssfiltre_age",                                   "eptb - Code de la région",             "reg", "ssfiltre",          "reg_cd",  86L,
             "ssfiltre_age",                                "eptb - Libellé de la région",         "reg_lib", "ssfiltre",         "reg_lib",  87L,
             "ssfiltre_age",                                               "eptb - Année",           "annee", "ssfiltre",           "annee",  88L,
             "ssfiltre_age",                 "eptb - Classe d'\u00e2ge du chef de ménage",             "age", "ssfiltre",       "indic_cat",  89L,
             "ssfiltre_age",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp", "ssfiltre",        "nb_obsnp",  90L,
             "ssfiltre_age",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb", "ssfiltre",          "mai_nb",  91L,
             "ssfiltre_age",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot", "ssfiltre",     "mai_surftot",  92L,
             "ssfiltre_age",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot", "ssfiltre",     "mai_prixtot",  93L,
             "ssfiltre_age", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt", "ssfiltre",        "proj_mtt",  94L,
             "ssfiltre_age",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot", "ssfiltre", "terrass_surftot",  95L,
       "ssfiltre_achatterr",                                   "eptb - Code de la région",             "reg", "ssfiltre",          "reg_cd",  96L,
       "ssfiltre_achatterr",                                "eptb - Libellé de la région",         "reg_lib", "ssfiltre",         "reg_lib",  97L,
       "ssfiltre_achatterr",                                               "eptb - Année",           "annee", "ssfiltre",           "annee",  98L,
       "ssfiltre_achatterr",                               "eptb - Achat du terrain en N",     "terr_achatn", "ssfiltre",     "terr_achatn",  99L,
       "ssfiltre_achatterr",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp", "ssfiltre",        "nb_obsnp", 100L,
       "ssfiltre_achatterr",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb", "ssfiltre",          "mai_nb", 101L,
       "ssfiltre_achatterr",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot", "ssfiltre",     "mai_surftot", 102L,
       "ssfiltre_achatterr",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot", "ssfiltre",     "mai_prixtot", 103L,
       "ssfiltre_achatterr", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt", "ssfiltre",        "proj_mtt", 104L,
       "ssfiltre_achatterr",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot", "ssfiltre", "terrass_surftot", 105L,
        "ssfiltre_finition",                                   "eptb - Code de la région",             "reg", "ssfiltre",          "reg_cd", 106L,
        "ssfiltre_finition",                                "eptb - Libellé de la région",         "reg_lib", "ssfiltre",         "reg_lib", 107L,
        "ssfiltre_finition",                                               "eptb - Année",           "annee", "ssfiltre",           "annee", 108L,
        "ssfiltre_finition",                                   "eptb - Degré de finition",        "finition", "ssfiltre",       "indic_cat", 109L,
        "ssfiltre_finition",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp", "ssfiltre",        "nb_obsnp", 110L,
        "ssfiltre_finition",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb", "ssfiltre",          "mai_nb", 111L,
        "ssfiltre_finition",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot", "ssfiltre",     "mai_surftot", 112L,
        "ssfiltre_finition",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot", "ssfiltre",     "mai_prixtot", 113L,
        "ssfiltre_finition", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt", "ssfiltre",        "proj_mtt", 114L,
        "ssfiltre_finition",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot", "ssfiltre", "terrass_surftot", 115L,
             "ssfiltre_csp",                                   "eptb - Code de la région",             "reg", "ssfiltre",          "reg_cd", 116L,
             "ssfiltre_csp",                                "eptb - Libellé de la région",         "reg_lib", "ssfiltre",         "reg_lib", 117L,
             "ssfiltre_csp",                                               "eptb - Année",           "annee", "ssfiltre",           "annee", 118L,
             "ssfiltre_csp",                               "eptb - CSP du chef de ménage",             "csp", "ssfiltre",       "indic_cat", 119L,
             "ssfiltre_csp",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp", "ssfiltre",        "nb_obsnp", 120L,
             "ssfiltre_csp",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb", "ssfiltre",          "mai_nb", 121L,
             "ssfiltre_csp",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot", "ssfiltre",     "mai_surftot", 122L,
             "ssfiltre_csp",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot", "ssfiltre",     "mai_prixtot", 123L,
             "ssfiltre_csp", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt", "ssfiltre",        "proj_mtt", 124L,
             "ssfiltre_csp",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot", "ssfiltre", "terrass_surftot", 125L,
              "ssfiltre_rp",                                   "eptb - Code de la région",             "reg", "ssfiltre",          "reg_cd", 126L,
              "ssfiltre_rp",                                "eptb - Libellé de la région",         "reg_lib", "ssfiltre",         "reg_lib", 127L,
              "ssfiltre_rp",                                               "eptb - Année",           "annee", "ssfiltre",           "annee", 128L,
              "ssfiltre_rp",                                "eptb - Résidence principale",              "rp", "ssfiltre",       "indic_cat", 129L,
              "ssfiltre_rp",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp", "ssfiltre",        "nb_obsnp", 130L,
              "ssfiltre_rp",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb", "ssfiltre",          "mai_nb", 131L,
              "ssfiltre_rp",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot", "ssfiltre",     "mai_surftot", 132L,
              "ssfiltre_rp",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot", "ssfiltre",     "mai_prixtot", 133L,
              "ssfiltre_rp", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt", "ssfiltre",        "proj_mtt", 134L,
              "ssfiltre_rp",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot", "ssfiltre", "terrass_surftot", 135L,
             "ssfiltre_moe",                                   "eptb - Code de la région",             "reg", "ssfiltre",          "reg_cd", 136L,
             "ssfiltre_moe",                                "eptb - Libellé de la région",         "reg_lib", "ssfiltre",         "reg_lib", 137L,
             "ssfiltre_moe",                                               "eptb - Année",           "annee", "ssfiltre",           "annee", 138L,
             "ssfiltre_moe",                                    "eptb - MOE constructeur",             "moe", "ssfiltre",       "indic_cat", 139L,
             "ssfiltre_moe",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp", "ssfiltre",        "nb_obsnp", 140L,
             "ssfiltre_moe",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb", "ssfiltre",          "mai_nb", 141L,
             "ssfiltre_moe",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot", "ssfiltre",     "mai_surftot", 142L,
             "ssfiltre_moe",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot", "ssfiltre",     "mai_prixtot", 143L,
             "ssfiltre_moe", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt", "ssfiltre",        "proj_mtt", 144L,
             "ssfiltre_moe",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot", "ssfiltre", "terrass_surftot", 145L,
               "filtre_csp",                                   "eptb - Code de la région",             "reg",   "filtre",          "reg_cd", 146L,
               "filtre_csp",                                "eptb - Libellé de la région",         "reg_lib",   "filtre",         "reg_lib", 147L,
               "filtre_csp",                                               "eptb - Année",           "annee",   "filtre",           "annee", 148L,
               "filtre_csp",                               "eptb - CSP du chef de ménage",             "csp",   "filtre",       "indic_cat", 149L,
               "filtre_csp",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp",   "filtre",        "nb_obsnp", 150L,
               "filtre_csp",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb",   "filtre",          "mai_nb", 151L,
               "filtre_csp",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot",   "filtre",     "mai_surftot", 152L,
               "filtre_csp",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot",   "filtre",     "mai_prixtot", 153L,
               "filtre_csp",                 "eptb - Nombre de terrains (achetés ou non)",         "terr_nb",   "filtre",         "terr_nb", 154L,
               "filtre_csp",                  "eptb - Prix total des terrains (en euros)",    "terr_prixtot",   "filtre",    "terr_prixtot", 155L,
               "filtre_csp", "eptb - Superficie totale des terrains achetés (en m\u00b2)",    "terr_surftot",   "filtre",    "terr_surftot", 156L,
               "filtre_csp",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot",   "filtre", "terrass_surftot", 157L,
               "filtre_csp", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt",   "filtre",        "proj_mtt", 158L,
               "filtre_age",                                   "eptb - Code de la région",             "reg",   "filtre",          "reg_cd", 159L,
               "filtre_age",                                "eptb - Libellé de la région",         "reg_lib",   "filtre",         "reg_lib", 160L,
               "filtre_age",                                               "eptb - Année",           "annee",   "filtre",           "annee", 161L,
               "filtre_age",                 "eptb - Classe d'\u00e2ge du chef de ménage",             "age",   "filtre",       "indic_cat", 162L,
               "filtre_age",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp",   "filtre",        "nb_obsnp", 163L,
               "filtre_age",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb",   "filtre",          "mai_nb", 164L,
               "filtre_age",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot",   "filtre",     "mai_surftot", 165L,
               "filtre_age",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot",   "filtre",     "mai_prixtot", 166L,
               "filtre_age",                 "eptb - Nombre de terrains (achetés ou non)",         "terr_nb",   "filtre",         "terr_nb", 167L,
               "filtre_age",                  "eptb - Prix total des terrains (en euros)",    "terr_prixtot",   "filtre",    "terr_prixtot", 168L,
               "filtre_age", "eptb - Superficie totale des terrains achetés (en m\u00b2)",    "terr_surftot",   "filtre",    "terr_surftot", 169L,
               "filtre_age",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot",   "filtre", "terrass_surftot", 170L,
               "filtre_age", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt",   "filtre",        "proj_mtt", 171L,
     "filtre_viabilisation",                                   "eptb - Code de la région",             "reg",   "filtre",          "reg_cd", 172L,
     "filtre_viabilisation",                                "eptb - Libellé de la région",         "reg_lib",   "filtre",         "reg_lib", 173L,
     "filtre_viabilisation",                                               "eptb - Année",           "annee",   "filtre",           "annee", 174L,
     "filtre_viabilisation",                            "eptb - Viabilisation du terrain",   "viabilisation",   "filtre",       "indic_cat", 175L,
     "filtre_viabilisation",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp",   "filtre",        "nb_obsnp", 176L,
     "filtre_viabilisation",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb",   "filtre",          "mai_nb", 177L,
     "filtre_viabilisation",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot",   "filtre",     "mai_surftot", 178L,
     "filtre_viabilisation",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot",   "filtre",     "mai_prixtot", 179L,
     "filtre_viabilisation",                 "eptb - Nombre de terrains (achetés ou non)",         "terr_nb",   "filtre",         "terr_nb", 180L,
     "filtre_viabilisation",                  "eptb - Prix total des terrains (en euros)",    "terr_prixtot",   "filtre",    "terr_prixtot", 181L,
     "filtre_viabilisation", "eptb - Superficie totale des terrains achetés (en m\u00b2)",    "terr_surftot",   "filtre",    "terr_surftot", 182L,
     "filtre_viabilisation",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot",   "filtre", "terrass_surftot", 183L,
     "filtre_viabilisation", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt",   "filtre",        "proj_mtt", 184L,
                "filtre_rp",                                   "eptb - Code de la région",             "reg",   "filtre",          "reg_cd", 185L,
                "filtre_rp",                                "eptb - Libellé de la région",         "reg_lib",   "filtre",         "reg_lib", 186L,
                "filtre_rp",                                               "eptb - Année",           "annee",   "filtre",           "annee", 187L,
                "filtre_rp",                                "eptb - Résidence principale",              "rp",   "filtre",       "indic_cat", 188L,
                "filtre_rp",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp",   "filtre",        "nb_obsnp", 189L,
                "filtre_rp",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb",   "filtre",          "mai_nb", 190L,
                "filtre_rp",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot",   "filtre",     "mai_surftot", 191L,
                "filtre_rp",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot",   "filtre",     "mai_prixtot", 192L,
                "filtre_rp",                 "eptb - Nombre de terrains (achetés ou non)",         "terr_nb",   "filtre",         "terr_nb", 193L,
                "filtre_rp",                  "eptb - Prix total des terrains (en euros)",    "terr_prixtot",   "filtre",    "terr_prixtot", 194L,
                "filtre_rp", "eptb - Superficie totale des terrains achetés (en m\u00b2)",    "terr_surftot",   "filtre",    "terr_surftot", 195L,
                "filtre_rp",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot",   "filtre", "terrass_surftot", 196L,
                "filtre_rp", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt",   "filtre",        "proj_mtt", 197L,
         "filtre_achatterr",                                   "eptb - Code de la région",             "reg",   "filtre",          "reg_cd", 198L,
         "filtre_achatterr",                                "eptb - Libellé de la région",         "reg_lib",   "filtre",         "reg_lib", 199L,
         "filtre_achatterr",                                               "eptb - Année",           "annee",   "filtre",           "annee", 200L,
         "filtre_achatterr",                               "eptb - Achat du terrain en N",     "terr_achatn",   "filtre",     "terr_achatn", 201L,
         "filtre_achatterr",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp",   "filtre",        "nb_obsnp", 202L,
         "filtre_achatterr",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb",   "filtre",          "mai_nb", 203L,
         "filtre_achatterr",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot",   "filtre",     "mai_surftot", 204L,
         "filtre_achatterr",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot",   "filtre",     "mai_prixtot", 205L,
         "filtre_achatterr",                 "eptb - Nombre de terrains (achetés ou non)",         "terr_nb",   "filtre",         "terr_nb", 206L,
         "filtre_achatterr",                  "eptb - Prix total des terrains (en euros)",    "terr_prixtot",   "filtre",    "terr_prixtot", 207L,
         "filtre_achatterr", "eptb - Superficie totale des terrains achetés (en m\u00b2)",    "terr_surftot",   "filtre",    "terr_surftot", 208L,
         "filtre_achatterr",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot",   "filtre", "terrass_surftot", 209L,
         "filtre_achatterr", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt",   "filtre",        "proj_mtt", 210L,
       "ssfiltre_chauffage",                                   "eptb - Code de la région",             "reg", "ssfiltre",          "reg_cd", 211L,
       "ssfiltre_chauffage",                                "eptb - Libellé de la région",         "reg_lib", "ssfiltre",         "reg_lib", 212L,
       "ssfiltre_chauffage",                                               "eptb - Année",           "annee", "ssfiltre",           "annee", 213L,
       "ssfiltre_chauffage",                                   "eptb - Mode de chauffage",       "chauffage", "ssfiltre",       "indic_cat", 214L,
       "ssfiltre_chauffage",                 "eptb - Nombre d’observations (non pondéré)",        "nb_obsnp", "ssfiltre",        "nb_obsnp", 215L,
       "ssfiltre_chauffage",           "eptb - Nombre de maisons (terrain acheté ou non)",          "mai_nb", "ssfiltre",          "mai_nb", 216L,
       "ssfiltre_chauffage",                  "eptb - Surface totale des maisons (en m2)",     "mai_surftot", "ssfiltre",     "mai_surftot", 217L,
       "ssfiltre_chauffage",                   "eptb - Prix total des maisons (en euros)",     "mai_prixtot", "ssfiltre",     "mai_prixtot", 218L,
       "ssfiltre_chauffage", "eptb - Co\u00fbt total des projets (maison et achat du terrain)",        "proj_mtt", "ssfiltre",        "proj_mtt", 219L,
       "ssfiltre_chauffage",     "eptb - Superficie totale des terrains d'assise (en m2)", "terrass_surftot", "ssfiltre", "terrass_surftot", 220L
     )
  ls_import[["T_onglets_champs"]] <- T_onglets_champs

  if (all(chps0_ls1$champs0 %in% T_onglets_champs$champs0) == FALSE) {
    liste_onglets_pb <-
      chps0_ls1$sheet[which(!chps0_ls1$champs0 %in% T_onglets_champs$champs0)] %>%
      unique()
  }

  T_onglets_usage <-tibble::tribble(
                    ~onglet,  ~ech_adm,   ~indic,      ~champ_sup,
        "achat_terrain_dep", "dep_cor", "compar",              NA,
        "achat_terrain_reg",     "reg", "compar",              NA,
         "filtre_achatterr", "reg_cor",   "typo",     "achatterr",
               "filtre_age", "reg_cor",   "typo",           "age",
               "filtre_csp", "reg_cor",   "typo",           "csp",
               "filtre_dep", "reg_cor", "compar",              NA,
                "filtre_fr",      "fr", "compar",              NA,
               "filtre_reg", "reg_cor", "compar",              NA,
                "filtre_rp", "reg_cor",   "typo",            "rp",
     "filtre_viabilisation", "reg_cor",   "typo", "viabilisation",
       "ssfiltre_achatterr", "reg_cor",   "typo",     "achatterr",
             "ssfiltre_age", "reg_cor",   "typo",           "age",
       "ssfiltre_chauffage", "reg_cor",   "typo",     "chauffage",
             "ssfiltre_csp", "reg_cor",   "typo",           "csp",
             "ssfiltre_dep", "dep_cor", "compar",              NA,
        "ssfiltre_finition", "reg_cor",   "typo",      "finition",
              "ssfiltre_fr",      "fr", "compar",              NA,
             "ssfiltre_moe", "reg_cor",   "typo",           "moe",
             "ssfiltre_reg",     "reg", "compar",              NA,
              "ssfiltre_rp", "reg_cor",   "typo",            "rp"
     )
  ls_import[["T_onglets_usage"]] <- T_onglets_usage

  terrains_champs_1 <- c(
    "annee",
    "indic",
    "indic_cat",
    "part",
    "terr_prixmoy_m2",
    "terr_surfmoy_m2",
    "terr_prixmoy",
    "part_prixterrain",
    "proj_mttmoy"
  )
  ls_import[["terrains_champs_1"]] <- terrains_champs_1
  terrains_champs_2 <- c("annee",
                         "nb_obsnp",
                         "terr_nb",
                         "terr_surftot",
                         "terr_prixtot",
                         "proj_mtt")
  ls_import[["terrains_champs_2"]] <- terrains_champs_2

  # Maisons
  maisons_champs_1 <- c(
    "annee",
    "indic",
    "indic_cat",
    "part",
    "mai_prixmoy_m2",
    "mai_surfmoy_m2",
    "mai_prixmoy"
  )
  ls_import[["maisons_champs_1"]] <- maisons_champs_1



  # C1 Terrains données générales : les départements, regions et France entière  ---------------------

  fich_frregdep_filtre <- c("filtre_fr", "filtre_reg", "filtre_dep")
  ls_import[["fich_frregdep_filtre"]] <- fich_frregdep_filtre


  # D1 les maisons données générales : les départements et régions----------

  fich_frregdep_ssfiltre <-
    c("ssfiltre_fr", "ssfiltre_reg", "ssfiltre_dep")
  ls_import[["fich_frregdep_ssfiltre"]] <- fich_frregdep_ssfiltre


  return(ls_import)


  } else{
    print("Copier le fichier EPTB_aggrege.xlsx dans 2_data annee")
  }
}
