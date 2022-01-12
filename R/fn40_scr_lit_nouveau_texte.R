#' Scribus : texte, lit le texte de remplacement
#'
#' @param x annee etude en caratere
#'
#' @return liste
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom purrr map
#' @importFrom purrr map_dfr
#' @importFrom purrr map_if
#' @importFrom scales label_percent
#' @importFrom stats complete.cases
#' @importFrom stringr str_c
#' @importFrom stringr str_glue
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr separate
#' @importFrom utf8 as_utf8
#' @export
#'
fn40_scr_lit_nouveau_texte <- function(x = ls_dates$annee_etude) {
  # Liste des indicateurs utilisables : -------

  list(
    annee_etude = resultats[["annee_etude"]],

    annee_precedente = resultats[["annee_precedente"]],

    # PROJETS

    # Projets difference prix departements corses
    # 2A
    cp_an_pjt_diff_cor_dpt2a =
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(indic %in% "cout_projet", territoire %in% "Corse-du-Sud") %>%
      dplyr::pull(dt_dep_cor) %>% scales::label_percent()(.),
    # 2B
    cp_an_pjt_diff_cor_dpt2b =
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(indic %in% "cout_projet", territoire %in% "Haute-Corse") %>%
      dplyr::pull(dt_dep_cor) %>% scales::label_percent()(.),

    # Projets difference prix 2A et 2B

    cp_an_pjt_diff_dpt2a_dpt2b =
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(
        indic %in% "cout_projet",
        territoire %in% c("Corse-du-Sud", "Haute-Corse")
      ) %>%
      dplyr::select(indic, territoire, value) %>%
      tidyr::pivot_wider(names_from = territoire, values_from = value) %>%
      dplyr::mutate(
        d_2a_2b = `Corse-du-Sud` - `Haute-Corse`,
        td_2a_2b = d_2a_2b / `Haute-Corse`
      ) %>% dplyr::pull(td_2a_2b) %>% scales::label_percent()(.),


    # Projets difference prix departements corses et France
    # 2A
    cp_an_pjt_diff_fr_dpt2a =
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(indic %in% "cout_projet", territoire %in% "Corse-du-Sud") %>%
      dplyr::pull(dt_dep_fr) %>%
      scales::label_percent(suffix = "%", decimal.mark = ",")(.),
    # 2B
    cp_an_pjt_diff_fr_dpt2b =
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(indic %in% "cout_projet", territoire %in% "Haute-Corse") %>%
      dplyr::pull(dt_dep_fr) %>%
      scales::label_percent(suffix = "%", decimal.mark = ",")(.),

    # Projets viabilisation
    #
    cp_an_viabilisation_ouinon = tab_calculs[["terrains_themes"]] %>%
      dplyr::filter(annee %in% resultats[["annee_etude"]],
                    indic %in% "viabilisation") %>%
      dplyr::pull(cout_projet),




    # TERRAINS PRIX

    # Terrains prix  m2 an N

    pt_m2_an = tab_calculs[["terrains_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "prix_m2") %>%
      dplyr::pull(value),

    # Terrains prix  m2 an N-1
    #
    pt_m2_an_prec = tab_calculs[["terrains_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "prix_m2") %>%
      dplyr::pull(value0),

    # Terrains prix  m2 an N Q1
    #
    pt_m2_an_q1 = tab_calculs[["terrains_autres_reg"]] %>%
      dplyr::filter(reg_lib %in% "Corse") %>% dplyr::pull(prix_m2q1),

    # Terrains prix  m2 an N m2 Q2
    #
    pt_m2_an_q2 = tab_calculs[["terrains_autres_reg"]] %>%
      dplyr::filter(reg_lib %in% "Corse") %>% dplyr::pull(prix_m2q2),

    # Terrains prix  m2 an N m2 Q3
    #
    pt_m2_an_q3 = tab_calculs[["terrains_autres_reg"]] %>%
      dplyr::filter(reg_lib %in% "Corse") %>% dplyr::pull(prix_m2q3),


    # Terrains difference prix_m2 departements corses et corse

    # 2A
    pt_an_m2_diff_fr_dpt2a <-
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(indic %in% "prix_m2", territoire %in% "Corse-du-Sud") %>%
      dplyr::pull(dt_dep_cor) %>%
      scales::label_percent(suffix = "%", decimal.mark = ",")(.),

    # 2B
    pt_an_m2_diff_fr_dpt2b <-
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(indic %in% "prix_m2", territoire %in% "Haute-Corse") %>%
      dplyr::pull(dt_dep_cor) %>%
      scales::label_percent(suffix = "%", decimal.mark = ",")(.),


    # Terrains difference prix total departements corses et france

    # 2A
    pt_an_diff_fr_dpt2a <-
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(indic %in% "prix", territoire %in% "Corse-du-Sud") %>%
      dplyr::pull(dt_dep_cor) %>%
      scales::label_percent(suffix = "%", decimal.mark = ",")(.),

    # 2B
    pt_an_diff_fr_dpt2b <-
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(indic %in% "prix", territoire %in% "Haute-Corse") %>%
      dplyr::pull(dt_dep_cor) %>%
      scales::label_percent(suffix = "%", decimal.mark = ",")(.),

    # Terrains prix m2 classement corse
    #
    pt_m2_an_q2 = tab_calculs[["terrains_autres_reg_clt"]] %>%
      dplyr::filter(reg_lib %in% "Corse") %>% dplyr::pull(prix_m2),


    # Terrains prix evolutions sur 1 an et depuis 2010

    # Terrains tx diff prix m2 an N an N-1
    #
    pt_m2_an_corse_clt = tab_calculs[["terrains_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "prix_m2") %>%
      dplyr::pull(taux),

    # Terrains tx diff prix m2 an N an 2010
    #
    pt_m2_evol_dep2010 = tab_calculs[["terrains_depuis2010"]] %>%
      dplyr::filter(
        territoire %in% "Corse",
        annee %in% resultats[["annee_etude"]] ,
        indic %in% "prix_m2"
      ) %>%
      dplyr::pull(taux) %>% round(., 1),



    # Terrains prix m2 an N CSP
    #
    pt_m2_an_csp_range = tab_calculs[["terrains_themes"]] %>%
      dplyr::filter(annee %in% resultats[["annee_etude"]],
                    indic %in% "csp") %>%
      dplyr::pull(prix_m2) %>% na.omit() %>% range(),

    # Terrains prix total an N CSP
    #
    pt_an_pluscher_csp = tab_calculs[["terrains_themes"]] %>%
      dplyr::filter(annee %in% resultats[["annee_etude"]],
                    indic %in% "csp") %>%
      dplyr::arrange(desc(prix)) %>%
      dplyr::slice(1:3) %>%
      tidyr::separate(
        col = "indic_cat",
        into = c("eff", "indic_cat"),
        sep = ":"
      ) %>%
      dplyr::pull(indic_cat) %>%  tolower() %>%
      stringr::str_c(., collapse = " ,"),

    # Terrains prix m2 an viabilisation
    #
    pt_an_m2_viabilisation_ouinon = tab_calculs[["terrains_themes"]] %>%
      dplyr::filter(annee %in% resultats[["annee_etude"]],
                    indic %in% "viabilisation") %>%
      dplyr::pull(prix_m2),




    # TERRAINS SURFACE

    # Terrains surface m2 an N
    #
    st_m2_an = tab_calculs[["terrains_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "surf_m2") %>%
      dplyr::pull(value),

    # Terrains surface m2 an N-1
    #
    st_m2_an_prec = tab_calculs[["terrains_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "surf_m2") %>%
      dplyr::pull(value0),

    # Terrains surface m2 taux diff an N an N-1
    #
    st_m2_evol_1an = tab_calculs[["terrains_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "surf_m2") %>%
      dplyr::pull(taux) %>% round(., 1),

    # Terrains surface m2 taux diff an N an 2010
    #
    st_m2_evol_dep2010 = tab_calculs[["terrains_depuis2010"]] %>%
      dplyr::filter(
        territoire %in% "Corse",
        annee %in% resultats[["annee_etude"]],
        indic %in% "surf_m2"
      ) %>%
      dplyr::pull(taux) %>% round(., 1),

    # Terrains surface m2 diff viabilisation
    #
    st_m2_an_viabilisation_ouinon = tab_calculs[["terrains_themes"]] %>%
      dplyr::filter(annee %in% resultats[["annee_etude"]],
                    indic %in% "viabilisation") %>%
      dplyr::pull(surf_m2),





    # MAISONS SURFACE

    # Maisons surface m2 an N
    #
    sm_m2_an = tab_calculs[["maisons_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "surf_m2") %>%
      dplyr::pull(value),

    # Maisons surface m2 an N-1
    #
    sm_m2_an_prec = tab_calculs[["maisons_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "surf_m2") %>%
      dplyr::pull(value0),

    # Maisons surface m2 an 2010
    #
    sm_m2_2010 = tab_calculs[["maisons_depuis2010"]] %>%
      dplyr::filter(
        territoire %in% "Corse",
        annee %in% resultats[["annee_etude"]],
        indic %in% "surf_m2"
      ) %>% dplyr::pull(value0),

    # Maisons surface m2 an moe construit particulier
    #
    sm_m2_an_moe_parti = tab_calculs[["maisons_themes"]] %>%
      dplyr::filter(
        annee %in% resultats[["annee_etude"]],
        indic %in% "moe",
        stats::complete.cases(prix_m2),
        stringr::str_sub(indic_cat, 1, 2) %in% "04"
      ) %>% dplyr::pull(surf_m2),

    # Maisons surface m2 an moe construit constructeur maisons individuelles
    #
    sm_m2_an_moe_cmi = tab_calculs[["maisons_themes"]] %>%
      dplyr::filter(
        annee %in% resultats[["annee_etude"]],
        indic %in% "moe",
        stats::complete.cases(prix_m2),
        stringr::str_sub(indic_cat, 1, 2) %in% "02"
      ) %>% dplyr::pull(surf_m2),




    # MAISONS PRIX
    #

    # Maisons prix au m2 an N
    #
    pm_m2_an = tab_calculs[["maisons_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "prix_m2") %>%
      dplyr::pull(value),

    # Maisons prix au m2 an N-1
    #
    pm_m2_an_prec = tab_calculs[["maisons_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "prix_m2") %>%
      dplyr::pull(value0),

    # Maisons prix au m2 an 2010
    #
    pm_m2_2010 = tab_calculs[["maisons_depuis2010"]] %>%
      dplyr::filter(
        territoire %in% "Corse",
        annee %in% resultats[["annee_etude"]],
        indic %in% "prix_m2"
      ) %>% dplyr::pull(value0),

    # Maisons prix au m2  taux diff an N an 2010
    #
    pm_m2_evol_1an = tab_calculs[["maisons_depuis2010"]] %>%
      dplyr::filter(
        territoire %in% "Corse",
        annee %in% resultats[["annee_etude"]],
        indic %in% "prix_m2"
      ) %>% dplyr::pull(taux) %>% round(., 1),

    # Maisons prix au m2  moe prix_m2 max et min
    #
    pm_m2_an_moe_max = tab_calculs[["maisons_themes"]] %>%
      dplyr::filter(annee %in% resultats[["annee_etude"]],
                    indic %in% "moe",
                    stats::complete.cases(prix_m2)) %>%
      dplyr::select(indic_cat, prix_m2) %>%
      tidyr::separate(
        col = "indic_cat",
        into = c("eff", "indic_cat"),
        sep = ":"
      ) %>%
      dplyr::mutate(indic_cat = stringr::str_trim(indic_cat) %>% tolower()) %>%
      dplyr::filter(prix_m2 == max(prix_m2)) %>% dplyr::select(-eff),

    pm_m2_an_moe_min = tab_calculs[["maisons_themes"]] %>%
      dplyr::filter(annee %in% resultats[["annee_etude"]],
                    indic %in% "moe",
                    stats::complete.cases(prix_m2)) %>%
      dplyr::select(indic_cat, prix_m2) %>%
      tidyr::separate(
        col = "indic_cat",
        into = c("eff", "indic_cat"),
        sep = ":"
      ) %>%
      dplyr::mutate(indic_cat = stringr::str_trim(indic_cat) %>% tolower()) %>%
      dplyr::filter(prix_m2 == min(prix_m2)) %>% dplyr::select(-eff),



    # Maisons prix total
    #

    # Maisons prix total an N
    #
    pm_tot_an = tab_calculs[["maisons_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "prix") %>%
      dplyr::pull(value),

    # prix total des maisons an N-1
    #
    pm_tot_an_prec = tab_calculs[["maisons_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "prix") %>%
      dplyr::pull(value0),

    # prix total des maisons an 2010
    #
    pm_tot_2010 = tab_calculs[["maisons_depuis2010"]] %>%
      dplyr::filter(territoire %in% "Corse",
                    annee %in% resultats[["annee_etude"]],
                    indic %in% "prix") %>% dplyr::pull(value0),

    # prix total des maisons an N classement
    #
    pm_tot_an_regions_clt = tab_calculs[["maisons_autres_reg_clt"]] %>%
      dplyr::filter(reg_lib %in% "Corse",
                    annee %in% resultats[["annee_etude"]]) %>%
      dplyr::pull(prix),



    # MAISONS NOMBRE
    #
    # Maisons nombre mopart construit particulier
    #
    nm_an_moe_part_parti_cor = tab_calculs[["maisons_themes"]] %>%
      dplyr::filter(
        annee %in% resultats[["annee_etude"]],
        indic %in% "moe",
        stringr::str_sub(indic_cat, 1, 2) %in% "04"
      ) %>%
      dplyr::pull(part) %>% scales::label_percent()(.),

    # Maisons nombre moe part construit constructeur maisons individuelles
    #
    nm_an_moe_part_cmi_cor = tab_calculs[["maisons_themes"]] %>%
      dplyr::filter(
        annee %in% resultats[["annee_etude"]],
        indic %in% "moe",
        stringr::str_sub(indic_cat, 1, 2) %in% "02"
      ) %>%
      dplyr::pull(part) %>%
      scales::label_percent(suffix = "%",decimal.mark = ",")(.),

    # Maisons nombre mopart construit entreprise ou artisan
    #
    nm_an_moe_part_eoa_cor = tab_calculs[["maisons_themes"]] %>%
      dplyr::filter(
        annee %in% resultats[["annee_etude"]],
        indic %in% "moe",
        stringr::str_sub(indic_cat, 1, 2) %in% "03"
      ) %>%
      dplyr::pull(part) %>% scales::label_percent()(.),

    # Maisons nombre mopart construit entreprise ou artisan
    #
    nm_an_chaufage_part_minmax =
      purrr::map(
        tab_calculs[["maisons_themes"]] %>%
          dplyr::filter(
            annee %in% resultats[["annee_etude"]],
            indic %in% "chauffage",
            stats::complete.cases(prix_m2)
          ) %>%
          dplyr::mutate(
            indic_cat2 = dplyr::case_when(
              stringr::str_sub(indic_cat, 1, 2) %in% c("04", "05") ~ "les énergies renouvelables seules ou associées à un autre mode de chauffage",
              stringr::str_sub(indic_cat, 1, 2) %in% c("02", "03") ~
                "l'électricité seule ou combinée avec le chauffage au bois d'appoint",
              stringr::str_sub(indic_cat, 1, 2) %in% c("06") ~
                "autres (électricté plus gaz, ...)",
              stringr::str_sub(indic_cat, 1, 2) %in% c("01") ~
                "le gaz"
            )
          ) %>% dplyr::group_by(indic_cat2) %>%
          dplyr::summarise(part = sum(part) %>% scales::label_percent()(.)) %>%
          dplyr::arrange(desc(part)),
        ~ unlist(.x)
      )


  ) -> ls_valeurs


  purrr::map_if(.x = ls_valeurs,
                .p = is.numeric,
                ~ format(.x, big.mark   = ".", decimal.mark = ",")) -> ls_valeurs

  # texte de remplacement : -------

  list(
    # page1 texte résumé
    "p1_bloc2_resume" = c(
      stringr::str_glue(
        "En Corse, en {ls_valeurs[['annee_etude']]}, le prix du mètre carré de terrain a augmenté/diminué de {ls_valeurs[['st_m2_evol_1an']]} % par rapport à l\'année précédente et s\'établit en moyenne à {ls_valeurs[['pt_m2_an']]} euros."
      ),
      stringr::str_glue(
        "Pour la construction des maisons, le prix au mètre carré reste le plus important de France avec {ls_valeurs[['pm_m2_an']]} euros ({ls_valeurs[['st_m2_evol_dep2010']]} % par rapport à {ls_valeurs[['annee_precedente']]})."
      )
    ),

    # page1 premier paragraphe terrains
    "p1_bloc6_texte" = c(
      stringr::str_glue(
        "En {ls_valeurs[['annee_etude']]}, le mètre carré de terrain s'est vendu en moyenne à {ls_valeurs[['pt_m2_an']]} euros soit une hausse annuelle de {ls_valeurs[['pt_m2_evol_1an']]} % et de {ls_valeurs[['pt_m2_evol_dep2010']]} % depuis 2010."
      ),
      stringr::str_glue(
        "La Corse se situe au {ls_valeurs[['pt_m2_an_corse_clt']]} rang parmi les 13 régions métropolitaines."
      ),
      "Le prix moyen au mètre carré des terrains achetés masque des disparités liées à la situation et à la taille de ceux-ci :",
      stringr::str_glue(
        "• 25% des terrains sont payés moins de {ls_valeurs[['pt_m2_an_q1']]} euros du m2 (Q1),"
      ) ,
      stringr::str_glue("• 50% à moins de {ls_valeurs[['pt_m2_an_q2']]} (médiane),") ,
      stringr::str_glue("• 75% à moins de {ls_valeurs[['pt_m2_an_q3']]} (Q3)."),
      stringr::str_glue(
        "La superficie moyenne des terrains achetés dans l\'année est de {ls_valeurs[['st_m2_an']]} m\u00b2, surface en baisse de {ls_valeurs[['st_m2_evol_1an']]} % par rapport à {ls_valeurs[['annee_precedente']]} ({ls_valeurs[['st_m2_evol_dep2010']]} % avec 2010) mais avec des terrains beaucoup plus grands en Corse-du-Sud qu'en Haute Corse. "
      ),
      stringr::str_glue(
        "Par rapport au prix moyen national, ceux des deux départements corses sont plus élevés notamment {round(100 * ls_valeurs[['pt_an_diff_fr_dpt2a']], 1)}% pour la Corse-du-Sud du fait de la grande taille de ceux-ci et du prix moyen au mètre carré et de {round(100 * ls_valeurs[['pt_an_diff_fr_dpt2b']], 1)}% pour la Haute-Corse."
      )  ,
      "La part du montant de l'achat du terrain dans le cout total du projet suit la m\u00eame tendance. Si le taux pour la Corse est proche de celui France entière, il y a de fortes disparités départementales.",
      stringr::str_glue(
        "Le montant moyen d'un projet est de {ls_valeurs[['cp_an_pjt_diff_dpt2a_dpt2b']]} % plus important en Corse-du-Sud qu'en Haute-Corse. Ils sont respectivement supérieurs de {ls_valeurs[['cp_an_pjt_diff_fr_dpt2a']]} % et {ls_valeurs[['cp_an_pjt_diff_fr_dpt2b']]} % au montant moyen pour la France."
      )
    ),

    # page2 premier paragraphe terrains themes
    "p1_bloc8_texte" = c(
      stringr::str_glue(
        "Si la valeur moyenne des surfaces de plancher, pour les maisons construites ici ({ls_valeurs[['sm_m2_an']]} m\u00b2), place la Corse dans la moyenne des régions, leur prix moyen au m\u00b2 ({ls_valeurs[['pm_m2_an']]}€/m\u00b2) est le plus important de France. Leur prix moyen ({ls_valeurs[['pm_tot_an']]}€) place aussi l’île en {ls_valeurs[['pm_tot_an_regions_clt']]} position des régions les plus chères (derrière xxx) et devant les régions Grand-Est et Île-de-France."
      ),
      "Comme pour les terrains, les surfaces et montants moyens des maisons en Corse-du-Sud sont plus importants qu’en Haute-Corse." ,
      stringr::str_glue(
        "Depuis 2010, pour la Corse, la surface moyenne de plancher a évolué en dent de scie mais globalement à la baisse, passant de {ls_valeurs[['sm_m2_2010']]}m\u00b2 à {ls_valeurs[['sm_m2_an']]}m\u00b2."
      ),
      stringr::str_glue(
        "Le prix moyen au mètre carré a fortement augmenté entre {ls_valeurs[['annee_etude']]} et {ls_valeurs[['annee_etude']]} passant de {ls_valeurs[['annee_etude']]} à {ls_valeurs[['annee_etude']]} euros. "
      ),
      stringr::str_glue(
        "Le prix moyen d’achat d’une maison a progressé dans le m\u00eame temps de {ls_valeurs[['pm_tot_2010']]} à {ls_valeurs[['pm_tot_an']]} euros."
      )
    ),

    # page3 premier paragraphe maisons
    "p2_bloc3_texte" = c(
      "La classe d'\u00e2ge des moins de 30 ans ach\u00eate en moyenne les terrains (les moins chers) mais privilégie quand m\u00eame une surface proche de la moyenne régionale. Du fait que la part de primo-accédant est particulièrement forte dans cette tranche d'age, le coût total du projet (maison+terrain) est aussi le moins important."   ,
      stringr::str_glue(
        "Comme attendu, plus le propriétaire est \u00e2gé, plus le prix du terrain acheté et le coût du projet total est élevé (xx\\% par rapport à la moyenne régionale)."
      ),
      stringr::str_glue(
        "Selon la catégorie socio-professionnelle de l'acheteur, le prix moyen du terrain varie de {ls_valeurs[['st_m2_an_csp_range']][1]} à {ls_valeurs[['st_m2_an_csp_range']][2]} euros du mètre carré, les prix les plus élevés concernent les {ls_valeurs[['pt_an_pluscher_csp']]}. "
      ),
      "Les projet concernent très majoritairement la construction de résidences principales.",
      stringr::str_glue(
        "L'achat des terrains non viabilisés porte sur des parcelles de plus grande taille ({ls_valeurs[['st_m2_an_viabilisation_ouinon']][2]} au lieu de {ls_valeurs[['st_m2_an_viabilisation_ouinon']][1]} m\u00b2) avec un coût de projet plus important ({ls_valeurs[['cp_an_viabilisation_ouinon']][2]} au lieu de {ls_valeurs[['cp_an_viabilisation_ouinon']][1]} euros). Leur prix au mètre carré est normalement plus faible ({ls_valeurs[['pt_m2_an_viabilisation_ouinon']][2]} au lieu de {ls_valeurs[['pt_m2_an_viabilisation_ouinon']][1]} m\u00b2)."
      )
    ),

    # 4eme paragraphe maisons themes
    "p3_bloc2_texte" = c(
      stringr::str_glue(
        "En {ls_valeurs[['annee_etude']]}, les dispositifs utilisant {ls_valeurs$nm_an_chaufage_part_minmax$indic_cat2[1]} s'imposent comme le mode de chauffage le plus répandu dans la construction de maisons individuelles : ils sont présents seuls ou combinés entre eux dans {ls_valeurs$nm_an_chaufage_part_minmax$part[1]} des projets)."
      ),
      stringr::str_glue(
        "Ce taux est de XX points supérieur à la moyenne nationale (XX%). L'utilisation des {ls_valeurs$nm_an_chaufage_part_minmax$indic_cat2[2]} est en seconde position {ls_valeurs$nm_an_chaufage_part_minmax$part[2]} % des projets et XX points par rapport à la moyenne nationale)."
      ),
      "La proportion de nouvelles maisons chauffées au gaz reste anecdotique en Corse.",
      stringr::str_glue(
        "Près de la moitié ({ls_valeurs$nm_an_moe_part_parti_cor}) des constructions de maisons sont supervisées par les particuliers eux-m\uc3aame pour un peu plus d'un quart (XX%) au niveau national."
      ),
      stringr::str_glue(
        "L'intervention d'un constructeur de maisons individuelle ne se fait que dans {ls_valeurs$nm_an_moe_part_cmi_cor} des cas, alors qu'au niveau national, ce taux est de XX%, de m\uc3aame pour les entrepreneurs et artisans oû les taux pour la Corse et France entière sont {ls_valeurs$nm_an_moe_part_eoa_cor} contre XX%."
      ),
      stringr::str_glue(
        "Le prix moyen par mètre carré des maisons est de {ls_valeurs$pm_m2_an} euros ; il varie selon le ma\uc3aetre d'oevre entre {ls_valeurs$pm_m2_an_moe_min %>% dplyr::pull(prix_m2)} euros dans le cas des {ls_valeurs$pm_m2_an_moe_min %>% dplyr::pull(indic_cat)} à {ls_valeurs$pm_m2_an_moe_max %>% dplyr::pull(prix_m2)} pour les {ls_valeurs$pm_m2_an_moe_max %>% dplyr::pull(indic_cat)}."
      ),
      stringr::str_glue(
        "La surface moyenne des maisons est moins importante ({ls_valeurs$sm_m2_an_moe_cmi} m\u00b2) dans le cas d'un constructeur de maisons individuelles que lorsque le particulier réalise les travaux {ls_valeurs$sm_m2_an_moe_parti} m\u00b2."
      )
    )
  ) -> mon_texte
  # purrr::map(mon_texte, utf8::utf8_valid)
  purrr::map(mon_texte, utf8::as_utf8) -> mon_texte
  # purrr::map(mon_texte, validUTF8)
  purrr::map_dfr(mon_texte, ~ tibble::tibble("nblt_r" = length(.x)), .id = "par") -> eff
  list("eff" = eff, "mon_texte" = mon_texte) -> ls_newtxt
  return(ls_newtxt)
}
