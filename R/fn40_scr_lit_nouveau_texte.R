#' Scribus : texte, lit le texte de remplacement
#'
#' @param x annee etude en caratere
#'
#' @return liste
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom purrr map
#' @importFrom purrr map_if
#' @importFrom stringr str_glue
#' @importFrom utf8 as_utf8
#' @export
#'
fn40_scr_lit_nouveau_texte <- function(x = ls_dates$annee_etude) {
  # Liste des indicateurs utilisables : -------

  list(
    annee_etude = resultats[["annee_etude"]],

    annee_precedente = resultats[["annee_precedente"]],

    # prix des terrains

    # terrains prix  m2 an N

    pt_m2_an = tab_calculs[["terrains_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "prix_m2") %>%
      dplyr::pull(value),

    # terrains prix  m2 an N-1
    #
    pt_m2_an_prec = tab_calculs[["terrains_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "prix_m2") %>%
      dplyr::pull(value0),

    # terrains prix  m2 an N Q1
    #
    pt_m2_an_q1 = tab_calculs[["terrains_autres_reg"]] %>%
      dplyr::filter(reg_lib %in% "Corse") %>% dplyr::pull(prix_m2q1),

    # terrains prix  m2 an N m2 Q2
    #
    pt_m2_an_q2 = tab_calculs[["terrains_autres_reg"]] %>%
      dplyr::filter(reg_lib %in% "Corse") %>% dplyr::pull(prix_m2q2),

    # terrains prix  m2 an N m2 Q3
    #
    pt_m2_an_q3 = tab_calculs[["terrains_autres_reg"]] %>%
      dplyr::filter(reg_lib %in% "Corse") %>% dplyr::pull(prix_m2q3),

    # terrains difference prix_m2 departements corses et france
    # 2A
    pt_an_m2_diff_fr_dpt2a <-
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(indic %in% "prix_m2", territoire %in% "Corse-du-Sud") %>%
      dplyr::pull(dt_dep_cor),
    # 2B
    pt_an_m2_diff_fr_dpt2b <-
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(indic %in% "prix_m2", territoire %in% "Haute-Corse") %>%
      dplyr::pull(dt_dep_cor),

    # terrains difference prix total departements corses et france
    # 2A
    pt_an_diff_fr_dpt2a <-
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(indic %in% "prix", territoire %in% "Corse-du-Sud") %>%
      dplyr::pull(dt_dep_cor),
    # 2B
    pt_an_diff_fr_dpt2b <-
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(indic %in% "prix", territoire %in% "Haute-Corse") %>%
      dplyr::pull(dt_dep_cor),

    # Projet difference prix departements corses et france
    # 2A
    pt_an_pjt_diff_cor_dpt2a <-
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(indic %in% "cout_projet", territoire %in% "Corse-du-Sud") %>%
      dplyr::pull(dt_dep_cor),
    # 2B
    pt_an_pjt_diff_cor_dpt2b <-
      tab_calculs[["terrains_depcor_compare_fr"]] %>%
      dplyr::filter(indic %in% "cout_projet", territoire %in% "Haute-Corse") %>%
      dplyr::pull(dt_dep_cor),

    # difference prix projet 2A et 2B

    pt_an_pjt_diff_dpt2a_dpt2b <-
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
      ) %>% dplyr::pull(td_2a_2b),

    # Terrains prix evolutions sur 1 an et depuis 2010

    # terrains tx diff prix m2 an N an N-1
    #
    pt_m2_evol_1an = tab_calculs[["terrains_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "prix_m2") %>%
      dplyr::pull(taux),

    # terrains tx diff prix m2 an N an 2010
    #
    pt_m2_evol_dep2010 = tab_calculs[["terrains_depuis2010"]] %>%
      dplyr::filter(
        territoire %in% "Corse",
        annee %in% resultats[["annee_etude"]] ,
        indic %in% "prix_m2"
      ) %>%
      dplyr::pull(taux) %>% round(., 1),

    # Terrains surfaces

    # terrains surface m2 an N
    #
    st_m2_an = tab_calculs[["terrains_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "surf_m2") %>%
      dplyr::pull(value),

    # terrains surface m2 an N-1
    #
    st_m2_an_prec = tab_calculs[["terrains_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "surf_m2") %>%
      dplyr::pull(value0),

    # terrains surface m2 taux diff an N an N-1
    #
    st_m2_evol_1an = tab_calculs[["terrains_an"]] %>%
      dplyr::filter(territoire %in% "Corse", indic %in% "surf_m2") %>%
      dplyr::pull(taux) %>% round(., 1),

    # terrains surface m2 taux diff an N an 2010
    #
    st_m2_evol_dep2010 = tab_calculs[["terrains_depuis2010"]] %>%
      dplyr::filter(
        territoire %in% "Corse",
        annee %in% resultats[["annee_etude"]],
        indic %in% "surf_m2"
      ) %>%
      dplyr::pull(taux) %>% round(., 1),

    # Maisons surface

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

    # Maisons prix au m2
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
      ) %>% dplyr::pull(taux),

    # pm_m2_evol_1an = tab_calculs[["maisons_depuis2010"]] %>%
    #   dplyr::filter(
    #     territoire %in% "Corse",
    #     annee %in% resultats[["annee_etude"]],
    #     indic %in% "prix_m2"
    #   ) %>% dplyr::pull(taux),


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
      dplyr::filter(
        territoire %in% "Corse",
        annee %in% resultats[["annee_etude"]],
        indic %in% "prix"
      ) %>% dplyr::pull(value0)

  ) -> ls_valeurs


  purrr::map_if(.x = ls_valeurs,
                .p = is.numeric,
                ~ format(.x, big.mark   = ".", decimal.mark = ",")) -> ls_valeurs

  # texte de remplacement : -------

  list(
    # texte résumé
    "p1_bloc2_resume" = c(
      stringr::str_glue(
        "En Corse, en {ls_valeurs[['annee_etude']]}, le prix du mètre carré de terrain a augmenté/diminué de {ls_valeurs[['st_m2_evol_1an']]} % par rapport à l\'année précédente et s\'établit en moyenne à {ls_valeurs[['pt_m2_an']]} euros."
      ),
      stringr::str_glue(
        "Pour la construction des maisons, le prix au mètre carré reste le plus important de France avec {ls_valeurs[['pm_m2_an']]} euros ({ls_valeurs[['st_m2_evol_dep2010']]} % par rapport à {ls_valeurs[['annee_precedente']]})."
      )
    ),

    # premier paragraphe
    "p1_bloc6_texte" = c(
      stringr::str_glue(
        "En {ls_valeurs[['annee_etude']]}, le mètre carré de terrain s'est vendu en moyenne à {ls_valeurs[['pt_m2_an']]} euros soit une hausse annuelle de {ls_valeurs[['pt_m2_evol_1an']]} % et de {ls_valeurs[['pt_m2_evol_dep2010']]} % depuis 2010."
      ),
      stringr::str_glue(
        "La Corse se situe au {ls_valeurs[['annee_etude']]} rang parmi les 13 régions métropolitaines."
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
        "Par rapport au prix moyen national, celui de la Haute-Corse est plus faible de xx% alors que la surface moyenne des terrains est plus forte alors que pour la Corse-du-Sud il est plus élevé (+xx%) mais cela est dû à la grande taille des terrains achetés."
      )  ,
      "La part du montant de l'achat du terrain dans le cout total du projet suit la m\u00eame tendance. Si le taux pour la Corse est proche de celui France entière, il y a de fortes disparités départementales, dues à la taille des terrains et au prix moyen au mètre carré. ",
      stringr::str_glue(
        "Le montant moyen d'un projet est de {ls_valeurs[['pt_an_pjt_diff_dpt2a_dpt2b']]} plus important en Corse-du-Sud qu'en Haute-Corse. Les montants moyens sont respectivement supérieurs de xx et xx% au montant moyen de la France."
      )
    ),

    # second paragraphe
    "p1_bloc8_texte" = c(
      stringr::str_glue(
        "Si la valeur moyenne des surfaces de plancher, pour les maisons construites ici ({ls_valeurs[['sm_m2_an']]} m\u00b2), placent la Corse dans la moyenne des régions, leur prix moyen au m\u00b2 ({ls_valeurs[['pm_m2_an']]}€/m\u00b2)est le plus important de France. Leur prix moyen ({ls_valeurs[['pm_tot_an']]}€) place aussi l’île en xx position des régions les plus chères derrière xxx et devant la xxx et la xxx."
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

    # 3eme paragraphe
    "p2_bloc3_texte" = c(
      "La classe d'\u00e2ge des moins de 30 ans ach\u00eate en moyenne les terrains les moins chers mais privilégie quand m\u00eame une surface proche de la moyenne régionale. Du fait que la part de primo-accédant est particulièrement forte dans cette tranche d'age, le coût total du projet (maison+terrain) est aussi le moins important."   ,
      "Comme attendu, plus le propriétaire est \u00e2gé, plus la superficie du terrain acheté et le coût du projet total est élevé (xx\\% par rapport à la moyenne régionale)."  ,
      "Selon la catégorie socio-professionnelle de l'acheteur, le prix moyen du terrain varie de xx à xx euros du mètre carré, les prix les plus élevés concernent les retraités les artisans, commerçants, chefs d'entreprise et les cadres et professions intellectuelles supérieures. Cette dernière catégorie ach\u00eate aussi en moyenne les terrains les plus grands et chers.  ",
      "Les projet concernent très majoritairement la construction de résidences principales.",
      "L'achat des terrains non viabilisés porte sur des parcelles de plus grande taille (+xx%) avec un coût de projet plus important (+xx%). Leur prix au mètre carré est normalement plus faible (-xx%)."
    ),

    # 4eme paragraphe
    "p3_bloc2_texte" = c(
      "La classe d'\u00e2ge des moins de 30 ans ach\u00eate en moyenne les terrains les moins chers mais privilégie quand m\u00eame une surface proche de la moyenne régionale. Du fait que la part de primo-accédant est particulièrement forte dans cette tranche d'age, le coût total du projet (maison+terrain) est aussi le moins important."   ,
      "Comme attendu, plus le propriétaire est \u00e2gé, plus la superficie du terrain acheté et le coût du projet total est élevé (xx\\% par rapport à la moyenne régionale)."  ,
      "Selon la catégorie socio-professionnelle de l'acheteur, le prix moyen du terrain varie de xx à xx euros du mètre carré, les prix les plus élevés concernent les retraités les artisans, commerçants, chefs d'entreprise et les cadres et professions intellectuelles supérieures. Cette dernière catégorie ach\u00eate aussi en moyenne les terrains les plus grands et chers.  ",
      "Les projet concernent très majoritairement la construction de résidences principales.",
      "L'achat des terrains non viabilisés porte sur des parcelles de plus grande taille (+xx \\%) avec un coût de projet plus important (+xx\\%). Leur prix au mètre carré est normalement plus faible (-xx\\%)."
    )
  ) -> mon_texte
  # purrr::map(mon_texte, utf8::utf8_valid)
  purrr::map(mon_texte, utf8::as_utf8) -> mon_texte
  # purrr::map(mon_texte, validUTF8)
  purrr::map_dfr(mon_texte, ~ tibble::tibble("nblt_r" = length(.x)), .id = "par") -> eff
  list("eff" = eff, "mon_texte" = mon_texte) -> ls_newtxt
  return(ls_newtxt)
}
