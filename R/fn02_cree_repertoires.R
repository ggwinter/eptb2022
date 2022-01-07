#' cree les repertoires dans le repertoire de travail.
#' @param new_rep vecteur de caracteres avec le nom des repertoires a creer
#'
#' @return nothing cree les repertoires
#' @export
#'

fn02_cree_repertoires <-
  function(new_rep = c('1_scripts',
                       '2_data',
                       paste('2_data', ls_dates$annee_etude, sep ='/'),
                       '3_tables',
                       '4_resultats',
                       '5_publication')) {
    purrr::walk(new_rep, ~if (dir.exists(file.path(getwd(), .x)) == FALSE)
      dir.create(file.path(getwd(), .x)))
  }
