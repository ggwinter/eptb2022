#' Met en forme les tableaux
#'
#' @param x charactere
#' @param y charactere
#'
#' @return
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom stringr str_detect
#' @export
fn06_met_en_forme_liste <- function(x, y) {
  if (stringr::str_detect(y , "filtre_fr") == TRUE) {
    x %>%
      dplyr::mutate("territoire_cd" = "999", "territoire_lib" = "France") -> x
  } else{
    if (stringr::str_detect(y , "filtre_dep") == TRUE) {
      x %>%
        dplyr::mutate(reg_cd = NULL, reg_lib = NULL) %>%
        dplyr::rename("territoire_cd" = "dep_cd",
                      "territoire_lib" = "dep_lib") -> x
    } else{
      if (stringr::str_detect(y , "filtre_reg") == TRUE) {
        x %>%
          dplyr::mutate(dep_cd = NULL, dep_lib = NULL) %>%
          dplyr::rename("territoire_cd" = "reg_cd",
                        "territoire_lib" = "reg_lib") -> x
      } else{
        x
      }
      # print(x)
    }
  }
  return(x)
}


