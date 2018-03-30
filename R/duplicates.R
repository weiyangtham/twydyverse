#' Filter duplicates on desired variables
#'
#' @param data a tbl
#' @param ... Variables to find duplicates of
#'
#' @return
#' @export
#'
#' @examples
#'
#' # df = data.frame(grp1 = c())

filter_duplicates <- function(data, ...) {
  group_var <- dplyr::quos(...)

  data %>%
    dplyr::group_by(!!! group_var) %>%
    dplyr::filter(max(dplyr::row_number()) > 1) %>%
    dplyr::ungroup()
}

#' Filter unique observations
#'
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

filter_unique <- function(data, ...) {
  group_var <- dplyr::quos(...)

  data %>%
    dplyr::group_by(!!! group_var) %>%
    dplyr::filter(max(dplyr::row_number()) == 1) %>%
    dplyr::ungroup()
}




