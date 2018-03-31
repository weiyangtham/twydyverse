#' Filter duplicates on desired variables
#'
#' @description Filter duplicates or unique values. These are
#' wrappers around a `group_by` then `filter`.
#'
#' @param data a tbl
#' @param ... Variables to find duplicates of
#'
#' @export
#'
#' @examples
#'
#' df <- data.frame(grp1 = c(1, 1, 2, 2, 3), grp2 = c(1, 2, 1, 2, 2))
#'
#' df %>% filter_duplicates(grp1)
#' df %>% filter_duplicates(grp2)
#'
#' df %>% filter_unique(grp1)

filter_duplicates <- function(data, ...) {
  group_var <- dplyr::quos(...)

  data %>%
    dplyr::group_by(!!! group_var) %>%
    dplyr::filter(max(dplyr::row_number()) > 1) %>%
    dplyr::ungroup()
}

#' @rdname filter_duplicates
filter_unique <- function(data, ...) {
  group_var <- dplyr::quos(...)

  data %>%
    dplyr::group_by(!!! group_var) %>%
    dplyr::filter(max(row_number()) == 1) %>%
    dplyr::ungroup()
}




