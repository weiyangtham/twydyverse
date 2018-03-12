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
#' df = data.frame(grp1 = c())

filter_duplicates <- function(data, ...) {
  group_var <- quos(...)

  data %>%
    group_by(!!! group_var) %>%
    filter(max(row_number()) > 1) %>%
    ungroup()
}
