#' tidyr's spread for multiple values
#'
#' Extension of `tidyr::spread` when there is a key with multiple values. Same
#' purpose as Stata's `reshape` wide.
#'
#' @param data a data frame
#' @param key key
#' @param values A selection of columns. If empty, all variables are selected.
#' You can supply bare variable names, select all variables between x and z with x:z,
#' exclude y with -y. For more options, see the dplyr::select() documentation.
#' @param sep separator to use between key and values
#'
#' @export
#'
#' @examples
#'
#' scores <- data.frame(
#' id = LETTERS[1:5],
#' age2000 = 11:15,
#' age2010 = 21:25,
#' scores2000 = 96:100,
#' scores2010 = 100:96)
#'
#' # gather_multivalue(scores, -id, "year")
#' scores_long <- gather_multivalue(scores, age2000:scores2010, "year")
#'
#' spread_multivalue(scores_long, "year", c(age, scores))

spread_multivalue = function(data, key, values, sep = ""){

  if (key == "key2"){
    stop("use a different name for key")
  }

  values <- dplyr::enquo(values)

  data %>%
    tidyr::gather("key2", "value", !!values) %>%
    tidyr::unite("keys", dplyr::one_of(c("key2", key)), sep = sep) %>%
    tidyr::spread("keys", "value")

  # data %>%
  #   tidyr::gather("key2", "value", !!vars) %>%
  #   tidyr::extract("key2", c("colname", key), regex, remove = TRUE) %>%
  #   tidyr::spread("colname", "value")

}
