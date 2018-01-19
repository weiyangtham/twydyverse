#' tidyr's gather for multiple values
#'
#' Extension of `tidyr::gather` when there is a key with multiple values. Same
#' purpose as Stata's `reshape` long.
#'
#' @param data a data frame
#' @param vars A selection of columns. If empty, all variables are selected.
#' You can supply bare variable names, select all variables between x and z with x:z,
#' exclude y with -y. For more options, see the dplyr::select() documentation.
#' @param key key
#' @param regex a regular expression used to extract the desired values
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
#' gather_multivalue(scores, -id, "year")
#' gather_multivalue(scores, age2000:scores2010, "year")
#'
#'
#' scores2 <- data.frame(
#' id = LETTERS[1:5],
#' age_2000 = 11:15,
#' age_2010 = 21:25,
#' scores_2000 = 96:100,
#' scores_2010 = 100:96)
#'
#' gather_multivalue(scores2, -id, "year", regex = "([a-z]+)_(\\d+)")


gather_multivalue = function(data, vars, key = "key", regex = "^([a-zA-Z]+)(\\d+)$"){

  if (key == "key2"){
    stop("use a different name for key")
  }

  vars <- dplyr::enquo(vars)

  # print(vars)
  #
  # print(quo(data %>%
  #             gather(key, value, !!vars) %>%
  #             tidyr::extract(key, c("colname", key2), regex)))

  data %>%
    tidyr::gather("key2", "value", !!vars) %>%
    tidyr::extract("key2", c("colname", key), regex, remove = TRUE) %>%
    tidyr::spread("colname", "value")

}
