#' Title
#'
#' @param data
#' @param key
#' @param values
#' @param sep
#'
#' @return
#' @export
#'
#' @examples
#'
spread_multivalue = function(data, key, values, sep = ""){

  if (key == "key2"){
    stop("use a different name for key")
  }

  values <- dplyr::enquo(values)

  data %>%
    tidyr::gather("key2", "value", !!values) # %>%
    # tidyr::unite("keys", "key2", key, sep = sep) %>%
    # tidyr::spread("keys", "value")

  # data %>%
  #   tidyr::gather("key2", "value", !!vars) %>%
  #   tidyr::extract("key2", c("colname", key), regex, remove = TRUE) %>%
  #   tidyr::spread("colname", "value")

}
