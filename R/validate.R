#' Validate that a URL is valid
#'
#' Validate that a URL is properly formatted using regex.
#'
#' @param url The URL to be assessed.
#' @return TRUE if url is a properly formatted URL; FALSE if it is not.
validate_url <- function(url) {
  stringi::stri_detect(url,
                       regex =
                         "(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$")
}
