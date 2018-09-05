get_ads <- function(url) {
  readr::read_csv(file = url, col_names = FALSE, comment = "#")
}


validate_url <- function(url) {
  stringi::stri_detect(url,
                       regex =
                         "(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$")
}

read_adstxt <- function(url) {
  if(!validate_url(url)) {
    stop(paste(url, "is not a valid URL"))
  }

  readLines(url) %>%
    # Delete comments
    stringi::stri_replace(mode = "first", regex = "#.*", replacement = "") %>%
    stringi::stri_trim_both() %>%
    .[!grepl(x = ., pattern = "^$")] %>%
    # Delete value declarations
    .[!grepl(x = ., pattern = "=")] %>%
    read.csv(text = ., header = FALSE, stringsAsFactors = FALSE) %>%
    tbl_df()
}

adstxt <- function(url) {
  # Parsing and error handling should be added here
  get_ads(url)
}
