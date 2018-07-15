adstxt <- function(url) {
  # Parsing and error handling should be added here
    get_ads(url)
}

get_ads <- function(url) {
  readr::read_csv(file = url, col_names = FALSE, comment = "#")
}
