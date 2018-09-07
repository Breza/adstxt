wp <- "washingtontimes.com/ads.txt" %>%
  httr::GET() %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  textConnection() %>%
  readLines(warn = FALSE) %>%
  grep("^[^#]", ., value = TRUE) %>% # Remove lines that start with #
  stringi::stri_trim_both() %>%
  .[which(. != "")] # Remove blank lines

library(conflicted)
library(tidyverse)
library(purrr)
library(stringi)
library(httr)
filter <- dplyr::filter


# When given a URL...
#   Confirm it is a valid URL using regex
#   Strip subdomains (per IAB standard)
#   If URL does not end in /ads.txt, add it




# Parse each row of the ads.txt separately.
parse_adstxt_line <- function(ads_txt_line) {
  # Remove end-of-row commas.
  if(stringi::stri_sub(ads_txt_line, from = -1L, to = -1L) == ",") {
    ads_txt_line <- stringi::stri_sub(ads_txt_line, from = 1, to = nchar(ads_txt_line) - 1)
  }

  # Remove mid-line comments
  if(stringi::stri_detect_fixed(ads_txt_line, "#")) {
    ads_txt_line <- strsplit(ads_txt_line, "#", fixed = TRUE)[[1]][1]
  }

  # The ads.txt standard allows the use of a comma-delimited list and equal signs.
  # Detect which one is being used in this row.
  uses_equal <- stringi::stri_detect_fixed(ads_txt_line, "=")
  uses_comma <- stringi::stri_detect_fixed(ads_txt_line, ",")


  if (uses_equal) {
    # Not yet implemented
    result <- c(NA, NA, NA, NA)
  }
  else if (uses_comma) {
    result <- ads_txt_line %>%
      strsplit(",") %>%
      .[[1]] %>%
      stringi::stri_trim_both()

    result_length <- length(result)

    # Throw an error if there are <3 or >4 elements.
    if (result_length < 3 || result_length > 4) {
      # stop(paste0(
      #   "Result has a length of ", result_length, ". Length of 3 or 4 expected.", "Here is the offending line:\n", ads_txt_line
      # ))
    } else if (result_length == 3) {
      # The fourth parameter is rarely used. Add NA where it's missing.
      result <- c(result, NA)
    }

  }

  names(result) <- c("advertiser", "account_id", "relationship", "certification_id")
  result <- as.data.frame(as.list(result), stringsAsFactors = FALSE)
  result <- result[, 1:4]

  return(result)
}


validate_url <- function(url) {
  stringi::stri_detect(url,
                       regex =
                         "(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$")
}


parse_adstxt_site <- function(url) {
  # Validate url as valid URL
  if(!validate_url(url)) {
    stop(paste(url, "is not a valid URL"))
  }

  # If url does not contain "ads.txt" append it to the end of url
  if(stringi::stri_detect_fixed(url, "ads.txt")) {
    url <- paste0(url, "ads.txt")
  }

  url %>%
    httr::GET() %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    textConnection() %>%
    readLines(warn = FALSE) %>%
    grep("^[^#]", ., value = TRUE) %>% # Remove lines that start with #
    stringi::stri_trim_both() %>%
    .[which(. != "")] %>%  # Remove blank lines
    purrr::map_dfr(parse_adstxt_line) %>%
    filter(!(is.na(advertiser) && is.na(account_id) && is.na(relationship) && is.na(certification_id))) %>%
    mutate(url = url)
}
