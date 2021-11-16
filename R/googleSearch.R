#-------------------------------------------------------------------------#
#-------------------------- GOOGLE API SEARCH  ---------------------------#
#-------------------------------------------------------------------------#

#' Make google urls from names for api queries
#' @export
createGoogleUrls <- function(query, api, custom_search, site_restricted = FALSE){
  url_start <- ifelse(site_restricted,
                      'https://www.googleapis.com/customsearch/v1/siterestrict?key=',
                      "https://www.googleapis.com/customsearch/v1?key=")
  url <- paste0(url_start,
                api,
                "&cx=",
                custom_search,
                query$google)
  return(url)
}

#' Query google jsonAPI
#' @export
googleQuery <- function(query, api, custom_search, site_restricted = FALSE){
  urls <- createGoogleUrls(query, api, custom_search, site_restricted)
  results <- list()
  for(i in 1:length(urls)){
    results[[as.character(query$internalTaxonId[i])]] <- httr::content(httr::GET(urls[i]))
    Sys.sleep(0.4)
  }
  return(results)
}
