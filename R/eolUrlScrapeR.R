#-------------------------------------------------------------------------#
#---------------------------- EOL URLSCRAPER  ----------------------------#
#-------------------------------------------------------------------------#

# author: alice stuart | date modified: 2020-03-11
# compiled in R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# running x86_64-apple-darwin15.6.0

#' Finds Encyclopedia of Life urls for each species
#'
#' \code{EOLUrlScrapeR} enters each value in search_terms into the eol search
#' bar and returns the links and text from the resultant dropdown menu it
#' returns a dataframe with link text and link for each search term, will
#' return NA if no curated results for search.
#'
#' @param names a tibble with all names associated with their internalTaxonIds
#' @param best_match logical, if true, will return the link for which the
#' text best matches the latin name searched, if false, will return all the
#' dropdown results for each search term
#' @return a tibble with columns \code{internalTaxonId}, \code{text} and
#' \code{link}
#' @section Requires: \code{\link{rselenium}}
#' @export
EOLUrlScrapeR <- function(names, best_match){
  # eCaps <- list(chromeOptions = list(args = c('--headless',
  #                                             '--disable-gpu',
  #                                             '--window-size=1280,800')))
  # rD <- RSelenium::rsDriver(extraCapabilities = eCaps)
  sci_names <- names %>%
    dplyr::filter(`language` == 'scientific') %>%
    dplyr::select(`internalTaxonId`, `scientificName`, `name`) %>%
    unique()
  latin_names <- sci_names$name
  internalTaxonId <- sci_names$internalTaxonId
  rD <- RSelenium::rsDriver()
  remDr <- rD[["client"]]
  url <- tidyr::tibble(text = character(),
                       link = character())
  remDr$navigate('https://eol.org/')
  for(i in 1:length(latin_names)){
    if(i != 1){
      for(j in 1:(stringr::str_length(latin_names[i-1])*2)){
        keysToSearchBox(remDr, 'css selector', '#q', '\u0008')
      }
      findAndClick(remDr, '/html/body/div[3]/div/div/h2')
    }
    keysToSearchBox(remDr, 'css selector', '#q', latin_names[i])
    Sys.sleep(1)
    # navigate dropdown
    temp_url <- urlFromDropdown(remDr, best_match, i, latin_names, internalTaxonId)
    url <- dplyr::bind_rows(url, temp_url)
  }
  closeRemDr(remDr, rD)
  url <- unique(url[!is.na(url$link),])
  if(length(duplicated(url$link))>0){
    warning('some urls have been assigned to more than one species,
             these can be found using the duplicated() function and
             will need to be assessed separately')
  }
  url <- dplyr::left_join(url, unique(sci_names[ ,c(1,2)])) %>%
    dplyr::select(`internalTaxonId`, `scientificName`, dplyr::everything())
  return(url)
}

#' Remove duplicate EOL urls
#'
#' \code{EOLDeduplicate} removes any cases where EOL urls have been assigned
#' to a species incorrectly because its name is contained within another
#' species name
#'
#' @param EOL_urls tibble created by \code{\link{EOLUrlScrapeR}}
#' @param names tibble created by binding IOC names to names from tidyRedlist
#' @return a tibble with columns \code{internalTaxonId}, \code{text} and
#' \code{link} with duplicates removed
#' @section Requires: \code{\link{rselenium}}
#' @export
EOLDeduplicate <- function(species_data){
  EOL_urls <- species_data[['EOL_urls']]
  names <- species_data[['names']]
  urls <- dplyr::mutate(EOL_urls,
                        rowNum = c(1:nrow(EOL_urls)))
  if(nrow(urls)==length(unique(urls$link))){
    message('no duplicates found')
    result <- EOL_urls
  } else {
    result <- intraSpecificDuplicates(urls) %>%
      interSpecificDuplicates(names = names)
  }
  if(nrow(result)<length(unique(EOL_urls$link))){
    stop('error: removed urls that were not duplicates')
  } else if (nrow(result)>length(unique(EOL_urls$text))){
    warning('not all duplicates removed, please check manually')
  }
  species_data[['unprocessed_EOL_urls']] <- species_data[['EOL_urls']]
  species_data[['EOL_urls']] <- result[ ,c(1:4)]
  return(species_data)
}

#' Extracts text and url from each element in an EOL search dropdown
#'
#' @param remDr rselenium remote driver
#' @param best_match logical, if true, will return the link for which the
#' text best matches the latin name searched, if false, will return all the
#' dropdown results for each search term
#' @param i the position in loop, set to 1 if not looping
#' @return a tibble with columns \code{internalTaxonId}, \code{text} and
#' \code{link}
#' @section Requires: \code{\link{rselenium}}
urlFromDropdown <- function(remDr, best_match, i, latin_names, internalTaxonId){
  NA_template <- tidyr::tibble(internalTaxonId = internalTaxonId[i],
                               text = NA,
                               link = NA)
  drop_down <- remDr$findElements(using = 'xpath',
                                  "/html/body/div[2]/form/span/div/div/div")
  if(length(drop_down)==1){
    url <- ExtractHyperlink(x_path = "/html/body/div[2]/form/span/div/div/div/a",
                                remDr = remDr,
                                id = internalTaxonId[i])
  } else if (length(drop_down)>1){
    x_path <- vector(mode = 'character',length = length(drop_down))
    for(j in 1: length(drop_down)){
      x_path[j] <- paste('/html/body/div[2]/form/span/div/div/div[',j,']/a',
                         sep='',
                         collapse='')
    }
    dropdown_options <- ExtractHyperlink(x_path,
                                             remDr,
                                             internalTaxonId[i])
    if(best_match == TRUE){
      option_no <- fuzzymatchposition(latin_names[i],dropdown_options[,1])
      if(length(option_no)>1){
        url <- NA_template
      } else {
        url <- dropdown_options[option_no,]
      }
    } else {
      url <- dropdown_options
    }
  } else if (length(drop_down)==0){
    url <- NA_template
  } else {
    warning('something is wrong!')
  }
  return(url)
}

#' Extracts text and url from hyperlink
#'
#' this function extracts the link text and url from an EOL dropdown list,
#' used in eolurlscraper it returns a dataframe with link text and link for
#' each entry in the EOL dropdown list
#'
#' @param x_path xpath for hyperlink
#' @param remDr rselenium remote driver
#' @param id the id associated with the term searched
#' @return a tibble with columns \code{internalTaxonId}, \code{text} and
#' \code{link}
#' @section Requires: \code{\link{rselenium}}
ExtractHyperlink <- function(x_path,remDr,id){
  text <- c()
  link <- c()
  for(i in 1:length(x_path)){
    dropdown_element <- remDr$findElement(using = 'xpath', x_path[i])
    text[i] <- dropdown_element$getElementText()
    link[i] <- dropdown_element$getElementAttribute(attrName = 'href')
  }
  result <- tidyr::tibble(internalTaxonId = rep_len(id,length(x_path)),
                         text = as.character(unlist(text)),
                         link = as.character(unlist(link)))
  return(result)
}

getEOLUrls <- function(species_data){
  species_data[['EOL_urls']] <- EOLUrlScrapeR(names = species_data[['names']],
                                              best_match = FALSE)
  return(species_data)
}


#' Finds rows that are due to a name being contained within another name
#'
#' @param duplicates a dataframe of all the duplicated urls
#' @param names a tibble with all names associated with their internalTaxonIds
#' @return a vector of all the ids of species names that are contained within
#' other species names
interSpecificDuplicates <- function(urls, names){
  rows <- c(1:nrow(urls))[duplicated(urls$link) |
                            duplicated(urls$link, fromLast = TRUE)]
  duplicates <- urls[rows, ] %>%
    dplyr::arrange(`text`)
  ids <- unique(duplicates$internalTaxonId)
  duplicate_names <- names %>%
    removeRepeatedNames() %>%
    dplyr::filter(`internalTaxonId` %in% ids,
                  `language` == 'scientific') %>%
    .[ ,c(1:5)] %>%
    unique()
  contained <- c()
  for (i in 1:length(ids)) {
    id_names <- duplicate_names$name[duplicate_names$internalTaxonId==ids[i]]
    id_search <- paste(id_names, sep = '|', collapse = '|')
    string <- duplicate_names$name[duplicate_names$internalTaxonId!=ids[i]]
    positions <- stringr::str_locate_all(string = string,
                                         pattern = id_search) %>%
      unlist()
    if(length(positions)==0){
      contained[i] <- FALSE
    } else {
      contained[i] <- TRUE
    }
  }
  contained_ids <- ids[contained]
  duplicate_rows <- duplicates[duplicates$internalTaxonId %in% contained_ids, ]$rowNum
  result <- urls[!urls$rowNum %in% duplicate_rows,]
  return(result)
}

intraSpecificDuplicates <- function(urls){
  rows <- c(1:nrow(urls))[duplicated(urls$link) |
                            duplicated(urls$link, fromLast = TRUE)]
  duplicates <- urls[rows, ] %>%
    dplyr::arrange(`text`)
  intra_duplicates <- duplicates[duplicated(duplicates[ ,c(1,4)]), ]
  row_num <- intra_duplicates$rowNum
  result <- urls[!urls$rowNum %in% row_num, ]
  return(result)
}

removeRepeatedNames <- function(names){
  rows <- c(1:nrow(names))[duplicated(names$name) |
                            duplicated(names$name, fromLast = TRUE)]
  duplicate_sci_names <- names[rows, ] %>%
    dplyr::mutate(rowNum = rows)
  duplicate_sci_names <- duplicate_sci_names[duplicate_sci_names$main!=TRUE, ]
  names <- names[-duplicate_sci_names$rowNum,]
  return(names)
}
