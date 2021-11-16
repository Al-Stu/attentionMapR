#-------------------------------------------------------------------------#
#--------------------------- WIKIPEDIA SCRAPER ---------------------------#
#-------------------------------------------------------------------------#


#' scrape all text for a vector of species from wikipedia
#'
#' @param wiki_searches a tibble with cols \code{ID} (a unique identifier for
#' each  species) and \code{search} (the term that will be searched in wikipedia
#' to find the page)
#' @param browser the browser to be used by RSelenium, one of 'chrome',
#' 'firefox', 'phantomjs', 'internet explorer'
#' @return a list containing one tibble with the name of each species' wikipedia
#' article and another the text from each section
#' page
#'
#' @import RSelenium
#' @import rJava
#' @export
wikiScrapeR <- function(wiki_searches,
                        browser = c('chrome', 'firefox', 'phantomjs', 'internet explorer')
                        ){
  rD <- RSelenium::rsDriver(browser = browser)
  remDr <- rD[["client"]]
  text <- c()
  contents <- c()
  pictures <- c()
  remDr$navigate('https://www.wikipedia.org/')
  keysToSearchBox(remDr,'xpath','//*[@id="searchInput"]',scientificName[1])
  findAndClick(remDr, '/html/body/div[3]/form/fieldset/button/i')
  text[1] <- getElementText(remDr, 'xpath', '/html/body/div[3]/div[3]/div[4]/div')
  if(length(remDr$findElements('xpath', '//*[@id="toc"]'))!=0){
    contents[1] <- getElementText(remDr, 'xpath', '//*[@id="toc"]')
  } else {
    contents[1] <- NA
  }
  pictures[1] <- length(remDr$findElements(using = 'class', 'image')) +
    length(remDr$findElements(using = 'class', 'mediaContainer'))
  for(i in 2:length(scientificName)){
    Sys.sleep(2)
    keysToSearchBox(remDr, 'xpath', '//*[@id="searchInput"]', scientificName[i])
    findAndClick(remDr, '//*[@id="searchButton"]')
    no_page <- remDr$findElements('xpath', '//*[@id="firstHeading"]')
    if(length(no_page) != 0){
      no_page_present <- getElementText(remDr, 'xpath', '//*[@id="firstHeading"]') %>%
        grepl(pattern = 'Search results')
    } else{
      no_page_present <- FALSE
    }
    if(!no_page_present){
      text[i] <- getElementText(remDr, 'xpath', '/html/body/div[3]/div[3]/div[4]/div')
      if(length(remDr$findElements('xpath', '//*[@id="toc"]'))!=0){
        contents[i] <- getElementText(remDr, 'xpath', '//*[@id="toc"]')
      } else {
        contents[i] <- NA
      }
    } else{
      text[i] <- NA
    }
    pictures[i] <- length(remDr$findElements(using = 'class', 'image')) +
      length(remDr$findElements(using = 'class', 'mediaContainer'))
  }
  closeRemDr(remDr, rD)
  gc()
  split_texts <- splitWikiScrape(text = text, contents = contents)
  result <- textListToTibble(split_texts, pictures, internalTaxonId, scientificName)
  return(result)
}

#' Find an element and get text
#'
#' @param remDr
#' @param using
#' @param path
#' @return a value containing the text from the element
#' @export
getElementText <- function(remDr, using, path){
  page <- remDr$findElement(using, path)
  text  <- page$getElementText() %>%
    unlist()
  return(text)
}

#' Find an element and get text
#'
#' @param remDr
#' @param using
#' @param path
#' @return a value containing the text from the element
#' @export
#'
splitWikiScrape <- function(text, contents){
  split_contents <- stringr::str_split(string = contents, pattern = '\\n') %>%
    sapply(function(x) gsub(pattern = '^[0-9][.]*[0-9]* ', replacement = '', x = x))
  split_texts <- list()
  for(i in 1:length(text)){
    if(!is.na(text[i])){
      if(!all(is.na(split_contents[[i]]))){
        pattern = paste('\\n', split_contents[[i]], '\\[', sep = '', collapse = '|')
      } else {
        pattern = paste('\\n', c('References|External links'), '\\[', sep = '', collapse = '|')
      }
      if(any(grepl(pattern = pattern, x = text[i]))){
        split_texts[[i]] <- stringr::str_split(string = text[i],
                                               pattern = pattern) %>%
          sapply(function(x) stringr::str_remove(string = x, pattern = '^edit]\\n')) %>%
          tm::stripWhitespace() %>%
          as.vector()
      } else {
        split_texts[[i]] <- stringr::str_split(string = text[i],
                                               pattern = paste('\\n',
                                                               split_contents[[i]],
                                                               '\\n',
                                                               sep = '',
                                                               collapse = '|')) %>%
          unlist() %>%
          tm::stripWhitespace() %>%
          as.vector()
      }
      if(!any(is.na(split_contents[[i]]))){
        names(split_texts[[i]]) <- split_contents[[i]][c(1:length(split_texts[[i]]))]
        names(split_texts[[i]])[1] <- 'Introduction'
      } else {
        names(split_texts[[i]]) <-
          c('Introduction','References','External links')[1:length(split_texts[[i]])]
      }
    } else{
      split_texts[[i]] <- NA
    }

  }
  return(split_texts)
}

#' Convert a list of text into a tibble
#'
#' @param list a list of texts where each element is the text for one
#' species and each sub-element is a section of that text
#' @param scientificNames the scientific names to be searched
#' @param internalTaxonId an identifier for each species
#' @return tibble with each section of the text as a row
#'
#' @export
textListToTibble <- function(list, pictures, internalTaxonId, scientificName){
  text_tibble <- tidyr::tibble(internalTaxonId = character(),
                               scientificName = character(),
                               section = character(),
                               text = character(),
                               length = numeric())
  for(i in 1:length(list)){
    if(!all(is.na(list[[i]]))){
      names <- names(list[[i]])
      tibble <- tidyr::tibble(internalTaxonId = internalTaxonId[i],
                              scientificName = scientificName[i],
                              section = names,
                              text = list[[i]],
                              length = stringr::str_count(text, ' '))

      picture_tibble <- tidyr::tibble(internalTaxonId = internalTaxonId[i],
                                      scientificName = scientificName[i],
                                      section = 'pictures',
                                      text = NA,
                                      length = pictures[i])

      text_tibble <- dplyr::bind_rows(text_tibble, tibble, picture_tibble)
    } else{
      tibble <- tidyr::tibble(internalTaxonId = internalTaxonId[i],
                              scientificName = scientificName[i],
                              section = NA,
                              text = NA,
                              length = 0)

      picture_tibble <- tidyr::tibble(internalTaxonId = internalTaxonId[i],
                                      scientificName = scientificName[i],
                                      section = 'pictures',
                                      text = NA,
                                      length = pictures[i])

      text_tibble <- dplyr::bind_rows(text_tibble, tibble, picture_tibble)
    }
  }
  return(text_tibble)
}

getWiki <- function(species_data){
  scientificName <- species_data[['assessments']]$scientificName
  internalTaxonId <- species_data[['assessments']]$internalTaxonId
  species_data[['wiki_texts']] <- wikiScrapeR(scientificName, internalTaxonId)
  return(species_data)
}
