#-------------------------------------------------------------------------#
#--------------------------- WIKIPEDIA SCRAPER ---------------------------#
#-------------------------------------------------------------------------#


#' scrape all text for a vector of species from wikipedia
#'
#' @param wiki_searches a tibble with cols \code{ID} (a unique identifier
#' for each  species) and \code{search} (the term that will be searched in
#' wikipedia to find the page)
#' @param language the language code of wikipedia page to search, defaults
#' to English. A full list of codes used can be found in the
#' wiki_language_codes table in this package (a copy of the table from
#' https://en.wikipedia.org/wiki/List_of_Wikipedias)
#' @param browser the browser to be used by RSelenium, one of 'chrome',
#' 'firefox', 'phantomjs', 'internet explorer'
#' @return a list containing one tibble with the name of each species'
#' wikipedia article and another the text from each section
#'
#' @export
wikiScrapeR <- function(wiki_searches,
                        language = 'en',
                        browser = c('chrome', 'firefox', 'phantomjs', 'internet explorer')
                        ){
  if(!language %in% wiki_language_codes$`WP code`){
    stop('language input is not valid, please check the wiki_language_codes table for correct code for each language')
  }
  rD <- RSelenium::rsDriver(browser = 'firefox')
  remDr <- rD[["client"]]
  url <- c()
  text <- c()
  contents <- c()
  pictures <- c()
  for(i in 1:nrow(wiki_searches)){
    remDr$navigate(paste0('https://',
                          language,
                          '.wikipedia.org/wiki/',
                          wiki_searches$search[i]
                          )
                   )
    # give page a chance to load
    Sys.sleep(1.3)
    # returns 1 if the no page returned text comes up and 0 if it doesn't
    no_page_returned <- remDr$findElements('css', 'table#noarticletext') %>%
      length
    if(no_page_returned == 1){
      url[i] <- 'no page'
      text[i] <- NA
      contents[i] <- NA
      pictures[i] <- NA
    } else{
      url[i] <- unlist(remDr$getCurrentUrl())
      text[i] <- getElementText(remDr, 'css', 'div#mw-content-text')
      if(length(remDr$findElements('xpath', '//*[@id="toc"]'))!=0){
        contents[i] <- getElementText(remDr, 'xpath', '//*[@id="toc"]')
      } else {
        contents[i] <- NA
      }
      pictures[i] <- length(remDr$findElements(using = 'class', 'image')) +
        length(remDr$findElements(using = 'class', 'mediaContainer'))
    }

  }
  closeRemDr(remDr, rD)
  gc()
  split_texts <- splitWikiScrape(text = text, contents = contents)
  result <- textListToTibble(split_texts = split_texts,
                             pictures = pictures,
                             url = url,
                             ID = wiki_searches$ID
                             )
  return(result)
}

#' make names into wikipedia url format
#'
#' @param main_names a tibble with the ID of each species and its main name
#' (scientific is best to minimise ambiguity)
#'
#' @return the same tibble with column 'search' added containing formatted
#' names
#'
formatNamesToWiki <- function(main_names){
  dplyr::mutate(main_names,
                search = gsub(x = main_name,
                              pattern = '[ ]',
                              replacement = '_'
                              )
                )
}

#' split text scraped from wikipedia pages into the constituent sections
#'
#' @param text vector of texts from wikipedia pages
#' @param contents vector of contents sections corresponding to texts
#'
#' @return a list with one element per page and, within that, one element
#' per section
#'
splitWikiScrape <- function(text, contents){
  split_contents <- stringr::str_split(string = contents, pattern = '\\n') %>%
    sapply(function(x) gsub(pattern = '^[0-9][.]*[0-9]* ', replacement = '', x = x))
  split_texts <- list()
  for(i in 1:length(text)){
    if(!is.na(text[i])){
      if(length(text) == 1){
        pattern = paste('\\n', split_contents, '\\[', sep = '', collapse = '|')
      } else if(!all(is.na(split_contents[[i]]))){
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
        if(length(text) == 1){
          names(split_texts[[i]]) <- split_contents[c(1:length(split_texts[[i]]))]
          names(split_texts[[i]])[1] <- 'Introduction'
        } else{
          names(split_texts[[i]]) <- split_contents[[i]][c(1:length(split_texts[[i]]))]
          names(split_texts[[i]])[1] <- 'Introduction'
        }
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
#'
textListToTibble <- function(split_texts, pictures, url, ID){
  combined_tibble <- tidyr::tibble(ID = character(),
                                   url = character(),
                                   section = character(),
                                   text = character(),
                                   length = numeric()
                                   )
  for(i in 1:length(split_texts)){
    if(!all(is.na(split_texts[[i]]))){
      names <- names(split_texts[[i]])
      text_tibble <- tidyr::tibble(ID = ID[i],
                                   url = url[i],
                                   section = names,
                                   text = split_texts[[i]],
                                   length = (stringr::str_count(text, ' ') + 1)
                                   )

      picture_tibble <- tidyr::tibble(ID = ID[i],
                                      url = url[i],
                                      section = 'pictures',
                                      text = NA,
                                      length = pictures[i]
                                      )

      combined_tibble <- dplyr::bind_rows(combined_tibble, text_tibble, picture_tibble)
    } else{
      text_tibble <- tidyr::tibble(ID = ID[i],
                                   url = url[i],
                                   section = NA,
                                   text = NA,
                                   length = 0
                                   )

      picture_tibble <- tidyr::tibble(ID = ID[i],
                                      url = url[i],
                                      section = 'pictures',
                                      text = NA,
                                      length = pictures[i]
                                      )

      combined_tibble <- dplyr::bind_rows(combined_tibble, tibble, picture_tibble)
    }
  }
  return(combined_tibble)
}

