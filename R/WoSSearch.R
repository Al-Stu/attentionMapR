#-------------------------------------------------------------------------#
#----------------------------- WoS SEARCH  -------------------------------#
#-------------------------------------------------------------------------#

#' Search Web of Science
#' @param search_terms a vector/ column of WoS format search terms
#' @param search_ids a vector/ column the same length as \code{search_ids}
#' containing an ID to be used to identify search results to each search term
#' @param directory the directory to be used for downloaded search results
#' @return the number of hits for each search term, the directory specified in
#' \code{directory} populated with search results
#' @export
WoSSearch <- function(search_terms, search_ids, directory, WoS_url){
  eCaps <- list(chromeOptions = list(
    # args = c('--headless', '--disable-gpu', '--window-size=1280,800'),
    prefs = list("download.default_directory" = directory)))
  rD <- RSelenium::rsDriver(extraCapabilities = eCaps)
  remDr <- rD[["client"]]
  exported <- WoSSearchAndExport(remDr, search_terms, search_ids, WoS_url)
  closeRemDr(remDr, rD)
  message(paste('files can be found in ',directory,sep='',collapse=''))
  exported$exports[is.na(exported$exports)] <- 0
  exported$hits[is.na(exported$hits)] <- 0
  references <- importRefs(directory, search_ids, exported$exports)
  ### need to see if no. references == hits
  check <- tidyr::tibble(internalTaxonId = character(),
                         numRefs = numeric(),
                         hits = numeric())
  for(i in 1:length(exported$internalTaxonId)){
    check[i, ] <- c(exported$internalTaxonId[i],
                    sum(references$internalTaxonId == exported$internalTaxonId[i]),
                    exported$hits[exported$internalTaxonId == exported$internalTaxonId[i]])
  }
  results <- list(references, check)
  return(results)
}

#' Search and export from WoS
#'
#' Searches each of the search terms from \code{search_terms} in turn and
#' exports their results to chrome's download directory
#'
#' @param remDr remote driver for rselenium
#' @param search_terms a vector/ column of WoS format search terms
#' @param search_ids a vector/ column the same length as \code{search_ids}
#' containing an ID to be used to identify search results to each search term
#' @return a vector with the number of files exported for each search
#' @section Used in: \code{\link{WoSSearch}}
WoSSearchAndExport <- function(remDr, search_terms, search_ids, WoS_url){
  exports <- c()
  hits <- c()
  for(i in 1:length(search_ids)){
    remDr$navigate(WoS_url)
    Sys.sleep(3)
    # Post search term
    keysToSearchBox(remDr, 'id', "value(input1)", search_terms[i])
    Sys.sleep(0.5)
    # click search
    findAndClick(remDr, "/html/body/form[1]/div[1]/div/div/div/table/tbody/tr/td[3]/span/span[1]/button")
    Sys.sleep(2)
    # check page has changed
    search_box <- remDr$findElements(using = 'id', "value(input1)")
    if(length(search_box)==0){
      # read no. hits
      hits_element <- remDr$findElement(using = 'xpath', '//*[@id="hitCount.top"]')
      hits[i] <- as.numeric(tm::removePunctuation(unlist(hits_element$getElementText())))
      # export
      exports[i] <- WoSExport(remDr = remDr, hits = as.numeric(hits[i]))
    }
  }
  result <- tidyr::tibble(internalTaxonId = search_ids,
                          hits = hits,
                          exports = exports)
  return(result)
}

#' Navigate WoS export menu
#'
#' downloads all records returned by a search, with the parameters set in
#' \code{\link{WoSInitialExportSetup}} and \code{\link{WoSSubsequentExportSetup}}
#'
#' @param remDr remote driver for rselenium
#' @param hits the number of hits returned by the WoS search
#' @return the number of files that have been exported
#' @section Used in: \code{\link{WoSSearchAndExport}}
WoSExport <- function(remDr, hits){
  exported <- 0
  # check what state WoS is in, choose exportsetup accordingly
  xpath_1 <- '//*[@id="exportTypeName"]'
  xpath_2 <- '//*[@id="page"]/div[1]/div[26]/div[2]/div/div/div/div[2]/div[3]/div[3]/div[2]/div[1]/button'
  if(length(remDr$findElements('xpath', xpath_1))==0){
    findAndClick(remDr, xpath_2)
  } else {
    findAndClick(remDr, xpath_1)
  }
  WoSInitialExportSetup(remDr)
  findAndClick(remDr,'//*[@id="exportButton"]')
  exported <- exported + 1
  Sys.sleep(2)
  checkExportMessage(remDr)
  if(hits >= 500){
    j <- 1
    if(hits >=1000){
      for(j in 2:floor(hits /500)){
        # click in records from box
        WoSSubsequentExportSetup(remDr)
        keysToSearchBox(remDr, 'xpath', '//*[@id="markFrom"]', as.character(j*500-499))
        keysToSearchBox(remDr, 'xpath', '//*[@id="markTo"]', as.character(j*500))
        findAndClick(remDr,'//*[@id="exportButton"]')
        exported <- exported + 1
        Sys.sleep(2)
        checkExportMessage(remDr)
      }
    }
    WoSSubsequentExportSetup(remDr)
    keysToSearchBox(remDr, 'xpath', '//*[@id="markFrom"]', as.character(j*500+1))
    keysToSearchBox(remDr, 'xpath', '//*[@id="markTo"]', as.character(hits))
    findAndClick(remDr,'//*[@id="exportButton"]')
    exported <- exported + 1
    Sys.sleep(2)
    checkExportMessage(remDr)
  }
  return(exported)
}

#' Select parameters in WoS export dropdowns
#'
#' selects full record and BibTeX as export parameters, suitable for the first
#' time this menu is navigates
#'
#' @param remDr remote driver for rselenium
#' @section Used in: \code{\link{WoSExport}}
WoSInitialExportSetup <- function(remDr){
  other_file_types <- remDr$findElements('xpath', '/html/body/div[1]/div[26]/div[2]/div/div/div/div[2]/div[3]/div[3]/div[2]/div[1]/ul/li/span/ul/li[3]/a')
  if(length(other_file_types)!=0){
    findAndClick(remDr, '/html/body/div[1]/div[26]/div[2]/div/div/div/div[2]/div[3]/div[3]/div[2]/div[1]/ul/li/span/ul/li[3]/a')
  }
  # click File Format dropdown
  findAndClick(remDr,'//*[@id="select2-saveOptions-container"]')
  # click BibTeX
  bibtex <- remDr$findElement(using = 'xpath','//*[@id="select2-saveOptions-results"]')
  loc <- bibtex$getElementLocation()
  ypos <- round(as.numeric(loc$y)/8)
  remDr$mouseMoveToLocation(webElement = bibtex)
  remDr$mouseMoveToLocation(y = -60)
  remDr$click()
  # click Record Content dropdown
  findAndClick(remDr,'//*[@id="select2-bib_fields-container"]')
  # click Full Record
  box <- remDr$findElement(using = 'xpath','//*[@id="select2-bib_fields-results"]')
  remDr$mouseMoveToLocation(webElement = box)
  remDr$click()
  # check 'Records from:' box
  findAndClick(remDr,'//*[@id="numberOfRecordsRange"]')
}

#' Select parameters in WoS export dropdowns
#'
#' selects full record and BibTeX as export parameters, suitable for the second
#' or subsequent times this menu is navigated
#'
#' @param remDr remote driver for rselenium
#' @section Used in: \code{\link{WoSExport}}
WoSSubsequentExportSetup <- function(remDr){
  Sys.sleep(0.1)
  # click export button
  findAndClick(remDr,'/html/body/div[1]/div[26]/div[2]/div/div/div/div[2]/div[3]/div[3]/div[2]/div[1]/button')
  # click Record Content dropdown
  findAndClick(remDr,'//*[@id="select2-bib_fields-container"]')
  # click Full Record
  box <- remDr$findElement(using = 'xpath','//*[@id="select2-bib_fields-results"]')
  remDr$mouseMoveToLocation(webElement = box)
  remDr$click()
  # check 'Records from:' box
  findAndClick(remDr,'//*[@id="numberOfRecordsRange"]')
}

#' Import downloaded .bib references
#'
#' imports all downloaded .bib files in \code{directory} and associates
#' them with the correct \code{search_ids} using \code{exports}
#'
#' @param directory the directory to be used for downloaded search results
#' @param search_ids a vector/ column the same length as \code{search_ids}
#' containing an ID to be used to identify search results to each search term
#' @param exports
#' @return the number of files that have been exported
#' @section Used in: \code{\link{WoSSearchAndExport}}
#' @export
importRefs <- function(directory, search_ids, exported){
  current_wd <- getwd()
  setwd(directory)
  files_details <- file.info(list.files(pattern = ".bib$"))
  sorted_details <- files_details[with(files_details, order(as.POSIXct(mtime))), ]
  files <- rownames(sorted_details)
  references <- tidyr::tibble(internalTaxonId = character())
  if(exported$exports[1]!=0){
    references <- lapply(files[1:exported$exports[1]], revtools::read_bibliography) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(internalTaxonId = search_ids[1])
    if(nrow(references)!=exported$hits[1]){
      warning(paste0('incorrect number of references for species ', search_ids[1]))
    }
  }
  for(i in 2:length(search_ids)){
    if(exported$exports[i]!=0){
      temp_refs <- lapply(files[(sum(exported$exports[1:(i-1)])+1):sum(exported$exports[1:i])],
                          revtools::read_bibliography) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(internalTaxonId = search_ids[i])
      if(nrow(temp_refs)!=exported$hits[i]){
        warning(paste0('incorrect number of references for species ', search_ids[i]))
      }
      references <- dplyr::bind_rows(references, temp_refs)
    }
  }
  setwd(current_wd)
  referenceId <- paste('p',c(1:nrow(references)),sep = '')
  result <- cbind(referenceId, references) %>%
    tidyr::as_tibble() %>%
    dplyr::mutate_if(sapply(., is.factor), as.character) %>%
    dplyr::select(`internalTaxonId`, `referenceId`, dplyr::everything())
  return(result)
}

#' Find an element and click it in rselenium
#'
#' finds element specified by \code{xpath} and clicks it
#'
#' @param remDr remote driver for rselenium
#' @param xpath xpath for element to be clicked
#' @section Used in: \code{\link{WoSExport}}, \code{\link{WoSInitialExportSetup}} and
#' \code{\link{WoSSubsequentExportSetup}}
#' @export
findAndClick <- function(remDr,xpath){
  element <- remDr$findElement(using = 'xpath',xpath)
  element$clickElement()
}

#' Find an element and enter keys
#'
#' finds element specified by \code{xpath} and enters \code{keys}
#'
#' @param remDr remote driver for rselenium
#' @param method the type of path to be used for element selection
#' @param path path for element to be clicked
#' @param keys the keys to be entered into the element
#' @section Used in: \code{\link{WoSExport}}
#' @export
keysToSearchBox <- function(remDr, method, path, keys){
  box <- remDr$findElement(using = method,value = path)
  box$clearElement()
  box$sendKeysToElement(list(keys))
}

#' Checks for WoS no results error message and closes if present
#'
#' @param remDr remote driver for rselenium
#' @section Used in: \code{\link{WoSExport}}
#' @export
checkExportMessage <- function(remDr){
  Sys.sleep(0.5)
  box <- remDr$findElements(using = 'xpath',value = '/html/body/div[11]/div[1]/button/span[1]')
  if(length(box)!=0){
    findAndClick(remDr,'/html/body/div[11]/div[1]/button/span[1]')
  }
}

#' closes remDr and rD
#'
#' @param remDr remote driver for rselenium
#' @param rD also remote driver? for rselenium
#' @section Used in: \code{\link{WoSSearch}}
#' @export
closeRemDr <- function(remDr, rD){
  remDr$close()
  rD[["server"]]$stop()
  rm(rD)
  gc()
}

#' Carries out multiple searches on Web of Science and saves the number of
#' hits for each
#'
#'
#' @param search_terms a vector/ column of WoS format search terms
#' @param search_ids a vector/ column the same length as \code{search_ids}
#' containing an ID to be used to identify search results to each search term
#' @param WoS_url the url for the WoS page with a search bar once you have logged on
#' @export
WoSHits <- function(search_terms, search_ids, directory, WoS_url){
  eCaps <- list(chromeOptions = list(
    # args = c('--headless', '--disable-gpu', '--window-size=1280,800'),
    prefs = list("download.default_directory" = directory)))
  rD <- RSelenium::rsDriver(extraCapabilities = eCaps)
  remDr <- rD[["client"]]
  hits <- WoSSearchHits(remDr, search_terms, search_ids, WoS_url)
  closeRemDr(remDr, rD)
  hits[is.na(hits)] <- 0
  return(hits)
}

#' Carries out multiple searches on Web of Science and saves the number of
#' hits for each
#'
#' @param search_terms a vector/ column of WoS format search terms
#' @param search_ids a vector/ column the same length as \code{search_ids}
#' containing an ID to be used to identify search results to each search term
#' @param WoS_url the url for the WoS page with a search bar once you have logged on
#' @param remDr remote driver for rselenium
WoSSearchHits <- function(remDr, search_terms, search_ids, WoS_url){
  hits <- c()
  for(i in 1:length(search_ids)){
    remDr$navigate(WoS_url)
    Sys.sleep(3)
    # Post search term
    keysToSearchBox(remDr, 'id', "value(input1)", search_terms[i])
    Sys.sleep(0.5)
    # click search
    findAndClick(remDr, "/html/body/form[1]/div[1]/div/div/div/table/tbody/tr/td[3]/span/span[1]/button")
    Sys.sleep(2)
    # check page has changed
    search_box <- remDr$findElements(using = 'id', "value(input1)")
    if(length(search_box)==0){
      # read no. hits
      hits_element <- remDr$findElement(using = 'xpath', '//*[@id="hitCount.top"]')
      hits[i] <- as.numeric(tm::removePunctuation(unlist(hits_element$getElementText())))
    }
  }
  result <- tidyr::tibble(searchIds = search_ids,
                          searchTerms = search_terms,
                          hits = hits)
}
