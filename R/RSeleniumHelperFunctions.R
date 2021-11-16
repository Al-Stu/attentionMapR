#-------------------------------------------------------------------------#
#---------------------- RSELENIUM HELPER FUNCTIONS -----------------------#
#-------------------------------------------------------------------------#

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

#' Find an element and get text
#'
#' @param remDr the remote driver
#' @param using the type of path to use
#' @param path the path to use
#' @return a value containing the text from the element
#'
getElementText <- function(remDr, using, path){
  page <- remDr$findElement(using, path)
  text  <- page$getElementText() %>%
    unlist()
  return(text)
}
