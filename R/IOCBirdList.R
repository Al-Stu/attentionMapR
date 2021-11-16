#-------------------------------------------------------------------------#
#---------------------------- IOC BIRD LIST  -----------------------------#
#-------------------------------------------------------------------------#

# IOC.CURRENTVER
# this function finds the most recent version of the IOC bird list
# it returns a version number
# requires rvest

# author: alice stuart and matt lewis | date modified: 2020-03-12
# compiled in R version 3.6.3 (2020-02-29) -- "Holding the Windsock" running x86_64-apple-darwin15.6.0

#' Find current version of IOC bird list
#'
#' note: currently broken :'(
#'
#' @return the version
#' @export
IOCCurrentVer <- function(){
  text <- textboxfromurl('https://www.worldbirdnames.org/ioc-lists/master-list-2/','//*[@id="header"]')[2]
  text <- gsub(pattern = '^.*version',x = text, replacement = 'version')
  text <- gsub(pattern = '\\sHome$',x = text, replacement = '')
  text <- unlist(strsplit(text, split = '\\s'))
  return(text)
}

#' Downloads specified version of IOC bird list
#'
#' @param version the version of the list you wish to download, in format e.g.
#' version = '10.1'. If current version is desired, use version = 'current'
#' @param tidy logical, if TRUE, will run \code{\link{IOCformat}} on the downloaded
#' tibble and return a tidied copy
#' @return a tibble containing an unprocessed version of the IOC bird list
#' downloaded
#' @export
IOCDownload <- function(version, tidy){
  res <- httr::GET("https://www.worldbirdnames.org/ioc-lists/master-list-2/")
  doc <- xml2::read_html(httr::content(res, as='text'))
  nodes <- rvest::html_nodes(doc, ".post-bodycopy")
  node_text <- unlist(as.character(nodes[[1]]))
  links <- as.character(stringr::str_extract_all(string = node_text, pattern = 'href=["].*["]'))
  links <- strsplit(links,',')
  links <- links[[1]][grepl(pattern = 'vs_other', x = links[[1]])]
  links <- gsub(pattern = '^.*\\\"https:', replacement = 'https:', x = links)
  links <- gsub(pattern = 'xlsx.*$', replacement = 'xlsx', x = links)
  if(version == 'current'){
    correct_link <- links[1]
  } else {
    correct_link <- links[grepl(pattern = version,x=links)]
  }
  httr::GET(correct_link, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
  df <- readxl::read_excel(tf, 1L) %>%
    tidyr::as_tibble()
  if(tidy){
    df <- IOCFormat(df)
  }
  return(df)
}

#' Formats IOC bird list into tidy data
#'
#' @param IOC_sheet IOC bird list tibble, downloaded using
#' \code{\link{IOCDownload}}
#' @return a tibble containing the tidies IOC bird list
#' @export
IOCFormat <- function(IOC_sheet){
  columns <- colnames(IOC_sheet)
  discard_cols <- grepl(pattern = 'rank|notes|group|family|volume|red list category',
                        x = columns,
                        ignore.case = TRUE)
  taxonomies <- IOC_sheet[,!discard_cols]
  av_string_length <- c(100)
  for(i in 2:ncol(taxonomies)){
    temp_names <- na.omit(taxonomies[,i])
    temp_length <- NULL
    temp_length <- apply(temp_names,1,nchar)
    av_string_length[i] <- mean(temp_length)
  }
  keep_cols <- av_string_length >=3
  taxonomies <- taxonomies[,keep_cols]
  colnames(taxonomies)[1] <- 'sequence'
  columns <- colnames(taxonomies)
  IUCN_column <- grep(pattern = 'Handbook of the Birds of the World and BirdLife International digital checklist of the birds of the world', x = columns, ignore.case = TRUE)
  taxonomies <- taxonomies[,c(1,IUCN_column,c(2:(IUCN_column-1)),c((IUCN_column+1):ncol(taxonomies)))]
  taxonomies <- tidyr::fill(taxonomies,2)
  return(taxonomies)
}

#' select target spp. from IOC birdlist and add IDs
#'
#' @param formatted_IOC tibble created by \code{\link{IOCDownload}} when
#' \code{tidy = TRUE}
#' @param cleaned_redlist_data cleaned redlist data output by
#' \code{\link{tidyRedlist::tidySpeciesData}}
#' @return formatted_IOC for species in cleaned_redlist_data with
#' internalTaxonId and scientificName columns
#' @export
IOCSpeciesNames <- function(formatted_IOC, cleaned_redlist_data){
  names(formatted_IOC)[2] <- 'scientificName'
  alternate_taxonomy <- dplyr::right_join(formatted_IOC,
                                          cleaned_redlist_data[['assessments']][,c(1:2)])
  return(alternate_taxonomy)
}

#' Find species alternate names
#'
#' @param formatted_IOC tibble created by \code{\link{IOCDownload}} when
#' \code{tidy = TRUE}
#' @param cleaned_redlist_data cleaned redlist data output by
#' \code{\link{tidyRedlist::tidySpeciesData}}
#' @return a tibble with columns \code{internalTaxonId}, \code{scientificName},
#' \code{name}, \code{language}, \code{main} and \code{source}. Effectivedly
#' \code{formatted_IOC} pivotted longer.
#' @export
IOCAltnames <- function(formatted_IOC, cleaned_redlist_data){
  # select only spp. included in Red List dataset and add internalTaxonId
  names(formatted_IOC)[2] <- 'scientificName'
  alternate_taxonomy <- dplyr::right_join(formatted_IOC,
                                          cleaned_redlist_data[['assessments']][,c(1:2)])
  # combine names into one column and add to species_data
  all_scientific_names <- cleaned_redlist_data$names
  for(i in 3:ncol(formatted_IOC)){
    alt_names <- alternate_taxonomy[,i] %>%
      as.data.frame()
    names(alt_names) <- 'name'
    temp_alternate_name <- tidyr::tibble(internalTaxonId = alternate_taxonomy$internalTaxonId,
                                         scientificName = alternate_taxonomy$scientificName,
                                         name = alt_names$name,
                                         language = 'scientific',
                                         main = 'FALSE',
                                         source = 'IOC')
    all_scientific_names <- dplyr::bind_rows(all_scientific_names, temp_alternate_name)
  }
  all_scientific_names <- unique(all_scientific_names[!is.na(all_scientific_names$name),]) %>%
    .[rev(order(.$main)), ] %>%
    dplyr::arrange(`internalTaxonId`) %>%
    removeDuplicatedNames()
  return(all_scientific_names)
}

#' Remove duplicated names
#'
#' Removes duplicated names within a species irrespective of whether source
#' is different, will preferentially remove from sources where
#' \code{main} = FALSE
#'
#' @param names tibble or df with at least the columns \code{internalTaxonId},
#' \code{name} and \code{main}
#' @return a tibble with columns \code{internalTaxonId}, \code{scientificName},
#' \code{name}, \code{main} and \code{source}. Effectivedly
#' \code{formatted_IOC} pivotted longer.
#' @export
removeDuplicatedNames <- function(names){
  duplicated_rows <- duplicated(names[c('internalTaxonId','name', 'source')])
  dup_row_check <- names[duplicated_rows,]
  if(all(dup_row_check$main == 'FALSE')){
    deduplicated_names <- names[!duplicated_rows,]
  } else {
    stop('some of the names removed were main names')
  }
  if(nrow(deduplicated_names) != nrow(unique(names[ , c('name', 'internalTaxonId', 'source')]))){
    stop('names which were not duplicates were removed')
  }
  return(deduplicated_names)
}

#' adds IOC names to existing names tibble
#'
#' @param species_data IUCN Redlist data cleaned with tidyRedlist
#' @export
getIOCNames <- function(species_data){
  IOC_names <- IOCAltnames(formatted_IOC = formatted_IOC,
                           cleaned_redlist_data = species_data) %>%
    fixNamesWithBrackets()
  species_data[['names']] <- dplyr::bind_rows(species_data[['names']] %>%
                                                tidyr::as_tibble(),
                                              IOC_names %>%
                                                tidyr::as_tibble() %>%
                                                dplyr::mutate(main = as.character(main))) %>%
    dplyr::filter(!is.na(`name`)) %>%
    dplyr::arrange(`internalTaxonId`)
  return(species_data)
}

#' splits names with brackets into two names
#'
#' if the name is A (b) c, will return the names A c and A b c, then add these
#' to the overall names list and remove duplicates
#'
#' @param names a names dataframe
#' @export
fixNamesWithBrackets <- function(names){
  names_with_brackets <- names[grepl(pattern = '[(]', x = names$name), ]
  bracket_name_removed <- dplyr::mutate(names_with_brackets,
                                    name = `name` %>%
                                      gsub(pattern = '[(][^(]+[)] ', replacement = '', x = .))
  brackets_removed <- dplyr::mutate(names_with_brackets,
                                    name = `name` %>%
                                      gsub(pattern = '[(]|[)]', replacement = '', x = .))
  names <- names[!grepl(pattern = '[(]', x = names$name), ]
  all_names <- dplyr::bind_rows(names, bracket_name_removed, brackets_removed) %>%
    unique()
  return(all_names)
}

fixNamesWithQmark <- function(names){
  sci_names <- names %>%
    dplyr::mutate(rowNumber = c(1:nrow(.))) %>%
    dplyr::filter(`language` == 'scientific',
                  grepl(pattern = '[?]', x = `name`)) %>%
    dplyr::mutate(genusName = tidyRedlist::strSplitSelect(`scientificName`, ' ', 1),
                  sppName = gsub('[?] ', '', `name`),
                  name = paste0(`genusName`, ' ', `sppName`))
  fixed_names <- names %>%
    tidyr::as_tibble() %>%
    dplyr::mutate(name = as.character(`name`))
  fixed_names[sci_names$rowNumber, 'name'] <- sci_names[ ,'name']
  fixed_names <- unique(fixed_names)
  return(fixed_names)
}
