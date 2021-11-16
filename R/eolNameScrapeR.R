#-------------------------------------------------------------------------#
#---------------------------- EOL NAMESCRAPER  ----------------------------#
#-------------------------------------------------------------------------#

# author: alice stuart and matt lewis | date modified: 2020-03-11
# compiled in R version 3.6.3 (2020-02-29) -- "Holding the Windsock" running x86_64-apple-darwin15.6.0

#' Scrape scientific names from Encyclopedia of Life
#'
#' \code{EOLSciNamesScrapeR} takes urls for species homepages and scrapes
#' the alternate latin names for each spp
#'
#' @param EOL_urls tibble of EOL urls created by \code{\link{EOLUrlScrapeR}}
#' @return a tibble with columns \code{internalTaxonId}, \code{scientificName},
#' \code{name}, \code{main}, \code{language} and \code{source}.
#' @section Requires: rvest
#' @export
EOLSciNameScrapeR <- function(EOL_urls){
  name_url <- paste(EOL_urls$link,'/names',sep='')
  latin_names_df <- list()
  for(l in 1:length(EOL_urls$link)){
    alt_name_text <- EOLTextBoxFromUrl(urls = name_url[l],
                                       x_path = '/html/body/div[3]/div[2]/div/div[1]/div[2]/div/div[2]')
    if(!is.null(alt_name_text)){
      kept_alt_names<-c()
      for(i in 1:length(alt_name_text)){
        if(!grepl("Synonym|Reference|according", alt_name_text[i])){
          name <- unlist(strsplit(alt_name_text[i],split=" "))
          if(length(name) <= 2){
            name <- paste(name, collapse = ' ')
          } else if(name[3] == tolower(name[3])){
            name <- paste(name[1:3], collapse=" ")
          }else{
            name <- paste(name[1:2], collapse=" ")
          }
          kept_alt_names <- c(kept_alt_names, name)
        }
      }
      kept_sci_names <- c(as.character(EOL_urls$text[l]),kept_alt_names)
      main <- c(TRUE, rep_len(FALSE, (length(kept_sci_names)-1)))
      latin_names_df[[l]] <- tidyr::tibble(name = kept_sci_names,
                                           main =  main) %>%
        unique()
    }  else {
      latin_names_df[[l]] <- tidyr::tibble(name = EOL_urls$text[l],
                                           main = TRUE)
    }
    latin_names_df[[l]] <- dplyr::transmute(latin_names_df[[l]],
                                            internalTaxonId = EOL_urls$internalTaxonId[l],
                                            scientificName = EOL_urls$scientificName[l],
                                            name = `name`,
                                            language = 'scientific',
                                            main = as.logical(`main`),
                                            source = 'EOL')
  }
  latin_names <- dplyr::bind_rows(latin_names_df)
  return(latin_names)
}

#' Scrape English common names from Encyclopedia of Life
#'
#' \code{EOLSciNamesScrapeR} takes urls for species homepages and scrapes
#' the alternate latin names for each spp
#'
#' @param EOL_urls tibble of EOL urls created by \code{\link{EOLUrlScrapeR}}
#' @return a tibble with columns \code{internalTaxonId}, \code{scientificName},
#' \code{name}, \code{main}, \code{language} and \code{source}.
#' @section Requires: rvest
#' @export
EOLCommonNameScrapeR <- function(EOL_urls){
  name_url <- paste(EOL_urls$link,'/names',sep='')
  internalTaxonId <- EOL_urls$internalTaxonId
  scientificName <- EOL_urls$scientificName
  common_names_df <- list()
  for(l in 1:length(EOL_urls$link)){
    page <- xml2::read_html(name_url[l])
    common_name_nodes <- rvest::html_nodes(page,
                                           xpath='/html/body/div[3]/div[2]/div/div[2]/div[1]/div/div[2]/div/div')
    common_name_list <- rvest::html_children(common_name_nodes) %>%
      rvest::html_text() %>%
      as.character() %>%
      as.list()
    names_template <- tidyr::tibble(internalTaxonId = character(),
                                    scientificName = character(),
                                    name = character(),
                                    language = character(),
                                    main = logical(),
                                    source = character())
    if(length(common_name_list)!=0)  {
      for(i in 1:length(common_name_list)){
        names <- unlist(strsplit(common_name_list[[i]][1],split="\n"))
        preferred <- grepl('preferred', names)
        names <- names[names != "" & !grepl("Recognize|recognize|prefer", names)]
        if(i==1){
          common_names_df[[l]] <- names_template
        }
        if(length(names)!=0){
          temp_main <- rep_len(any(preferred),length(names))
          temp_df <- tidyr::tibble(internalTaxonId = internalTaxonId[l],
                                   scientificName = scientificName[l],
                                   name = names,
                                   language = 'English',
                                   main = rep_len(any(preferred),length(names)),
                                   source = 'EOL')
          common_names_df[[l]] <- dplyr::bind_rows(common_names_df[[l]],temp_df)
        }
      }
      common_names_df[[l]] <-unique(common_names_df[[l]])
    } else {
      common_names_df[[l]] <- tidyr::tibble(internalTaxonId = internalTaxonId[l],
                                           scientificName = scientificName[l],
                                           name = NA,
                                           language = NA,
                                           main = NA,
                                           source = NA)
    }
  }
  common_names <- dplyr::bind_rows(common_names_df)
  common_names$name <- stringr::str_trim(as.character(common_names$name))
  return(common_names)
}

# EOLTextBoxFromUrl
# reads a text box from specified url and xpath

EOLTextBoxFromUrl <- function(urls, x_path){
  page <- xml2::read_html(urls)
  # Alternative names
  alt_name_nodes <- rvest::html_nodes(page,xpath = x_path)
  alt_name_text <- unlist(strsplit(rvest::html_text(alt_name_nodes),split="\n"))
  alt_name_text <- alt_name_text[which(alt_name_text != "")]
  return(alt_name_text)
}

getEOLNames <- function(species_data){
  ## EOL_names
  EOL_sci_names <- EOLSciNameScrapeR(EOL_urls = species_data[['EOL_urls']])
  EOL_common_names <- EOLCommonNameScrapeR(EOL_urls = species_data[['EOL_urls']])

  species_data[['names']] <- dplyr::bind_rows(species_data[['names']],
                                              EOL_sci_names,
                                              EOL_common_names) %>%
    dplyr::filter(!is.na(`name`)) %>%
    dplyr::arrange(`internalTaxonId`) %>%
    unique() %>%
    .[ ,c(1:6)]
  na_ids <- species_data[["names"]][is.na(species_data[["names"]]$scientificName), ]$internalTaxonId
  species_data[["names"]][is.na(species_data[["names"]]$scientificName), 2] <-
    species_data[['assessments']]$scientificName[species_data[['assessments']]$internalTaxonId %in%
                                                   na_ids]
  return(species_data)
}
