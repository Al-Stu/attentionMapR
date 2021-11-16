#-------------------------------------------------------------------------#
#------------------------ REDLIST NAMES TO INPUT -------------------------#
#-------------------------------------------------------------------------#

#' Formats IUCN Red List download directory into format required for
#' creating search terms
#'
#' @param iucn_wd the working directory for IUCN download data
#' @return a tibble with columns: \code{ID}, the unique identifier for the
#' species (in this case the main scientific name); \code{name}, the names of
#' the species; \code{name_type}, the type of name (in this case scientific or
#' common); \code{language}, the language of the name; and \code{main}, logical,
#' whether the name is to be used as the main name for the species
#' @import dplyr
#'
#' @export
#'
formatIUCNDirectory <- function(iucn_wd){
  files <- getIUCNFiles(iucn_wd)
  names <- formatIUCN(iucn_synonyms = files$synonyms,
                      iucn_common_names = files$common_names,
                      iucn_simple_summary = files$simple_summary
                      )

  return(names)
}

#' Formats names data from IUCN Red List download into format required for
#' creating search terms
#'
#' @param iucn_synonyms synonyms csv downloaded from IUCN
#' @param iucn_common_names common names csv downloaded from IUCN
#' @param iucn_simple_summary simple summary csv downloaded from IUCN
#' @return a tibble with columns: \code{ID}, the unique identifier for the
#' species (in this case the main scientific name); \code{name}, the names of
#' the species; \code{name_type}, the type of name (in this case scientific or
#' common); \code{language}, the language of the name; and \code{main}, logical,
#' whether the name is to be used as the main name for the species
#' @import dplyr
#'
#' @export
#'
formatIUCN <- function(iucn_synonyms, iucn_common_names, iucn_simple_summary){
  ID_name_and_taxon_id <- dplyr::select(iucn_simple_summary,
                                     internalTaxonId,
                                     genusName,
                                     speciesName
                                     ) %>%
    dplyr::transmute(internalTaxonId = internalTaxonId,
                     ID = paste0(genusName, ' ', speciesName)
                     )

  all_names <- bind_rows(formatIUCNSynonyms(ID_name_and_taxon_id,
                                        iucn_synonyms
                                        ),
                     formatIUCNCommonNames(ID_name_and_taxon_id,
                                           iucn_common_names
                                           )
                     )

  all_names_ordered <- left_join(ID_name_and_taxon_id, all_names) %>%
    select(!internalTaxonId)

  return(all_names_ordered)
}

#' Get files from IUCN working directory
#'
#' @param iucn_wd the working directory containing iucn download data
#' @return a list with simple summary, common names and synonyms
#' @import dplyr
#' @importFrom readr read_csv
#'

getIUCNFiles <- function(iucn_wd){
  list(simple_summary = read_csv(paste0(iucn_wd, '/simple_summary.csv')),
       common_names = read_csv(paste0(iucn_wd, '/common_names.csv')),
       synonyms = read_csv(paste0(iucn_wd, '/synonyms.csv'))
       )
}

#' Formats synonyms data from IUCN Red List download into format required for
#' creating search terms
#'
#' @param ID_name_and_taxon_id the name being used as an ID and IUCN internal
#' taxon ID
#' @param iucn_synonyms synonyms csv downloaded from IUCN
#' @return a tibble with columns: \code{ID}, the unique identifier for the
#' species (in this case the main scientific name); \code{name}, the names of
#' the species; \code{name_type}, the type of name (in this case scientific or
#' common); \code{language}, the language of the name; and \code{main}, logical,
#' whether the name is to be used as the main name for the species
#' @import stringi
#' @export
#'
formatIUCNSynonyms <- function(ID_name_and_taxon_id, iucn_synonyms){
  # make it so the name used for ID is the main scientific name
  ID_names <- formatID_name_and_taxon_id(ID_name_and_taxon_id)

  # pull out names from free text column
  scientific_names_from_text <- full_join(ID_name_and_taxon_id,
                                          iucn_synonyms
                                          ) %>%
    transmute(internalTaxonId = internalTaxonId,
              ID = ID,
              name = stri_extract(name,
                                           regex = '^\\p{Lu}[a-z .]+'
                                           ) %>%
                stri_replace_all(regex = '[ ]$', ''),
              name_type = 'scientific',
              language = as.character(NA),
              main = F
              ) %>%
    filter(!is.na(name)) %>%
    bind_rows(filter(., grepl(pattern = 'ssp', x = name)) %>%
                mutate(name = gsub(x = name,
                                   pattern = ' ssp[.] ',
                                   replacement = ' '
                                   )
                       )
              )

  # pull out names from the genus and species names columns
  scientific_names_from_columns <- full_join(ID_name_and_taxon_id,
                                             iucn_synonyms
                                             ) %>%
    transmute(internalTaxonId = internalTaxonId,
              ID = ID,
              name = paste0(genusName, ' ', speciesName),
              name_type = 'scientific',
              language = as.character(NA),
              main = F
              ) %>%
    filter(!is.na(name) & name != 'NA NA')

  # bind into one tibble
  scientific_names <- bind_rows(ID_names,
                                scientific_names_from_text,
                                scientific_names_from_columns
                                )

  # reorder into original order and remove internal taxon id
  scientific_names <- left_join(ID_name_and_taxon_id,
                                scientific_names
                                ) %>%
    .[!duplicated(.$name), ] %>%
    select(!internalTaxonId)

  # return this
  return(scientific_names)
}

#' Formats common names data from IUCN Red List download into format required for
#' creating search terms
#'
#' @param ID_name_and_taxon_id the name being used as an ID and IUCN internal
#' taxon ID
#' @param iucn_common_names synonyms csv downloaded from IUCN
#' @return a tibble with columns: \code{ID}, the unique identifier for the
#' species (in this case the main scientific name); \code{name}, the names of
#' the species; \code{name_type}, the type of name (in this case scientific or
#' common); \code{language}, the language of the name; and \code{main}, logical,
#' whether the name is to be used as the main name for the species
#' @export
#' @importFrom tm removePunctuation
#'
formatIUCNCommonNames <- function(ID_name_and_taxon_id, iucn_common_names){
  common_names <- full_join(ID_name_and_taxon_id, iucn_common_names) %>%
    transmute(ID = ID,
           name = tolower(name) %>%
             gsub(pattern = '-', replacement = ' ') %>%
             removePunctuation(),
           name_type = 'common',
           language = language,
           main = main == 'true'
           ) %>%
    group_by(ID, name, name_type, language) %>%
    summarise(main = any(main)) %>%
    group_by(ID, name, name_type) %>%
    summarise(language = paste(language, collapse = '; '),
              main = any(main)
              ) %>%
    group_by(ID, language) %>%
    mutate(main = ifelse(length(unique(name)) == 1,
                         T,
                         main
                         )
           ) %>%
    ungroup()

  return(common_names)
}

#' Formats ID_name_and_taxon_id into names tibble
#'
#' @param ID_name_and_taxon_id the name being used as an ID and IUCN internal
#' taxon ID
#' @return a tibble with columns: \code{ID}, the unique identifier for the
#' species (in this case the main scientific name); \code{name}, the names of
#' the species; \code{name_type}, the type of name (in this case scientific or
#' common); \code{language}, the language of the name; and \code{main}, logical,
#' whether the name is to be used as the main name for the species
#'
formatID_name_and_taxon_id <- function(ID_name_and_taxon_id){
  ID_name_and_taxon_id %>%
    mutate(ID = ID,
           name = ID,
           name_type = 'scientific',
           language = as.character(NA),
           main = T
           )
}
