#-------------------------------------------------------------------------#
#-------------------------- CREATE SEARCH TERMS --------------------------#
#-------------------------------------------------------------------------#

#' Creates a Web of Science search term from common and scientific names
#'
#' @param names tibble of names you want to change into search terms, created
#' using one of the format functions
#' @param name_types_to_include vector of the name types you want to include in
#' the search terms
#' @param remove_overlapping_names logical, defaults to F, if true any names
#' that are not a main name and are contained within the names of another
#' species are removed
#' @param include_sci_name_abbreviation logical, defaults to F, if true the
#' abbreviation for each scientific name will be included
#' @return a tibble with columns \code{ID} and one column for each site's search
#' term
#' @import magrittr
#' @export
createSearchTerms <- function(names,
                              name_types_to_include = 'all',
                              remove_unnecessary_terms = T,
                              remove_overlapping_names = T,
                              include_sci_name_abbreviation = F
                              ){

  names_to_search <- appropriateNames(names,
                                        name_types_to_include = name_types_to_include,
                                        remove_unnecessary_terms = remove_unnecessary_terms,
                                        remove_overlapping_names = remove_overlapping_names,
                                        include_sci_name_abbreviation = include_sci_name_abbreviation
                                        )
}

#' Abbreviates scientific names
#'
#' @param sci_names a vector of scientific names to be abbreviated
#' @param ids a vector the same length as \code{sci_names} containing the
#' internalTaxonId for each sci_name
#' @return a dataframe with all the abbreviations for each sci name matched
#' to the correct id
#' @import stringr
#'
abbrevSciName <- function(names){
  sci_names_with_ID <- filter(names,
                              name_type == 'scientific'
                              ) %>%
    select(ID, name)
  scinames <- str_split(sci_names_with_ID$name,pattern = ' ')
  first_letter <- substr(x = sapply(scinames, function(x) x[1]),start = 1,stop = 1)
  second_word <- sapply(scinames, function(x) x[2])
  third_word <- sapply(scinames, function(x) x[3])
  fourth_word <- sapply(scinames, function(x) x[4])
  end <- paste(second_word, third_word, fourth_word, sep = ' ') %>%
    gsub(pattern = 'NA', replacement = '') %>%
    str_squish()
  abbrev_sciname <- paste(first_letter, end, sep= ' ')
  all_names <- bind_rows(names,
                         tidyr::tibble(ID = sci_names_with_ID$ID,
                          name = abbrev_sciname,
                          name_type = 'scientific',
                          language = as.character(NA),
                          main = F
                          )
                         ) %>%
    originalIDOrder(names = names)

  return(all_names)
}

#' Order by original ID order
#'
#' @param table_to_order the table/tibble that needs to be reordered
#' @param names the original tibble of names for each thing
#' @return the data from \code{table_to_order} reordered such that the IDs are in the
#' same order as in \code{names}
#'
originalIDOrder <- function(table_to_order, names){
  id_order <- tibble(ID = names$ID,
                     order = as.numeric(factor(names$ID,
                                               levels = unique(names$ID)
                                               )
                                        )
                     ) %>%
    unique()

  ordered_table <- table_to_order %>%
    left_join(id_order) %>%
    .[order(.$order), ] %>%
    select(!order)

  row.names(ordered_table) <- 1:nrow(ordered_table)

  return(ordered_table)
}

#' find names that contained at least one other name and return the number of
#' the name they contain
#' @param names tibble of all the names for each item
#' @return logical vector where true means that name is contained within at
#' least one other name
#' @importFrom tidyr separate_rows
#'
overlappingNames <- function(names){
  species_adist <- adist(x = names$name,
                         partial = TRUE,
                         ignore.case = T
                         )

  overlapping_names <- tibble(name_number = 1:ncol(species_adist),
                              overlaps = apply(species_adist,
                                               FUN = function(X) length(which(X == 0)) > 1,
                                               MARGIN = 2
                                               ),
                              overlaps_with = apply(species_adist,
                                                    FUN = function(X) paste(which(X == 0),
                                                                            collapse = ','
                                                                            ),
                                                    MARGIN = 2
                                                    )
                              ) %>%
    rowwise() %>%
    mutate(overlaps_with = gsub(x = overlaps_with,
                                pattern = name_number,
                                replacement = ''
                                ) %>%
             gsub(pattern = '^,|,$',
                  replacement = '')
           ) %>%
    ungroup() %>%
    separate_rows(overlaps_with, sep = ',') %>%
    rowwise() %>%
    transmute(name_number = name_number,
              ID = names$ID[name_number],
              name = names$name[name_number],
              contains_another_name = overlaps,
              contained_names = names$name[as.numeric(overlaps_with)],
              contained_names_IDs = names$ID[as.numeric(overlaps_with)]
              ) %>%
    ungroup() %>%
    group_by(ID, name, name_number) %>%
    summarise(contains_another_name = unique(contains_another_name),
           contained_names = list(contained_names),
           contained_names_IDs = list(contained_names_IDs),
           contains_another_name_for_same_species = ifelse(is.na(contained_names_IDs),
                                                           F,
                                                           any(contained_names_IDs == ID)
                                                           )
           ) %>%
    .[order(.$name_number), ] %>%
    select(!name_number)

  return(overlapping_names)
}

#' Creates a Google search term from common and scientific names
#'
#' @param names tibble with at least cols \code{ID}, \code{name}
#' @return a tibble with columns \code{internalTaxonId} and \code{google}
#' which contains google search terms
#' @export
googleSearchTerms <- function(names_to_search){
  search_terms <- tibble(ID = character())
  IDs <- unique(names_to_search$ID)
  for(i in 1:length(IDs)){
    positive_search <- names_to_search$name[names_to_search$ID == IDs[i]]
    negative_search <- filter(names_to_search, ID == IDs[i]) %>%
      select(contained_names, contained_names_IDs) %>%
      unnest()

    word_count <- ngram::wordcount(c(positive_search, negative_search))
    if(length(negative_search) > 0){
      species_search_term <- tidyr::tibble(internalTaxonId = overlap_checked_names$internalTaxonId[i],
                                           positiveTerms = list(positive_search),
                                           negativeTerms = list(negative_search),
                                           google = paste0('&q="',
                                                           paste(stringr::str_replace_all(positive_search,
                                                                                          ' ',
                                                                                          '+'),
                                                                 collapse = '"+OR+"'),
                                                           '"+-"',
                                                           paste(stringr::str_replace_all(negative_search,
                                                                                          ' ',
                                                                                          '+'),
                                                                 collapse = '"+-"'),
                                                           '"'
                                           )
      )
    } else{
      species_search_term <- tidyr::tibble(internalTaxonId = overlap_checked_names$internalTaxonId[i],
                                           positiveTerms = list(positive_search),
                                           negativeTerms = list(negative_search),
                                           google = paste0('&q="',
                                                           paste(stringr::str_replace_all(positive_search,
                                                                                          ' ',
                                                                                          '+'),
                                                                 collapse = '"+OR+"'),
                                                           '"'
                                           )
      )
    }

    if(word_count > 32){
      species_search_term$google <- NA
    }
    search_terms <- dplyr::bind_rows(search_terms, species_search_term)
  }
  return(search_terms)
}
