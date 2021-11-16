#-------------------------------------------------------------------------#
#-------------------------- OVERLAP CHECK NAMES --------------------------#
#-------------------------------------------------------------------------#

#' checks names for overlap which might result in the search for one species
#' returning another
#'
#' @param names the tidyr::tibble of names etc.
#' @return names with three extra columns \code{contained_in} a list
#' of any names that contain the name in the row, \code{contains} a list of all
#' the names the name in the row contains, and \code{recommendation} with keep
#' (names with no overlap), possibly unnecessary (contains another name for the)
#' same species, or warn (contained within the name of another species)
overlapCheck <- function(names){
  # find overlapping names
  overlap_checked_names <- overlappingNames(names) %>%
    dplyr::bind_cols(tidyr::tibble(name_ID = c(1:nrow(names))))

  names_contained_in_another <- overlap_checked_names %>%
    dplyr::ungroup() %>%
    dplyr::select(name, contained_names, contained_names_IDs) %>%
    tidyr::unnest(c('contained_names', 'contained_names_IDs')) %>%
    dplyr::filter(!is.na(contained_names)) %>%
    dplyr::rename(ID = contained_names_IDs,
           contained_in = name,
           name = contained_names
           ) %>%
    dplyr::select(ID, name, contained_in) %>%
    dplyr::left_join(dplyr::select(overlap_checked_names,
                     contained_in_IDs = ID,
                     contained_in = name
                     )
              )

  all_overlap_variables <- overlap_checked_names %>%
    tidyr::unnest(cols = c(contained_names, contained_names_IDs)) %>%
    dplyr::left_join(names_contained_in_another) %>%
    unique() %>%
    dplyr::mutate(contained_in_name_for_different_species = contained_in_IDs != ID,
           contained_in = paste(contained_in_IDs, contained_in, sep = '; '),
           contained_names = paste(contained_names_IDs, contained_names, sep = '; ')) %>%
    dplyr::group_by(ID, name, name_ID) %>%
    dplyr::summarise(across(c('contained_names',
                       'contained_in'
                       ),
                     function(X) list(unique(X)) %>%
                       ifelse(test = . == 'NA; NA',
                              list(NA),
                              .
                              )
                     ),
              might_return_results_for_another_species = any(contained_in_name_for_different_species),
              possibly_uneccessary = any(contains_another_name_for_same_species)
              ) %>%
    dplyr::ungroup() %>%
    .[order(.$name_ID), ] %>%
    dplyr::select(!name_ID) %>%
    dplyr::mutate(might_return_results_for_another_species = ifelse(is.na(might_return_results_for_another_species),
                                                             F,
                                                             might_return_results_for_another_species
                                                             )
           )

  return(all_overlap_variables)
}

#' find names that contained at least one other name and return the number of
#' the name they contain
#' @param names tidyr::tibble of all the names for each item
#' @return logical vector where true means that name is contained within at
#' least one other name
#'
overlappingNames <- function(names){
  species_adist <- adist(x = names$name,
                         partial = TRUE,
                         ignore.case = T
  )

  overlapping_names <- tidyr::tibble(name_number = 1:ncol(species_adist),
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
    dplyr::rowwise() %>%
    dplyr::mutate(overlaps_with = gsub(x = overlaps_with,
                                pattern = name_number,
                                replacement = ''
    ) %>%
      gsub(pattern = '^,|,$',
           replacement = '')
    ) %>%
    dplyr::ungroup() %>%
    tidyr::separate_rows(overlaps_with, sep = ',') %>%
    dplyr::rowwise() %>%
    dplyr::transmute(name_number = name_number,
              ID = names$ID[name_number],
              name = names$name[name_number],
              contains_another_name = overlaps,
              contained_names = names$name[as.numeric(overlaps_with)],
              contained_names_IDs = names$ID[as.numeric(overlaps_with)]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(ID, name, name_number) %>%
    dplyr::summarise(contains_another_name = unique(contains_another_name),
              contained_names = list(contained_names),
              contained_names_IDs = list(contained_names_IDs),
              contains_another_name_for_same_species = ifelse(is.na(contained_names_IDs),
                                                              F,
                                                              any(contained_names_IDs == ID)
              )
    ) %>%
    .[order(.$name_number), ] %>%
    dplyr::select(!name_number)

  return(overlapping_names)
}
