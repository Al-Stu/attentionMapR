#-------------------------------------------------------------------------#
#-------------------------- OVERLAP CHECK NAMES --------------------------#
#-------------------------------------------------------------------------#

#' checks names for overlap which might result in the search for one species
#' returning another
#'
#' @param names the tibble of names etc.
#' @return names with three extra columns \code{contained_in} a list
#' of any names that contain the name in the row, \code{contains} a list of all
#' the names the name in the row contains, and \code{recommendation} with keep
#' (names with no overlap), possibly unnecessary (contains another name for the)
#' same species, or warn (contained within the name of another species)
#' @importFrom tidyr unnest
overlapCheck <- function(names){
  # find overlapping names
  overlap_checked_names <- overlappingNames(names) %>%
    bind_cols(tibble(name_ID = c(1:nrow(names))))

  names_contained_in_another <- overlap_checked_names %>%
    ungroup() %>%
    select(name, contained_names, contained_names_IDs) %>%
    unnest(c('contained_names', 'contained_names_IDs')) %>%
    filter(!is.na(contained_names)) %>%
    rename(ID = contained_names_IDs,
           contained_in = name,
           name = contained_names
           ) %>%
    select(ID, name, contained_in) %>%
    left_join(select(overlap_checked_names,
                     contained_in_IDs = ID,
                     contained_in = name
                     )
              )

  all_overlap_variables <- overlap_checked_names %>%
    unnest(cols = c(contained_names, contained_names_IDs)) %>%
    left_join(names_contained_in_another) %>%
    unique() %>%
    mutate(contained_in_name_for_different_species = contained_in_IDs != ID,
           contained_in = paste(contained_in_IDs, contained_in, sep = '; '),
           contained_names = paste(contained_names_IDs, contained_names, sep = '; ')) %>%
    group_by(ID, name, name_ID) %>%
    summarise(across(c('contained_names',
                       'contained_in',
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
    ungroup() %>%
    .[order(.$name_ID), ] %>%
    select(!name_ID) %>%
    mutate(might_return_results_for_another_species = ifelse(is.na(might_return_results_for_another_species),
                                                             F,
                                                             might_return_results_for_another_species
                                                             )
           )

  return(all_overlap_variables)
}
