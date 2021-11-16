#-------------------------------------------------------------------------#
#---------------------------- DISCARDED CODE -----------------------------#
#-------------------------------------------------------------------------#

# selects the appropriate names to be made into search terms
#
# @param names tibble of names you want to change into search terms, created
# using one of the format functions
# @param name_types_to_include vector of the name types you want to include in
# the search terms
# @param remove_overlapping_names logical, defaults to F, if true any names
# that are not a main name and are contained within the names of another
# species are removed
# @param include_sci_name_abbreviation logical, defaults to F, if true the
# abbreviation for each scientific name will be included
#
# @returns names but with only the correct names to include in search term
# @importFrom tidyr unnest
# appropriateNames <- function(names,
#                              name_types_to_include,
#                              remove_unnecessary_terms = T,
#                              remove_overlapping_names = F,
#                              include_sci_name_abbreviation = F
# ){
#   # set up to include all names that need to be in the search term
#   if(include_sci_name_abbreviation){
#     names_abbreviated_if_needed <- abbrevSciName(names)
#   } else{
#     names_abbreviated_if_needed <- names
#   }
#
#   if(name_types_to_include != 'all'){
#     names_to_use <- filter(names_abbreviated_if_needed,
#                            name_type %in% name_types_to_include
#     )
#   } else{
#     names_to_use <- names_abbreviated_if_needed
#   }
#
#
#   # only run overlap check if needed as it takes a while
#   if(remove_unnecessary_terms | remove_overlapping_names){
#     overlap_check <- overlappingNames(names_to_use)
#     # remove names that contain another name for the same species
#     if(remove_overlapping_names){
#       contained_names <- select(overlap_check,
#                                 ID = contained_names_IDs,
#                                 name = contained_names
#       ) %>%
#         filter(!is.na(ID)) %>%
#         unnest(cols = c(ID, name)) %>%
#         left_join(select(names,
#                          ID,
#                          name,
#                          main
#         )
#         ) %>%
#         unique() %>%
#         mutate(remove_or_warn = ifelse(main,
#                                        'warn',
#                                        'remove'
#         )
#         )
#
#       names_without_overlaps <- left_join(names_without_unecessary,
#                                           contained_names
#       ) %>%
#         unique() %>%
#         mutate(remove_or_warn = ifelse(is.na(remove_or_warn),
#                                        'keep',
#                                        remove_or_warn
#         )
#         ) %>%
#         filter(remove_or_warn != 'remove')
#
#       if(any(names_without_overlaps$remove_or_warn == 'warn')){
#         warning_names <- names_without_overlaps %>%
#           filter(remove_or_warn == 'warn') %>%
#           select(ID, name) %>%
#           transmute(line = paste0(ID, ': ', name)%>%
#                       paste(collapse = '\n')
#           ) %>%
#           .[1,1] %>%
#           unlist()
#
#         warning(paste0('Some of the search terms contained within other search terms could not be removed as they are main names, it might be worth looking at these manually, these are:\n\n',
#                        warning_names,
#                        '\n'
#         )
#         )
#       }
#
#       return(names_without_overlaps %>%
#                select(c(colnames(names), 'contained_names', 'contained_names_IDs'))
#       )
#     } else{
#       names_without_unecessary <- left_join(names, overlap_check) %>%
#         filter(!contains_another_name_for_same_species) %>%
#         select(c(colnames(names), 'contained_names', 'contained_names_IDs'))
#     }
#   } else{
#     names_without_unecessary <- names_to_use
#   }
#
#   return(names_without_unecessary)
# }
