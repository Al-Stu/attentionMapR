# #-------------------------------------------------------------------------#
# #-------------------------- CREATE SEARCH TERMS --------------------------#
# #-------------------------------------------------------------------------#
#
# # Creates a Web of Science search term from common and scientific names
# #
# # @param names an output of \code{\link{tidyRedlist::tidySpeciesData}},
# # with (at least) columns \code{internalTaxonId}, \code{name}, and
# # \code{language}
# # @return a tibble with columns \code{internalTaxonId} and \code{Wos} which
# # contains WoS search terms
# # @importFrom tm removePunctuation
# # @export
# WoSSearchTerms <- function(names){
#   search_terms <- tidyr::tibble(ID = unique(names$ID),
#                                 WoS = as.character(NA)
#   )
#   for(i in 1:nrow(search_terms)){
#     to_search <- names$name[names$internalTaxonId ==
#                               search_terms$internalTaxonId[i]] %>%
#       as.character() %>%
#       .[unlist(stri_length(removePunctuation(.)))>2]
#     search_terms$WoS[i] <- proximitySearchTerm(to_search,
#                                                "WOS"
#     )
#   }
#   return(search_terms)
# }
#
# # Creates a set of scopus search terms from common and scientific names
# #
# # @param names an output of \code{\link{tidyRedlist::tidySpeciesData}},
# # with (at least) columns \code{internalTaxonId}, \code{name}, and
# # \code{language}
# # @return a list with an element of scopus search terms split into <256
# # character substrings per species
# # @export
# scopusSearchTerms <- function(names){
#   internalTaxonId <- unique(names$internalTaxonId)
#   to_search <- c()
#   search_terms <- list()
#   for(i in 1:length(internalTaxonId)){
#     to_search[i] <- names$name[names$internalTaxonId == internalTaxonId[i]] %>%
#       as.character() %>%
#       paste({,.,}, sep = , collapse =  OR )
#     or_position <- stringr::str_locate_all(to_search[i],  OR ) %>%
#       .[[1]] %>%
#       .[ ,1]
#     number_boxes <- ceiling((max(or_position)/256))
#     if(number_boxes>1){
#       search_terms[[i]] <- vector(length = number_boxes)
#       split <- 1
#       for(j in 1:(number_boxes-1)){
#         split[j+1] <- or_position[which.min(abs(or_position-(256*j)))-1]
#         search_terms[[i]][j] <- substr(to_search[i], split[j], split[j+1])
#       }
#       search_terms[[i]][number_boxes] <- substr(to_search[i],
#                                                 split[number_boxes],
#                                                 stringr::str_length(to_search[i]))
#     } else {
#       search_terms[[i]] <- to_search[i]
#     }
#   }
#   names(search_terms) <- internalTaxonId
#   return(search_terms)
# }
#
# # Creates a regex search term from common and scientific names
# #
# # @param names an output of \code{\link{tidyRedlist::tidySpeciesData}},
# # with (at least) columns \code{internalTaxonId}, \code{name}, and
# # \code{language}
# # @return a tibble with columns \code{internalTaxonId} and \code{regex_sci}
# # with a regex search term for all scientific names for that species
# # \code{regex_common} with regex search term for all common names and
# # \code{regex_both} with regex search term for all names
# # @export
# regexSearchTerms <- function(names){
#   internalTaxonId <- unique(names$internalTaxonId)
#   search_terms <- tidyr::tibble(internalTaxonId = internalTaxonId,
#                                 regex_common = NA,
#                                 regex_sci = NA,
#                                 regex_both = NA)
#   for(i in 1:length(internalTaxonId)){
#     temp_common <- names$name[names$internalTaxonId == internalTaxonId[i] &
#                                 names$language != scientific] %>%
#       unique() %>%
#       .[!overlappingNames(., ignore.case = F)] %>%
#       c(., tolower(.)) %>%
#       c(., gsub("[-]", " ", .)) %>%
#       c(., tm::removePunctuation(.)) %>%
#       stringr::str_trim() %>%
#       unique() %>%
#       ifelse(test = stringr::str_length(.) <= 3,
#              yes = paste0("[ ]", ., "[. ]"),
#              no = .)
#
#     temp_sci <- names$name[names$internalTaxonId == internalTaxonId[i] &
#                              names$language == scientific] %>%
#       stringr::str_trim() %>%
#       unique() %>%
#       .[!overlappingNames(., ignore.case = F)]
#     abbrev_sci <- abbrevSciName(sci_names = as.character(temp_sci),
#                                 ids = rep_len(internalTaxonId[i],
#                                               length(temp_sci)))$names
#     temp_sci <- c(temp_sci,abbrev_sci)
#     search_terms$regex_sci[i] <- paste(temp_sci[!is.na(temp_sci)], collapse=|) %>% makeBlankNA()
#     search_terms$regex_common[i] <- paste(temp_common[!is.na(temp_common)], collapse=|) %>% makeBlankNA()
#     search_terms$regex_both[i] <- paste(c(temp_common, temp_sci) %>%
#                                           .[!is.na(.)],
#                                         sep = |,
#                                         collapse=|) %>%
#       makeBlankNA()
#   }
#   return(search_terms)
# }
#
# # Abbreviates scientific names
# #
# # @param sci_names a vector of scientific names to be abbreviated
# # @param ids a vector the same length as \code{sci_names} containing the
# # internalTaxonId for each sci_name
# # @return a dataframe with all the abbreviations for each sci name matched
# # to the correct id
# abbrevSciName <- function(sci_names,ids){
#   scinames <- stringr::str_split(sci_names,pattern =  )
#   first_letter <- substr(x = sapply(scinames, function(x) x[1]),start = 1,stop = 1)
#   second_word <- sapply(scinames, function(x) x[2])
#   third_word <- sapply(scinames, function(x) x[3])
#   end <- gsub(pattern =  NA$,replacement = ,x = paste(second_word,third_word,sep =  ))
#   abbrev_sciname <- paste(first_letter,end,sep=  )
#   result <- tidyr::tibble(internalTaxonId = ids,
#                           names = abbrev_sciname)
#   second_letter <- substr(x = second_word,start = 1,stop = 1)
#   double_abbrev_sciname <- paste(first_letter,second_letter,third_word,sep=  ) %>%
#     tidyr::tibble(internalTaxonId = ids,
#                   names = .) %>%
#     .[!grepl(pattern =  NA$,
#              x = .$names),]
#   result <- dplyr::bind_rows(result,double_abbrev_sciname) %>%
#     dplyr::arrange(`internalTaxonId`)
#   return(result)
# }
#
# # proximitySearchTerm
# # uses proximity operators to create a WOS or SCOPUS searchterm
# #
# # @param search_terms vector of terms to be concatenated into the one proximity
# # search term
# # @param WOS_or_SCOPUS
# proximitySearchTerm <- function(search_terms, WOS_or_SCOPUS){
#   split_search <- str_split(search_terms,pattern =  )
#   if(WOS_or_SCOPUS ==WOS){
#     proximity_search <- sapply(X = split_search,FUN = function (x) paste(x,collapse =  NEAR/0 ))
#   } else if (WOS_or_SCOPUS ==SCOPUS){
#     proximity_search <- sapply(X = split_search,FUN = function (x) paste(x,collapse =  pre/0 ))
#   } else {
#     stop("database incorrectly set, please set WOS_or_SCOPUS to WOS for Web
#          of Science or SCOPUS for SCOPUS ")
#   }
#   proximity_search <- paste(proximity_search,collapse =  OR )
#   return(proximity_search)
# }
#
# # Creates a Web of Science search term tibble from species data
# #
# # @param species_data list with red list data
# # @return a tibble with columns \code{internalTaxonId} and \code{Wos} which
# # contains WoS search terms as \code{species_data[[WoS_search_terms]]}
# # @export
# getWoSSearchTerms <- function(species_data, scientific = FALSE){
#   ## create WoS search terms
#   names <- species_data[[names]]
#   if(scientific){
#     names <- dplyr::filter(names, language == scientific)
#   }
#   names_to_search <- names %>%
#     removeRepeatedNames() %>%
#     .[ ,c(1:4)] %>%
#     unique()
#   species_data[[WoS_search_terms]] <- WoSSearchTerms(names_to_search)
#   return(species_data)
# }
#
# # Creates a Google search term from common and scientific names
# #
# # @param names an output of \code{\link{tidyRedlist::tidySpeciesData}},
# # with (at least) columns \code{internalTaxonId}, \code{name}, and
# # \code{language}
# # @return a tibble with columns \code{internalTaxonId} and \code{google}
# # which contains google search terms
# # @export
# googleSearchTerms <- function(names){
#   overlap_checked_names <- findOverlappingNames(names)
#
#   search_terms <- tidyr::tibble(internalTaxonId = character())
#   for(i in 1:nrow(overlap_checked_names)){
#     positive_search <- unlist(overlap_checked_names$positiveSearchTerms[i])
#     negative_search <- unlist(overlap_checked_names$negativeSearchTerms[i])
#
#     word_count <- ngram::wordcount(c(positive_search, negative_search))
#     if(length(negative_search) > 0){
#       species_search_term <- tidyr::tibble(internalTaxonId = overlap_checked_names$internalTaxonId[i],
#                                            positiveTerms = list(positive_search),
#                                            negativeTerms = list(negative_search),
#                                            google = paste0(&q=",
#                                                            paste(stringr::str_replace_all(positive_search,
#                                                                                            ,
#                                                                                           +),
#                                                                  collapse = "+OR+"),
#                                                            "+-",
#                                                            paste(stringr::str_replace_all(negative_search,
#                                                                                            ,
#                                                                                           +),
#                                                                  collapse = "+-"),
#                                                            "
#                                            )
#       )
#     } else{
#       species_search_term <- tidyr::tibble(internalTaxonId = overlap_checked_names$internalTaxonId[i],
#                                            positiveTerms = list(positive_search),
#                                            negativeTerms = list(negative_search),
#                                            google = paste0(&q=",
#                                                            paste(stringr::str_replace_all(positive_search,
#                                                                                            ,
#                                                                                           +),
#                                                                  collapse = "+OR+"),
#                                                            "
#                                            )
#       )
#     }
#
#     if(word_count > 32){
#       species_search_term$google <- NA
#     }
#     search_terms <- dplyr::bind_rows(search_terms, species_search_term)
#   }
#   return(search_terms)
# }
#
# findOverlappingNames <- function(names){
#   unique_species_names <- names %>%
#     dplyr::filter(language == scientific |
#                     language == English) %>%
#     dplyr::select(-dplyr::one_of(language, source)) %>%
#     dplyr::mutate(name = name %>%
#                     gsub(pattern = -, replacement =  ) %>%
#                     tolower()
#     ) %>%
#     .[rev(order(.$main)), ] %>%
#     .[!duplicated(.[ , c(name, internalTaxonId)]), ]
#
#   # find all names that are identical between species and are not a main name,
#   # remove from search
#   species_names_duplicates <- unique_species_names %>%
#     dplyr::mutate(rowNum = c(1:nrow(.))) %>%
#     .[rev(order(.$main)), ] %>%
#     .[duplicated(.$name) | duplicated(.$name, fromLast = TRUE), ] %>%
#     dplyr::filter(main == FALSE)
#
#   unique_species_names <- unique_species_names[!c(1:nrow(unique_species_names)) %in%
#                                                  species_names_duplicates$rowNum, ]
#
#   # find overlapping names within species e.g. bittern and eurasian bittern
#   # and remove longer name
#   internal_taxon_ids <- unique(names$internalTaxonId)
#   species_names_to_search <- tidyr::tibble(name = character())
#   for(i in 1:length(internal_taxon_ids)){
#     species_names <-unique_species_names[unique_species_names$internalTaxonId == internal_taxon_ids[i], ]
#     overlapping_names <- overlappingNames(species_names$name)
#     species_names_to_search <- dplyr::bind_rows(species_names_to_search,
#                                                 tidyr::tibble(internalTaxonId = internal_taxon_ids[i],
#                                                               name = species_names$name[!overlapping_names] %>%
#                                                                 tolower() %>%
#                                                                 tm::removePunctuation(),
#                                                               main = unique_species_names[unique_species_names$internalTaxonId == internal_taxon_ids[i], ] %>%
#                                                                 .[!duplicated(.$name), ] %>%
#                                                                 .[!overlapping_names, ] %>%
#                                                                 .$main
#                                                 )
#     )
#   }
#
#   search_terms <- tidyr::tibble(positiveSearchTerms = list(),
#                                 negativeSearchTerms = list())
#   # find names that overlap between species
#   for(i in 1:length(internal_taxon_ids)){
#     search_terms <- findNegativeTerms(search_terms, species_names_to_search, internal_taxon_ids[i])
#   }
#
#   return(search_terms)
# }
#
# overlappingNames <- function(name, ignore.case = TRUE){
#   species_adist <- adist(x = name,
#                          partial = TRUE,
#                          ignore.case = ignore.case)
#   overlapping_names <- apply(species_adist,
#                              FUN = function(X) length(which(X == 0)) > 1,
#                              MARGIN = 2)
#   return(overlapping_names)
# }
#
# findNegativeTerms <- function(search_terms, species_names_to_search, internal_taxon_id){
#   species_names <- species_names_to_search$name[species_names_to_search$internalTaxonId == internal_taxon_id]
#   names_contained_in <- species_names_to_search$name[species_names_to_search$internalTaxonId != internal_taxon_id] %>%
#     .[grepl(pattern = paste0(species_names,
#                              collapse = |),
#             x = .)
#     ] %>%
#     .[!overlappingNames(.)]
#   species_search_terms <- tidyr::tibble(internalTaxonId = internal_taxon_id,
#                                         positiveSearchTerms = list(species_names),
#                                         negativeSearchTerms = list(names_contained_in)
#   )
#   search_terms <- dplyr::bind_rows(search_terms, species_search_terms)
#   return(search_terms)
# }
#
# # creates a consensed regular expression for scientific names of a species
# simplifySciRegex <- function(scientific_names){
#   sci_names_tibble <- tidyr::tibble(name = scientific_names) %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(split_name = stringr::str_split(name, " "),
#                   genus = split_name[1],
#                   species = split_name[2],
#                   subspecies = split_name[3]) %>%
#     dplyr::group_by(genus, species) %>%
#     dplyr::summarise(species_letter = substr(species, 1, 1),
#                      species_regex = ifelse(test = !any(is.na(subspecies)),
#                                             yes = paste0("((", species_letter,"[.]?|",
#                                                          species, ")[ ](",
#                                                          paste(subspecies, collapse = "|"),
#                                                          "))"
#                                             ),
#                                             no = species
#                      )
#     ) %>%
#     dplyr::ungroup() %>%
#     dplyr::group_by(genus) %>%
#     dplyr::summarise(genus_letter = substr(genus, 1, 1),
#                      genus_after_letter = stringi::stri_extract(genus,
#                                                                 regex = "(?<=[A-Z])[a-z]+"),
#                      genus_regex = paste0("((([", genus_letter, tolower(genus_letter),"](",
#                                           genus_after_letter, "))|", genus_letter, "[.]?)[ ](",
#                                           paste(species_regex, collapse = "|"), "))"
#                      )
#     )
#
#   regex_term <- paste(unique(sci_names_tibble$genus_regex), collapse = |)
#
#   return(regex_term)
# }
#
# makeBlankNA <- function(string){
#   ifelse(string == "",
#          NA,
#          string)
# }
