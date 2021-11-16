
scrapeItAll <- function(species_data_directory, directory, WoS_url){
  RL_data <- tidyRedlist::importList(species_data_directory,'','csv') %>%
    tidyRedlist::tidySpeciesData() %>%
    getIOCNames()
  species_data_plus_urls <- RL_data %>%
    getEOLUrls()
  species_data_urls_deduplicated <- EOLDeduplicate(species_data_plus_urls)
  species_data_plus_names <- getEOLNames(species_data_urls_deduplicated)
  species_data_plus_names[['names']] <- species_data_plus_names[['names']] %>%
    fixNamesWithBrackets() %>%
    fixNamesWithQmark()
  species_data_plus_search_terms  <- getWoSSearchTerms(species_data_plus_names)
  species_data_plus_WoS <- species_data_plus_search_terms
  species_data_plus_WoS[['WoS_results']] <- WoSSearch(search_terms = species_data_plus_WoS[['WoS_search_terms']]$WoS,
                                            search_ids = species_data_plus_WoS[['WoS_search_terms']]$internalTaxonId,
                                            directory = directory,
                                            WoS_url = WoS_url)
  species_data_plus_wiki <- getWiki(species_data_plus_names)
}
