#-------------------------------------------------------------------------#
#------------------------ EXAMPLE WORKFLOW WIKI --------------------------#
#-------------------------------------------------------------------------#

# ## read in data
# simple_summary <- read_csv(file.choose())
#
# ## pull out wikipedia names and IDs (in this case the ID will be the scientific
# ## name as I don't like using the IUCN internalTaxonId) search has to have
# ## spaces replaced with underscores
#
# wiki_searches <- tibble(ID = paste(simple_summary$genusName,
#                                    simple_summary$speciesName,
#                                    sep = ' '
#                                    ),
#                         search = paste(simple_summary$genusName,
#                                        simple_summary$speciesName,
#                                        sep = '_'
#                                        )
#                         )
#
# ## scrape wikipedia text and urls
# wikipedia_page_data <- wikiScrapeR(wiki_searches, browser = 'firefox')
