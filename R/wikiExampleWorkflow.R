# #-------------------------------------------------------------------------#
# #------------------------ EXAMPLE WORKFLOW WIKI --------------------------#
# #-------------------------------------------------------------------------#
#
# ## read in data
# simple_summary <- readr::read_csv(file.choose())
#
# ## pull out wikipedia names and IDs (in this case the ID will be the scientific
# ## name as I don't like using the IUCN internalTaxonId) search has to have
# ## spaces replaced with underscores
#
# wiki_searches <- tidyr::tibble(ID = paste(simple_summary$genusName,
#                                           simple_summary$speciesName,
#                                           sep = ' '
#                                           ),
#                                main_name = paste(simple_summary$genusName,
#                                                  simple_summary$speciesName,
#                                                  sep = ' '
#                                                  )
#                                ) %>%
#   formatNamesToWiki()
#
# ## scrape wikipedia text and urls
# wikipedia_page_data <- wikiScrapeR(wiki_searches,
#                                    language = 'en',
#                                    browser = 'firefox'
#                                    )
#
# ## get pageviews for each page
# wiki_urls <- dplyr::select(wikipedia_page_data,  ID, url) %>%
#   unique()
#
# wiki_page_views <- wikiPageViews(wiki_urls, start_year = 2019, end_year = 2020, granularity = 'monthly')
