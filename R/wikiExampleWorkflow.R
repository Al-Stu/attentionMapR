# #-------------------------------------------------------------------------#
# #------------------------ EXAMPLE WORKFLOW WIKI --------------------------#
# #-------------------------------------------------------------------------#
# ##### Author: Alice Stuart
# ##### Date: 17.11.21
# ##### purpose of script:  scrape page views and number of words for wikipedia
# #####                     articles
#
# ## install packages that you're likely to not have, there might be more but
# ## they'll come up as an error message then you can just install them then ####
# install.packages(c('devtools', 'tm', 'stringi'))
# devtools::install_github('Al-Stu/attentionMapR')
#
# # library for attentionMapR (don't need to load the library for other
# # packages as they're called explicitly)
# library(attentionMapR)
#
# ## read in unmodified simple summary from IUCN download data ####
# simple_summary <- readr::read_csv(file.choose())
#
# ## pull out wikipedia names and IDs (in this case the ID will be the scientific
# ## name as I don't like using the IUCN internalTaxonId) search has to have
# ## spaces replaced with underscores ####
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
# ## scrape wikipedia text and urls ####
# wikipedia_page_data <- wikiScrapeR(wiki_searches,
#                                    language = 'en',
#                                    browser = 'firefox'
#                                    )
#
# ## get pageviews for each page ####
# wiki_urls <- dplyr::select(wikipedia_page_data,  ID, url) %>%
#   unique()
#
# # this will give a list, element one is a tibble with the total pageviews
# # across the time period, element two is a list with an entry for each time
# # point
# wiki_page_views <- wikiPageViews(wiki_urls,
#                                  start_year = 2019,
#                                  end_year = 2020,
#                                  granularity = 'monthly'
#                                  )
#
