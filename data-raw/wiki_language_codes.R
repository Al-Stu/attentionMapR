## code to prepare `wiki_language_codes` dataset goes here
wiki_codes_url <- 'https://en.wikipedia.org/wiki/List_of_Wikipedias'

wiki_language_codes <- wiki_codes_url %>%
  rvest::read_html() %>%
  rvest::html_nodes(xpath='/html/body/div[3]/div[3]/div[5]/div[1]/table[2]') %>%
  rvest::html_table() %>%
  .[[1]] %>%
  dplyr::select(!Logo)

usethis::use_data(wiki_language_codes, overwrite = TRUE)
