% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wikiScrapeR.R
\name{wikiScrapeR}
\alias{wikiScrapeR}
\title{scrape all text for a vector of species from wikipedia}
\usage{
wikiScrapeR(
  wiki_searches,
  language = "en",
  browser = c("chrome", "firefox", "phantomjs", "internet explorer")
)
}
\arguments{
\item{wiki_searches}{a tibble with cols \code{ID} (a unique identifier
for each  species) and \code{search} (the term that will be searched in
wikipedia to find the page)}

\item{language}{the language code of wikipedia page to search, defaults
to English. A full list of codes used can be found in the
wiki_language_codes table in this package (a copy of the table from
https://en.wikipedia.org/wiki/List_of_Wikipedias)}

\item{browser}{the browser to be used by RSelenium, one of 'chrome',
'firefox', 'phantomjs', 'internet explorer'}
}
\value{
a list containing one tibble with the name of each species'
wikipedia article and another the text from each section
}
\description{
scrape all text for a vector of species from wikipedia
}
