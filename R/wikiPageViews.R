#-------------------------------------------------------------------------#
#-------------------------- WIKIPEDIA PAGE VIEWS -------------------------#
#-------------------------------------------------------------------------#

#' Wikipedi page views
#'
#' query the wikipedia api to find the number of page views
#'
#' @param wiki_urls tibble with ID and the url of the wikipedia pages being
#' queried
#' @param granularity the temporal granularity you want JSON results to be
#' returned with must be one of 'daily' or 'monthly'
#' @param access the method of access to be counted, must be one of:
#' 'all-access', desktop', 'mobile-app', 'mobile-web'
#' @param agent the type of access to be counted, must be one of:
#' 'all-agents', 'user', 'spider', 'automated'
#' @param start_year the year you want to start from
#' @param start_month the month you want to start from, defaults to January
#' (01)
#' @param start_day the day you want to start from, defaults to the first
#' (01)
#' @param end_year the year you want to end with
#' @param end_month the month you want to end with, defaults to December
#' (12)
#' @param end_day the day you want to end with, defaults to the last
#' day of the month
#'
#' @import jsonlite
#' @importFrom lubridate days_in_month
#' @export
#'
wikiPageViews <- function(wiki_urls,
                          granularity = c('daily', 'monthly'),
                          access = 'all-access',
                          agent = 'user',
                          start_year,
                          start_month = 01,
                          start_day = 01,
                          end_year,
                          end_month = 12,
                          end_day = NULL
                          ){
  # set end day to last day of the month if not set by user
  if(is.null(end_day)){
    end_day <- lastDayOfMonth(end_year, end_month)
  }

  # check the dates are real dates
  checkDate(start_year, start_month, start_day, 'start')
  checkDate(end_year, end_month, end_day, 'end')

  # check access method is correct
  checkAccess(access)

  # create list for all the JSON results
  bird_JSON_list <- list()

  # create tibble for page views
  page_views <- wiki_urls %>%
    mutate(page_views = as.numeric(NA))

  # loop through urls querying JSON API
  for(i in 1:nrow(wiki_urls)){
    wiki_name <- wiki_urls$url[i] %>% gsub('https://[a-z]+[.]wikipedia[.]org/wiki/', '')
    country <- wiki_urls$url[i] %>%
      gsub('^https://|[.]wikipedia[.]org/wiki/.+$', '')
    Sys.sleep(0.01) # because this JSON REST API won't accept more than 200 requests per second (I'm being extra cautious)
    # don't want to bother searching species that don't have a page
    if(!is.na(wiki_name)){
      # make url for API query
      url <- paste0("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
                    country,
                    "wikipedia.org/",
                    access,
                    "/",
                    agent,
                    "/",
                    wiki_name,
                    '/',
                    granularity,
                    '/',
                    paste0(start_year, start_month, start_day),
                    '/',
                    paste0(end_year, end_month, end_day)
                    )
      # make API query
      bird_JSON_list[[wiki_urls$ID[i]]] <- tryCatch(fromJSON(url), error=function(err) NA)
      # add total for year to bird_wikipedia_data if there is one
      page_views$page_views[i] <- ifelse(!is.na(bird_JSON_list[[i]]),
                                         sum(bird_JSON_list[[i]]$items$views),
                                         NA
                                         )
    } else{
      bird_JSON_list[[wiki_urls$ID[i]]] <- NA
    }
  }
  # create list with page views tibble as first element and raw JSON results as second
  result <- list(page_views,
                 bird_JSON_list
                 )

  return(result)
}

#' check dates
#'
#' check whether the given dates are real
#'
#' @param year year
#' @param month month
#' @param day day
#' @param start_or_end whether it's the start date or the end date
#'
checkDate <- function(year, month, day, start_or_end){
  date_check <- paste(year, month, day, sep = '-') %>%
    as.Date('%Y-%m-%d')

  if(is.na(date_check)){
    stop(paste0('The ', start_or_end, ' day, month and year given do not produce a real date, please check.'))
  }
}

#' find end day
#'
#' finds last day of the month
#'
#' @param end_year the end year to be considered
#' @param end_month the end month to be considered
#'
#'
lastDayOfMonth <- function(end_year, end_month){
  paste(end_year, end_month, 01, sep = '-') %>%
    as.Date('%Y-%m-%d') %>%
    days_in_month()
}

#' check access method is acceptable
#'
#' @param access the access method
#'
checkAccess <- function(access){
  if(!access %in% c('all-access',
                    'desktop',
                    'mobile-app',
                    'mobile-web'
                    )
     ){
    stop("incorrect value for access, please ensure it is one of:\n'all-access',\n'desktop',\n'mobile-app', or\n'mobile-web'")
  }
}

#' check agent is acceptable
#'
#' @param agent the agent accessing the page
#'
checkAgent <- function(agent){
  if(!agent %in% c('all-agents',
                   'user',
                   'spider',
                   'automated'
                   )
     ){
    stop("incorrect value for agent, please use one of:\n'all-agents',\n'user',\n'spider', or\n'automated'")
  }
}
