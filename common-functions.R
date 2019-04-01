library(tidyverse)
library(rvest)
library(httr)
library(polite)
library(lubridate)
get_all_runs <- function(athlete_id) {
  # Construct the URL to get all results for a given athlete ID
  allRunsURL <-
    paste0("http://www.parkrun.org.uk/results/athleteeventresultshistory/",
           "?athleteNumber=", athlete_id, "&eventNumber=0")
  # Create a session connecting to the above URL
  session <- bow(allRunsURL, force = TRUE)
  # Scrape the main table from this page
  result <- scrape(session) %>%
    html_node("div:nth-child(10) #results") %>%
    html_table()
  # Scrape the athlete's name from this page
  header <- scrape(session) %>%
    html_node("h2") %>%
    html_text()
  first_name <- str_extract_all(header, boundary("word"))[[1]][[1]]
  # Reformat the column names to match usual style
  colnames(result) <- c("event", "date", "event_number", "position", "time",
                        "wava_age_grade", "personal_best")
  # Reformat the date and time variables
  result <- result %>%
    mutate(time = if_else(nchar(time) == 5,
                          hms(paste0("00:", time), quiet = TRUE),
                          hms(paste0("0", time), quiet = TRUE)),
           date = as.Date(date, "%d/%m/%Y"),
           sameday_rank = ifelse(mday(date) == 1 &
                                   month(date) == 1,
                                 ifelse(event == "Hove Promenade", 1, 2),
                                 1))
  # Return the results to the caller
  list(results = result, name = first_name)
}