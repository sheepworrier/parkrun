library(tidyverse)
library(rvest)
library(httr)
library(polite)
library(lubridate)
library(toOrdinal)
library(ggplot2)
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

template_run <-
  paste("%s ran at %s today and finished in %s.  This was their %s Parkrun at",
        "%s and their %s overall.\n")
template_no_run <-
  paste("%s did not run today - their last run was at %s on %s, which they",
        "completed in %s.\n")

generate_report_text <- function(template_run, template_no_run, report_date,
                                 athlete_results) {
  name <- athlete_results$name
  most_recent_result <- athlete_results$results[1, ]
  runs_at_latest_event <- athlete_results$results %>%
    filter(event == most_recent_result$event) %>%
    nrow()
  # Pick template based on most recent run date
  if (most_recent_result$date == report_date) {
    text <- sprintf(template_run, name, most_recent_result$event,
                    most_recent_result$time, toOrdinal(runs_at_latest_event),
                    most_recent_result$event,
                    toOrdinal(nrow(athlete_results$results)))
  } else {
    text <- sprintf(template_no_run, name, most_recent_result$event,
                    most_recent_result$date, most_recent_result$time)
  }
}

generate_boxplot_dataframe <- function(athlete_results) {
  boxplot_results <- athlete_results$results %>%
    mutate(name = athlete_results$name,
           time = as.duration(time) / 60) %>%
    select(name, time)
  boxplot_results
}
