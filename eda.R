library(rvest)
library(RCurl)
library(XML)
library(dplyr)
library(ggplot2)
library(ggrepel)
# UDF to extract recults and plot them
plot_results <- function(athlete_id, event_name) {
  my_results_url <-
    paste0("https://www.parkrun.org.uk/parkrunner/", athlete_id, "/all/")
  print(paste("Processing:", my_results_url))
  # Load raw results page using curl
  go_to_link <- getURL(my_results_url)
  # Convert to a structured list in HTML format
  my_results_page <- read_html(go_to_link)
  # Read the table of results into data frame
  my_results_df <- html_table(my_results_page)[[3]]
  # Label as pre-COVID or post-COVID
  event_results <- my_results_df %>%
    mutate(Date = as.Date(`Run Date`, format = "%d/%m/%Y"),
           Era = if_else(Date < '2021-07-31', "Pre-COVID", "Post-COVID"),
           Time = ms(Time) %>%
             as.duration()) %>%
    filter(Event == !!event_name) %>%
    arrange(Era, Date) %>%
    group_by(Era) %>%
    mutate(fastest_time = cummin(Time) %>%
             as.POSIXct(origin = "2022-09-10", tz = "Europe / London"),
           days_since_era_start = (Date - min(Date)) %>%
             as.numeric(),
           label = if_else(fastest_time == min(Time) %>%
                             as.POSIXct(origin = "2022-09-10",
                                        tz = "Europe / London"),
                           format(fastest_time, "%M:%S"),
                           NA_character_),
           label = format(fastest_time,
                          "%M:%S"),
           label = if_else(lag(label) == label,
                           NA_character_,
                           label))
  # Plot of results side by side
  ggplot(data = event_results, aes(x = Date, y = fastest_time, group = Era,
                                       colour = Era)) +
    geom_line() +
    geom_label_repel(aes(label = label),
                     nudge_x = 1,
                     na.rm = TRUE)
  # Plot of results overlaid
  ggplot(data = event_results, aes(x = days_since_era_start, y = fastest_time,
                                       group = Era, colour = Era)) +
    geom_line() +
    geom_label_repel(aes(label = label),
                     nudge_x = 1,
                     na.rm = TRUE) +
    labs(title = paste(event_name, "stats"), x = "Days since start of Era",
         y = "Fastest Time")  
}
# Julie's results
plot_results(937323, "Hove Promenade")
# Dean's results
plot_results(733446, "Brighton & Hove")
