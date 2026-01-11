library(dplyr)
library(readr)


read_airbnb <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE)

  df |>
    filter(
      !is.na(latitude), !is.na(longitude),
      !is.na(price),
      latitude  > 40.3, latitude  < 41.1,
      longitude > -74.5, longitude < -73.3
    )
}

summarize_listings <- function(df) {
  df |>
    summarise(
      n_listings = n(),
      avg_price = round(mean(price, na.rm = TRUE), 2),
      med_price = round(median(price, na.rm = TRUE), 2),
      avg_reviews = round(mean(number_of_reviews, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

