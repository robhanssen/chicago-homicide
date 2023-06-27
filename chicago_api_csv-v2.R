#
# R script to analyze the homicide rate in Chicago.
# Data is provided via the API of data.cityofchicago.org
#

#
# load the required libraries
#
library(tidyverse)
library(lubridate)


scalevalue <- function(value, scale) {
  max_value <- max(value)
  upscale <- (max_value %/% scale + 1) * scale
  return(upscale)
}


#
# import via web API
# $app_token=2OghfIz6RILPCh3eJQ5XI4ZZQ obtained via OpenData by Socrata
# crime_file = "https://data.cityofchicago.org/resource/6zsd-86xi.csv?primary_type=HOMICIDE&$where=year>2000&$limit=20000&$$app_token=2OghfIz6RILPCh3eJQ5XI4ZZQ"
#

violation <- "HOMICIDE"
firstyear <- 2000
recordlimit <- 20000
apptoken <- "2OghfIz6RILPCh3eJQ5XI4ZZQ"

crime_file <- paste0(
  "https://data.cityofchicago.org/resource/6zsd-86xi.csv?",
  "primary_type=", violation,
  "&$where=year>", firstyear,
  "&$limit=", recordlimit,
  "&$$app_token=", apptoken
)

chicagocrime <- read_csv(crime_file,
  col_types = cols(
    date = col_datetime(format = "%Y-%m-%dT%H:%M:%S"),
    location = col_skip(),
    updated_on = col_skip(),
    x_coordinate = col_skip(),
    y_coordinate = col_skip()
  )
) %>%
  rename(datetime = date) %>%
  arrange(datetime)

#
# add some useful date-based modifiers for further analysis
#
chicagocrime <- chicagocrime %>%
  mutate(
    date = as.Date(datetime, format = "%Y-%m-%d"),
    year = year(date),
    decade = (year %/% 10) * 10,
    quarter = quarter(date),
    month = month(date),
    day = day(date),
    hour = hour(datetime),
    dow = weekdays(date),
    yday = yday(date)
  )

#
# Summary homicide data on year-to-date, down to the last reported yearday
#
homicide <- chicagocrime %>%
  group_by(year, date, yday) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(ysum = cumsum(count)) %>%
  ungroup()

max_yday <- homicide %>%
  filter(year == max(year)) %>%
  summarize(maxyday = max(yday)) %>%
  pull(maxyday)

homicide_YTDay <- homicide %>% # nolint
  filter(yday <= max_yday) %>%
  group_by(year) %>%
  summarise(total = sum(count)) %>%
  ungroup()

maxdate <- max(homicide$date)
title <- paste(
  "Chicago homicides year-to-date as of",
  format(maxdate, format = "%b %d")
)

max_y_value <- scalevalue(homicide_YTDay$total, 50)

crime_ytd_g <-
  homicide_YTDay %>% ggplot() +
  aes(year, total) +
  geom_bar(stat = "identity") +
  labs(title = title, x = "Year", y = "Homicide count") +
  geom_text(aes(label = total), vjust = -1)


ggsave("graphs/chicagohomicide_YTD.png",
  plot = crime_ytd_g
)

max_y_value <- scalevalue(homicide$ysum, 100)

title <- paste0(
  "Chicago cumulative homicides by day-of-year as of ",
  format(maxdate, format = "%b %d")
)

crime_cumulative_g <-
  homicide %>%
  filter(year %in% c(2015, 2016, 2020, 2021, 2022, 2023)) %>%
  mutate(ydate = as.Date("2017-01-01") + yday - 1) %>%
  ggplot() +
  aes(ydate, ysum, color = factor(year)) +
  geom_line() +
  labs(
    title = title, x = "Time of year",
    y = "Cumulative homicide count", color = "By year"
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0, max_y_value)) +
  theme(legend.position = c(0.07, 0.845))


ggsave("graphs/chicagohomicide-by-dayofyear.png",
  plot = crime_cumulative_g
)

#
# by months
#
#

crime_by_month_g <-
  chicagocrime %>%
  group_by(year, month) %>%
  summarize(monthlyhomicide = n()) %>%
  filter(year > 2014) %>%
  mutate(date = as.Date(paste0(year, "-", month, "-01"))) %>%
  ggplot() +
  aes(
    x = year, y = monthlyhomicide,
    fill = fct_reorder(factor(month), -month)
  ) +
  geom_col() +
  labs(x = "Year", y = "Homicide per month", fill = "Month")

ggsave("graphs/chicagohomicide-by-month.png",
  plot = crime_by_month_g
)
