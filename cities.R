#==================================
# Get city characteristics

library(XML)
library(RCurl)
library(tidyverse)

url_cities <- 'https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population'

wiki_stuff <- getURL(url_cities, .opts = list(ssl.verifypeer = FALSE) )
tables <- readHTMLTable(wiki_stuff, stringsAsFactors = FALSE)

tbl_cities <- tables[[5]]

tbl_cities <- tbl_cities[-1, c(2, 4, 8, 9, 11)]

names(tbl_cities) <- c('city', 'population', 'land_area', 'population_density', 'lat_long')

tbl_cities <- tbl_cities %>%
  mutate_if(is.character, ~gsub('[^ -~]', '', .)) %>%
  mutate(
      city = gsub("[[:digit:]]", "", city)
    , city = gsub("\\[", "", city)
    , city = gsub("\\]", "", city)
    , land_area = gsub("km2", "", land_area)
    , population_density = gsub("/", "", population_density)
    , population_density = gsub("sqmi", "", population_density)
  )

munge_to_numeric <- function(x) {
  x <- gsub("[[:blank:]]", "", x)
  x <- gsub(",", "", x)

  x %>%
    as.numeric()
}

tbl_cities <- tbl_cities %>%
  mutate_at(
    c('population', 'land_area', 'population_density')
    , munge_to_numeric
  )

tbl_cities <- tbl_cities %>%
  tidyr::separate(
      lat_long
    , c("lat_long", "extra")
    , sep = " / "
    , extra = 'drop') %>%
  select(-extra) %>%
  separate(
      lat_long
    , c("latitude", "longitude")
    , sep = " "
  )

convert_lat_long <- function(x) {
  x <- gsub("[[:alpha:]]", "", x)

  lat_length <- nchar(x)
  first_tok <- ifelse(lat_length == 6, 2, 3)
  degrees <- substr(x, 1, first_tok) %>% as.numeric()
  minutes <- substr(x, first_tok + 1, first_tok + 2) %>% as.numeric()
  seconds <- substr(x, first_tok + 3, first_tok + 4) %>% as.numeric()

  degrees + minutes / 60 + seconds / 360
}

tbl_cities <- tbl_cities %>%
  mutate_at(
    c("latitude", "longitude")
    , convert_lat_long
  ) %>%
  mutate(
    longitude = -longitude
  )

#=============================
# Manual fix for some cities
tbl_cities <- tbl_cities %>%
  mutate(
    city = case_when(
        city == "Tampa" ~ "Tampa Bay"
      , city == "New York City" ~ "New York"
      , city == "Washington, D.C." ~ "Washington DC"
      , TRUE ~ city
    )
  )

save(
    file = "data/cities.rda"
  , tbl_cities)
