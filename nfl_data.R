library(tidyverse)
library(httr)
library(jsonlite)
library(caret)

#================================================
# Base URLs
url_team_search <- "http://nflarrest.com/api/v1/team/search/"
url_player <- "http://nflarrest.com/api/v1/team/topPlayers/"
url_player_arrest <- 'http://NflArrest.com/api/v1/player/arrests/'

#================================================
# Get the list of team names
url_team_search <- modify_url(url_team_search, query = list(term = "s"))
response <- GET(url_team_search)
the_content <- content(response, "text", encoding = "UTF-8")
tbl_teams <- fromJSON(the_content, simplifyDataFrame =  TRUE)

#================================================
# Get players as a vector

GetTeamPlayer <- function(team){
  url_player_team <- paste0(url_player, team)
  response <- GET(url_player_team)
  the_content <- content(response, "text", encoding = "UTF-8")
  df <- fromJSON(the_content, simplifyDataFrame =  TRUE)
  df$Name
}

players <- map(
    tbl_teams$team_code
  , GetTeamPlayer
) %>%
  flatten_chr()

#================================================
# Get the arrests

GetPlayerArrest <- function(player){
  player <- gsub(" ", "%20", player)
  player <- gsub("'", "%27", player)
  url_player_arrest <- paste0(url_player_arrest, player)
  response <- GET(url_player_arrest)
  the_content <- content(response, "text", encoding = "UTF-8")
  df <- fromJSON(the_content, simplifyDataFrame =  TRUE)
  df
}

tbl_arrests <- players %>%
  map_dfr(GetPlayerArrest)

mojo <- tbl_arrests
tbl_arrests <- mojo

#==============================================
# Munge arrests

tbl_arrests <- tbl_arrests %>%
  select(
      -Team_preffered_name
    , -Team_logo_id
    , -Team_Conference_Division
    , -Team_hex_alt_color
    , -Team_hex_color
    , -reddit_group_id
    , -Position
    , -Crime_category_color
    , -YearToDate
  ) %>%
  rename(
      arrest_id = arrest_stats_id
    , arrest_date = Date
    , day_of_week = Day_of_Week
    , position_type = Position_type
    , team_city = Team_city
    , conference = Team_Conference
    , division = Team_Division
    , team_name = Team_name
    , crime_category = Crime_category
    , team_abbr = Team
    , days_since = DaysSince
    , arrest_season_state = ArrestSeasonState
    , days_to_last_arrest = DaysToLastArrest
    , days_to_last_crime_arrest = DaysToLastCrimeArrest
    , days_to_last_team_arrest = DaysToLastTeamArrest
  )

names(tbl_arrests) <- tolower(names(tbl_arrests))

munge_chars <- function(x){
  x %>%
    gsub(pattern = " / ", replacement = "_") %>%
    gsub(pattern = " ", replacement = "_")
}

one_hot_vars <- c(
    'team_abbr'
  , 'division'
  , 'position_name'
  , 'position_type'
  , 'day_of_week'
  , 'encounter'
  , 'arrest_season_state'
  , 'team_city'
  , 'crime_category'
)

tbl_arrests <- tbl_arrests %>%
  mutate_at(
      c('arrest_id', 'season'
        , 'year', 'month', 'day'
        , 'days_since', 'days_to_last_arrest'
        , 'days_to_last_crime_arrest', 'days_to_last_team_arrest')
    , as.numeric
  ) %>%
  mutate_at(
      c('arrest_date')
    , as.Date
  ) %>%
  mutate_at(
      one_hot_vars
    , munge_chars
  ) %>%
  mutate(team_city = gsub("_", " ", team_city))

#==================================
# Join to cities
load("data/cities.rda")

tbl_arrests <- tbl_arrests %>%
  left_join(tbl_cities, by = c(team_city = 'city'))

missing_cities <- tbl_arrests$latitude %>%
  is.na() %>%
  any()

if(missing_cities) {

  tbl_arrests %>%
    filter(is.na(latitude)) %>%
    select(team_city) %>%
    unique()


  stop("Missing some cities!")
}

#==================================
# Form players table

tbl_players <- tbl_arrests %>%
  group_by(name) %>%
  arrange(name, arrest_date) %>%
  slice(1) %>%
  ungroup() %>%
  inner_join(
    tbl_arrests %>% group_by(name) %>% summarise(num_arrests = n())
  ) %>%
  mutate(
      multi_arrest = (num_arrests > 1)
    , multi_arrest_num = ifelse(multi_arrest, 1, 0)
    , multi_arrest_factor = as.factor(multi_arrest)
  ) %>%
  mutate_if(
      is.character
    , as.factor
  )

tbl_arrests <- tbl_arrests %>%
  mutate_if(
    is.character
    , as.factor
  )

# Why do I have more players than arrest records?
setdiff(players, tbl_players$name)
# Because I can't find record for players who have an apostrophe in their name

#==========================================
# Build one-hot encoding table.
# Do I really need this?
vars_formula <- paste0('~', paste0(one_hot_vars, collapse = '+')) %>%
  as.formula()

dummyVars <- tbl_players %>%
  dummyVars(formula = vars_formula) %>%
  predict(newdata = tbl_players) %>%
  as.data.frame()

# one_hot_cols <- names(dummyVars)
# ExtractVars <- function(prefix, vals){
#   grep(prefix, vals, value = TRUE)
# }
#
# one_hot_names <- sapply(one_hot_vars, ExtractVars, vals = one_hot_cols)

tbl_players_one_hot <- dplyr::bind_cols(tbl_players, dummyVars)

save(
    file = "data/nfl_data.rda"
  , tbl_arrests
  , tbl_players
  , tbl_teams
  , tbl_players_one_hot
)
