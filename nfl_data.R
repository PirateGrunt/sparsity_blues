library(httr)
library(dplyr)
library(jsonlite)
library(caret)

team_search_url <- "http://nflarrest.com/api/v1/team/search/"
team_search_url <- modify_url(team_search_url, query = list(term = "s"))
response <- GET(team_search_url)
the_content <- content(response, "text", encoding = "UTF-8")
tbl_teams <- fromJSON(the_content, simplifyDataFrame =  TRUE)

GetTeamPlayer <- function(team){
  team_player_url <- "http://nflarrest.com/api/v1/team/topPlayers/"
  team_player_url <- paste0(team_player_url, team)
  response <- GET(team_player_url)
  the_content <- content(response, "text", encoding = "UTF-8")
  df <- fromJSON(the_content, simplifyDataFrame =  TRUE)
  df
}

players <- lapply(tbl_teams$team_code, function(x){
  df <- GetTeamPlayer(x)
  players <- df$Name
})

names(players) <- NULL

players <- players %>%
  unlist() %>%
  unique()

GetPlayerArrest <- function(player){
  player_url <- 'http://NflArrest.com/api/v1/player/arrests/'
  player <- gsub(" ", "%20", player)
  player_url <- paste0(player_url, player)
  response <- GET(player_url)
  the_content <- content(response, "text", encoding = "UTF-8")
  df <- fromJSON(the_content, simplifyDataFrame =  TRUE)
  df
}

lstArrests <- lapply(players, function(x){
  df <- GetPlayerArrest(x)
  df
})

lstArrests <- lstArrests[!is.na(lstArrests)]
tbl_arrests <- do.call(rbind, lstArrests)

tbl_arrests <- tbl_arrests %>%
  select(
      -Crime_category_color
    , -Team_hex_alt_color
    , -Team_hex_color
    , -Team_logo_id
    , -Team_preffered_name
    , -Team_Conference_Division
    , -general_category_id
    , -resolution_category_id
    , -legal_level_id
    , -Year
    , -Day
  ) %>%
  rename(
      ArrestDate = Date
    , DayOfWeek = Day_of_Week
    , ArrestID = arrest_stats_id
    , PositionType = Position_type
    , TeamCity = Team_city
    , Conference = Team_Conference
    , Division = Team_Division
    , TeamName = Team_name
    , CrimeCategory = Crime_category
    , TeamAbbr = Team
  ) %>%
  mutate(
      ArrestDate = as.Date(ArrestDate)
    , ArrestID = as.numeric(ArrestID))

MungeVals <- function(x){
  x %>%
    gsub(pattern = " / ", replacement = "_") %>%
    gsub(pattern = " ", replacement = "_")
}

one_hot_vars <- c(
  'TeamAbbr', 'Division', 'Position'
  , 'PositionType', 'DayOfWeek', 'Encounter'
  , 'ArrestSeasonState', 'TeamCity', 'CrimeCategory')

for (i in seq_along(one_hot_vars)) {
  tbl_arrests[[one_hot_vars[i]]] <- MungeVals(tbl_arrests[[one_hot_vars[i]]])
}

tbl_players <- tbl_arrests %>%
  select(
      -Day_of_Week_int
    , -DaysSince
    , -DaysToLastArrest
    , -DaysToLastCrimeArrest
    , -DaysToLastTeamArrest
    , -Month
    , -Position_name
    , -TeamName
  ) %>%
  group_by(Name) %>%
  arrange(Name, ArrestDate) %>%
  slice(1) %>%
  inner_join(summarise(group_by(tbl_arrests, Name), NumArrests = n())) %>%
  mutate(
      MultiArrest = (NumArrests > 1)
    , MultiArrestNum = ifelse(MultiArrest, 1, 0)) %>%
  select(-reddit_group_id, -NumArrests) %>%
  ungroup()

tbl_players$MultiArrestFactor <- as.factor(tbl_players$MultiArrest)
tbl_players$PositionType <- as.factor(tbl_players$PositionType)
tbl_players$Season <- as.factor(tbl_players$Season)
tbl_players$ArrestSeasonState <- as.factor(tbl_players$ArrestSeasonState)


# Why do I have more players than arrest records?
setdiff(players, tbl_players$Name)
# Because I can't find record for players who have an apostrophe in their name

vars_formula <- paste0('~', paste0(one_hot_vars, collapse = '+')) %>%
  as.formula()

dummyVars <- tbl_players %>%
  dummyVars(formula = vars_formula) %>%
  predict(newdata = tbl_players) %>%
  as.data.frame()

one_hot_cols <- names(dummyVars)
ExtractVars <- function(prefix, vals){
  grep(prefix, vals, value = TRUE)
}

one_hot_names <- sapply(one_hot_vars, ExtractVars, vals = one_hot_cols)

tbl_players_one_hot <- dplyr::bind_cols(tbl_players, dummyVars)

save(
    file = "data/nfl_data.rda"
  , tbl_arrests
  , tbl_players
  , tbl_teams
  , tbl_players_one_hot
  , one_hot_cols
  , one_hot_names)
