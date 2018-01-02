library(httr)
library(dplyr)
library(jsonlite)
library(caret)

team_search_url <- "http://nflarrest.com/api/v1/team/search/"
team_search_url <- modify_url(team_search_url, query=list(term="s"))
response <- GET(team_search_url)
the_content <- content(response, "text", encoding = "UTF-8")
dfTeam <- fromJSON(the_content, simplifyDataFrame =  TRUE)

GetTeamPlayer <- function(team){
  team_player_url <- "http://nflarrest.com/api/v1/team/topPlayers/"
  team_player_url <- paste0(team_player_url, team)
  response <- GET(team_player_url)
  the_content <- content(response, "text", encoding = "UTF-8")
  df <- fromJSON(the_content, simplifyDataFrame =  TRUE)
  df
}

players <- lapply(dfTeam$team_code, function(x){
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
dfArrests <- do.call(rbind, lstArrests)

dfArrests <- dfArrests %>%
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
    gsub(pattern=" / ", replacement = "_") %>%
    gsub(pattern=" ", replacement = "_")
}

one_hot_vars <- c(
  'TeamAbbr', 'Division', 'Position'
  , 'PositionType', 'DayOfWeek', 'Encounter'
  , 'ArrestSeasonState', 'TeamCity', 'CrimeCategory')

for (i in seq_along(one_hot_vars)){
  dfArrests[[one_hot_vars[i]]] <- MungeVals(dfArrests[[one_hot_vars[i]]])
}

dfPlayers <- dfArrests %>%
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
  inner_join(summarise(group_by(dfArrests, Name), NumArrests = n())) %>%
  mutate(MultiArrest = (NumArrests > 1))

# Why do I have more players than arrest records?
setdiff(players, dfPlayers$Name)
# Because I can't find record for players who have an apostrophe in their name

vars_formula <- paste0('~', paste0(one_hot_vars, collapse = '+')) %>%
  as.formula()

dummyVars <- dfPlayers %>%
  dummyVars(
    formula = vars_formula
    ) %>%
  predict(newdata = dfPlayers) %>%
  as.data.frame()

one_hot_cols <- names(dummyVars)
ExtractVars <- function(prefix, vals){
  grep(prefix, vals, value = TRUE)
}

one_hot_names <- sapply(one_hot_vars, ExtractVars, vals=one_hot_cols)

dfPlayers <- dplyr::bind_cols(dfPlayers, dummyVars)

save(
    file = "data/nfl_data.rda"
  , dfArrests
  , dfPlayers
  , dfTeam
  , one_hot_cols
  , one_hot_names)
