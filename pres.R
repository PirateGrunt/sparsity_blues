knitr::opts_chunk$set(
    echo = FALSE
  , warning = FALSE
  , message = FALSE
)

library(tidyverse)
library(FactoMineR)
knitr::include_graphics('images/nfl_arrest.png')
load('data/nfl_data.rda')
num_players <- nrow(tbl_players)
num_arrests <- nrow(tbl_arrests)
num_multi_arrest <- sum(tbl_players$MultiArrest)
ggplot(filter(tbl_players, NumArrests > 1), aes(NumArrests, fill = ArrestSeasonState)) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = seq.int(10))
tbl_toy_mca <- tibble(
    id = 1:4
  , metro = c('urban', 'urban', 'rural', 'urban') %>% as_factor()
  , region = c('north', 'south', 'east', 'north') %>% as_factor()
)
tbl_toy_mca %>% 
  knitr::kable()
tbl_toy_mca_one_hot <- tbl_toy_mca %>% 
  gather(category, value, -id) %>% 
  unite(cdt, -id) %>%
  mutate(count = 1L) %>% 
  tidyr::spread(cdt, count, fill = 0L)

tbl_toy_mca_one_hot %>% knitr::kable()
tbl_toy_mca_one_hot %>% knitr::kable()
