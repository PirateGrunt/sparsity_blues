library(FactoMineR)
library(tidyverse)

data(tea)

tea_sub <- tea %>%
  select(Tea, How, how, sugar, where, always)

cats <- tea_sub %>%
  map_int(~nlevels(.))
cats

mcal <- MCA(tea_sub, graph = FALSE)

mojo <- mcal$var$coord %>%
  as_tibble(rownames = 'var') %>%
  mutate(category = rep(names(cats), cats)) %>%
  select(category, dplyr::everything())

mojo %>%
  ggplot(aes(`Dim 1`, `Dim 2`, color = category)) +
  geom_text(aes(label = var))

tea_sub %>%
  filter(where == 'tea shop') %>%
  group_by(how) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

tea_sub %>%
  filter(Tea == 'Earl Grey') %>%
  group_by(where) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

tea_sub %>%
  select(Tea, where) %>%
  group_by(Tea, where) %>%
  summarise(count = n()) %>%
  group_by(where) %>%
  mutate(count = count / sum(count)) %>%
  spread(where, count)

tea_sub %>%
  select(how, where) %>%
  group_by(how, where) %>%
  summarise(count = n()) %>%
  group_by(where) %>%
  mutate(count = count / sum(count)) %>%
  spread(where, count)


load('data/nfl_data.rda')
tbl_players <- tbl_players %>%
  ungroup()


mca_nfl <- tbl_players %>%
  select(Conference, Division, PositionType) %>%
  mutate(
      Conference = forcats::as_factor(Conference)
    , Division = forcats::as_factor(Division)
    , PositionType = forcats::as_factor(PositionType)
  ) %>%
  MCA(graph = FALSE)

mojo <- mca_nfl$var$coord %>%
  as_tibble(rownames = 'var')
  # mutate(
  #   category = rep(names(cats), cats)
  # ) %>%
  select(category, dplyr::everything())

mojo %>%
  ggplot(aes(`Dim 1`, `Dim 2`)) +
  geom_text(aes(label = var))

tbl_players %>%
  group_by(PositionType) %>%
  summarize(
    n = n()
    , total_arrests = sum(NumArrests)
    , mean_arrests = mean(NumArrests)
    , multi_arrest = sum(MultiArrest)
    , mean_multi_arrest = sum(MultiArrest) / n()
  )

mojo %>%
  filter(var != 'S') %>%
  ggplot(aes(`Dim 1`, `Dim 2`)) +
  geom_text(aes(label = var))

#--------
# Another table

tbl_mojo <- tibble(
    color = c('blue', 'green', 'blue', 'blue', 'green', 'blue', 'green', 'green') %>%
      as_factor()
  , size = c('large', 'small', 'small', 'medium', 'small', 'large', 'medium', 'large') %>%
    as_factor()
)
tbl_mojo %>% tab.disjonctif() %>% View()

tbl_mojo <- tribble(
  ~color, ~size, ~count
  , 'blue', 'large', 10
  , 'blue', 'medium', 8
  , 'blue', 'small', 3
  , 'green', 'large', 20
  , 'green', 'medium', 45
  , 'green', 'small', 4
  , 'red', 'large', 15
  , 'red', 'medium', 40
  , 'red', 'small', 8
)

tbl_mojo <- tribble(
  ~color, ~size, ~count
  , 'blue', 'large', 1
  , 'blue', 'medium', 4
  , 'blue', 'small', 3
  , 'green', 'large', 6
  , 'green', 'medium', 2
  , 'green', 'small', 4
  , 'red', 'large', 1
  , 'red', 'medium', 4
  , 'red', 'small', 7
)

tbl_mojo_contingency <- tbl_mojo %>%
  spread(size, count) %>%
  as.data.frame()

row.names(tbl_mojo_contingency) <- tbl_mojo_contingency$color
tbl_mojo_contingency$color <- NULL

fit_ca <- CA(tbl_mojo_contingency)

summary(fit_ca)
plot(fit_ca)
fit_ca$eig

data("children")

fit_ca <- CA(children, row.sup = 15:18, col.sup = 6:8)

fit_ca
summary(fit_ca)
plot(fit_ca)
fit_ca$eig


#--------------------
# MCA
tbl_mojo <- tibble(
  color = c('blue', 'green', 'blue', 'blue', 'green', 'blue', 'green', 'green') %>%
    as_factor()
  , size = c('large', 'small', 'small', 'medium', 'small', 'large', 'medium', 'large') %>%
    as_factor()
)

fit_mca <- MCA(tbl_mojo)
plot(fit_mca)
