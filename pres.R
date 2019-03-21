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

summarize_category <- function(tbl, category){
  tbl <- tbl[, c('MultiArrestNum', category)]
  names(tbl)[2] <- 'Category'
  
  tbl <- tbl %>%
    group_by(Category) %>% 
    summarize(
      MultiArrest = sum(MultiArrestNum, na.rm = TRUE)
      , N = n()
      ) %>%
    mutate(MultiArrestPct = MultiArrest / N)
  
  tbl
}

mojo <- summarize_category(tbl_players, 'ArrestSeasonState')

tbl_players %>% 
  select()

library(ROCR)
get_auc <- function(predict_val, actual_val){
  perf <- performance(prediction(predict_val, actual_val), 'auc')
  as.numeric(perf@y.values)
}



set.seed(1234)
library(modelr)
tbl_folds <- crossv_kfold(tbl_players, k = 10)

library(randomForest)
mojo <- na.roughfix(tbl_folds)
fit <- randomForest(
    formula = MultiArrest ~ Season + ArrestSeasonState + DayOfWeek
  , data = as.data.frame(tbl_folds$train[[1]])
)
summary(fit)
fit

varImpPlot(fit)

tbl_tree <- tibble(
    x = runif(1e3, 0,10)
  , e = rnorm(1e3)
) %>% 
  mutate(
    y = 1.5 + 2 * x + e
  )

tbl_tree %>% 
  ggplot(aes(x, y)) + 
  geom_point()

fit_tree <- tree::tree(formula = y ~ x, data = tbl_tree)

tbl_tree <- tbl_tree %>% 
  mutate(prediction = predict(fit_tree))

tbl_tree %>% 
  ggplot(aes(x)) + 
  geom_point(aes(y = y), alpha = 0.5) + 
  geom_point(aes(y = prediction), color = 'red')

tbl_mojo <- tibble(
    a = c('red', 'red', 'red', 'blue', 'blue')
  , b = c('black', 'white', 'black', 'white', 'black')
  , output = c(1, 1, 0, 0, 0)
)

entropy <- function(y) {
  tbl <- tibble(y)
  tbl <- tbl %>% 
    group_by(y) %>% 
    summarise(prob = n()) %>% 
    mutate(
        prob = prob / sum(prob)
      , ent = -prob * log(prob))
  
  tbl$ent %>% sum()
}

entropy(tbl_mojo$output)

entropy_post <- function(tbl) {
  tbl %>% 
  summarise(
      ent = entropy(output)
    , group_pct = n()
  ) %>% 
  mutate(
    group_pct = group_pct / sum(group_pct)
  ) %>% 
    ungroup() %>% 
  summarise(
    mojo = sum(ent * group_pct)
  ) %>% 
    pull(mojo)
  
}

tbl_mojo %>% 
  group_by(a) %>% 
  entropy_post()

tbl_mojo %>% 
  group_by(b) %>% 
  entropy_post()

entropy_post <- function(x, y) {
  tbl <- tibble(x, y)
  tbl <- tbl %>% 
    group_by(x) %>%
    summarise(prob = n() / nrow(tbl)) %>% 
    mutate(ent = -prob * log(prob))
  tbl$ent %>% sum()
}

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
