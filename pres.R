knitr::opts_chunk$set(
    echo = TRUE
  , warning = FALSE
  , message = FALSE
  , collapse = TRUE
)

library(tidyverse)

load('./data/nfl_data.rda')
knitr::include_graphics('images/nfl_arrest.png')

prob_multi_arrest <- sum(tbl_players$MultiArrest) / nrow(tbl_players)

sims <- 1e3
tbl_linear <- tibble(
    x = runif(sims, 0, 10)
  , e = rnorm(sims, sd = 5)
) %>% 
  mutate(
    y = 1.5 + 2 * x + e
  )

tbl_linear %>% 
  ggplot(aes(x, y)) + 
  geom_point()

tbl_logistic <- tbl_linear %>% 
  mutate(
      e = rlogis(1e3)
    , latent = -7.5 + 2 * x + e
    , y = as.integer(latent > 0)
  )

tbl_logistic %>% 
  ggplot(aes(x, y)) + 
  geom_point() + 
  geom_smooth(method = glm,  method.args = list(family = "binomial"))

set.seed(1234)
tbl_one_cat <- function(cat_label = 'a', sims = 1e3) {
  slope <- rnorm(1, 2, 2)
  intercept <- rnorm(1, 0, 10)
  tibble(
    x = runif(sims, 0, 10)
  , e = rnorm(sims, sd = 5)
  , category = rep(cat_label, sims)
) %>% 
  mutate(
    y = intercept + slope * x + e
  )
}

tbl_cat <- map_dfr(letters[1:5], tbl_one_cat)

fit_diff_slope <- lm(y ~ 0 + category + x, data = tbl_cat)
plt <- tbl_cat %>% 
  mutate(pred = predict(fit_diff_slope)) %>% 
  ggplot(aes(x, y)) + 
  geom_point(alpha = 0.4) + 
  geom_line(aes(y = pred, color = category), size = 2)
plt

fit_interaction <- lm(y ~ 1 + x:category, data = tbl_cat)
plt <- tbl_cat %>% 
  mutate(pred = predict(fit_interaction)) %>% 
  ggplot(aes(x, y)) + 
  geom_point(alpha = 0.4) + 
  geom_line(aes(y = pred, color = category), size = 2)
plt

plt <- tbl_cat %>% 
  ggplot(aes(x, y)) + 
  geom_point(alpha = 0.4) + 
  geom_smooth(method = lm, aes(color = category), size = 2)
plt

fit_both <- lm(y ~ 0 + category + x:category, data = tbl_cat)
mojo <- fit_both %>% 
  model.matrix()
mojo[, 1:8]%>% 
  head(10) %>% 
  knitr::kable()

library(naivebayes)

fit_nb <- naive_bayes(
    formula = MultiArrest ~ PositionType
  , data = tbl_players
)

fit_nb
# predict(fit_nb, type = 'prob')[1, ]

prior_y <- sum(tbl_players$MultiArrest) / nrow(tbl_players)
prob_x <- sum(tbl_players$PositionType == 'D') / nrow(tbl_players)
tbl_cond <- tbl_players %>% filter(MultiArrest)
prob_x_cond <- sum(tbl_cond$PositionType == 'D') / nrow(tbl_cond)
prior_y * prob_x_cond / prob_x
predict(fit_nb, type = 'prob')[1, 'TRUE']

prior_y

fit_nb <- naive_bayes(
    formula = MultiArrest ~ TeamAbbr + Conference + Division + Position 
        + PositionType + Encounter + CrimeCategory + ArrestSeasonState 
    + DayOfWeek
  , data = tbl_players
)

tbl_players %>% 
  mutate(MultiArrestPred = predict(fit_nb, type = 'prob')[, 'TRUE']) %>% 
  arrange(desc(MultiArrestPred)) %>% 
  select(MultiArrestPred) %>% 
  ggplot(aes(MultiArrestPred)) + 
  geom_histogram(binwidth = .02) + 
  geom_vline(xintercept = prob_multi_arrest, color = 'red')

library(tree)
fit_tree <- tree::tree(formula = y ~ x, data = tbl_linear)
summary(fit_tree)

tbl_linear <- tbl_linear %>% 
  mutate(prediction = predict(fit_tree))

tbl_linear %>% 
  ggplot(aes(x)) + 
  geom_point(aes(y = y), alpha = 0.5) + 
  geom_point(aes(y = prediction), color = 'red')

tbl_toy <- tibble(
    a = c('red', 'red', 'red', 'blue', 'blue')
  , b = c('black', 'white', 'black', 'white', 'black')
  , output = c(1, 1, 0, 0, 0)
)

tbl_toy %>% 
  knitr::kable()

entropy <- function(y) {
  tbl <- tibble(y) %>% 
    group_by(y) %>% 
    summarise(prob = n()) %>% 
    mutate(
        prob = prob / sum(prob)
      , ent = -prob * log(prob))
  
  tbl$ent %>% sum()
}

entropy_post <- function(tbl, out_col, split_col) {
  
  split_col <- enquo(split_col)
  out_col <- enquo(out_col)
  
  tbl %>% 
    group_by(!! split_col) %>% 
    summarise(
        ent = entropy(!! out_col)
      , group_pct = n() / nrow(tbl)
    ) %>%
    ungroup() %>%
    summarise(
      ent_post = sum(ent * group_pct)
    ) %>%
    pull(ent_post)
}

entropy(tbl_toy$output)

tbl_toy %>% 
  entropy_post(output, a)

tbl_toy %>% 
  entropy_post(output, b)

tbl_toy %>% 
  knitr::kable(format = 'html')

entropy(tbl_players$MultiArrestNum)
tbl_players %>% 
  entropy_post(MultiArrestNum, PositionType)

tbl_players %>% 
  entropy_post(MultiArrestNum, Season)

tbl_players %>% 
  entropy_post(MultiArrestNum, ArrestSeasonState)

library(rpart)

fit_tree <- tree(
    data = tbl_players
  , formula = MultiArrestFactor ~ PositionType + Season + ArrestSeasonState)

summary(fit_tree)

plot(fit_tree)
text(fit_tree, pretty = 0)

library(randomForest)
fit_forest <- randomForest(
    formula = MultiArrestFactor ~ PositionType + Season + ArrestSeasonState
  , data = tbl_players
)

varImpPlot(fit_forest)

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

tbl_cats <- tbl_players %>% 
  ungroup() %>% 
  select(
      CrimeCategory, ArrestSeasonState, Conference
    , Division, DayOfWeek, Outcome, Position, PositionType
    , Season) %>% 
  mutate_if(is.character, as.factor)

library(FactoMineR)
fit_mca <- MCA(tbl_cats, graph = FALSE)

tbl_players <- tbl_players %>% 
  ungroup() %>% 
  mutate(
    dim_1 = fit_mca$ind$coord[, 1]
    , dim_2 = fit_mca$ind$coord[, 2]
  )

tbl_players %>% 
  ggplot(aes(dim_1, dim_2), size = 2, alpha = 0.7) + 
  geom_point(aes(color = PositionType))

fit_logistic_mca <- glm(
  MultiArrestNum ~ 0 + dim_1 + dim_2
  , data = tbl_players
  , family = binomial())

fit_logistic_mca %>% 
  summary()

misclass <- function(tbl_test, fit_obj) {
  tbl_test <- tbl_test %>% 
    mutate(
        pred = predict(fit_obj, type = 'class', newdata = tbl_test)
      , misclass = pred != MultiArrestFactor
    )
  sum(tbl_test$misclass) / nrow(tbl_test)
}

library(modelr)
set.seed(1234)
tbl_folds <- crossv_kfold(tbl_players, k = 10)

tbl_folds %>% head()

tbl_folds$train[[1]] %>% class()

assess_fold <- function(obj_train, obj_test, method, the_formula) {
  tbl_train <- obj_train %>% as.data.frame()
  tbl_test <- obj_test %>% as.data.frame()
  
  fit <- do.call(
      method
    , args = list(formula = the_formula, data = tbl_train))
  
  misclass(tbl_test, fit)

}

one_fold_misclass <- assess_fold(
    tbl_folds$train[[1]]
  , tbl_folds$test[[1]]
  , tree::tree
  , as.formula('MultiArrestFactor ~ PositionType + Season'))

cross_validate <- function(formula, tbl_folds, method) {
  map2_dbl(
    tbl_folds$train
  , tbl_folds$test
  , assess_fold
  , method
  , formula
  ) %>% mean()
}

misclasses <- cross_validate(
    as.formula('MultiArrestFactor ~ PositionType + Season')
  , tbl_folds
  , tree::tree
)

misclasses <- cross_validate(
    as.formula('MultiArrestFactor ~ PositionType + Season')
  , tbl_folds
  , naive_bayes
)

make_formula <- function(predictors, target, intercept = TRUE) {
  str_predictors <- paste(predictors, collapse = '+')
  if (intercept) {
    str_formula <- paste(target, '~ 1 + ')
  } else {
    str_formula <- paste(target, '~')
  }
  
  str_formula <- paste(str_formula, str_predictors)
  as.formula(str_formula)
}

the_formulas <- list(
      c('PositionType', 'Season')
    , c('PositionType', 'Season', 'DayOfWeek')
    , c('PositionType', 'Season', 'DayOfWeek')
    , c('PositionType', 'Season', 'DayOfWeek', 'Conference')
    , c('PositionType', 'Season', 'DayOfWeek', 'Conference', 'Division')
    , c('PositionType', 'Season', 'DayOfWeek', 'Conference', 'Division', 'TeamCity')
  ) %>% 
  map(make_formula, 'MultiArrestFactor', intercept = FALSE) %>% 
  as.vector()

tbl_models <- tibble(
  formula = the_formulas
)

tbl_models %>% 
  knitr::kable()

tbl_models <- tbl_models %>%
  mutate(
      misclass_tree = map_dbl(formula, cross_validate, tbl_folds, tree::tree)
    , misclass_nb = map_dbl(formula, cross_validate, tbl_folds, naive_bayes)
  )

tbl_models %>% 
  knitr::kable()
