library(caret)
data(cars, package = 'caret')
library(tidyverse)

cars_categorized <- cars %>%
  gather(Model, dummy, Buick, Cadillac, Chevy, Pontiac, Saturn, Saab) %>%
  filter(dummy == 1) %>%
  select(-dummy) %>%
  gather(Style, dummy, convertible, coupe, hatchback, sedan, wagon) %>%
  filter(dummy == 1) %>%
  select(-dummy) %>%
  mutate(
    Model = as_factor(Model)
    , Style = as_factor(Style)
  )

fit <- cars %>%
  lm(formula = Price ~ Mileage + Cylinder + Doors + Cruise + Sound + Leather + Buick + Cadillac + Chevy + Pontiac + Saab + Saturn)

summary(fit)

fit_1 <- cars %>%
  lm(formula = Price ~ 0 + Mileage + Sound + Leather + Buick + Cadillac + Chevy + Pontiac + Saturn + Saab)

summary(fit_1)

fit_2 <- cars_categorized %>%
  lm(formula = Price ~ 0 + Mileage + Sound + Leather + Model)

summary(fit_2)

coef_1 <- coef(fit_1) %>% unname() %>% sort()

coef_2 <- coef(fit_2) %>% unname() %>% sort()
all.equal(coef_1, coef_2)

library(FactoMineR)
fit_mca <- cars_categorized %>%
  select(Model, Style) %>%
  MCA()

plot(fit_mca, invisible = 'ind')
plot(fit_mca, invisible = 'var')

cars_categorized <- cars_categorized %>%
  mutate(
    axis_1 = fit_mca$ind$coord[, 1]
    , axis_2 = fit_mca$ind$coord[, 2]
  )

fit_3 <- cars_categorized %>%
  lm(formula = Price ~ 1 + axis_1 + axis_2)

summary(fit_3)

fit_4 <- cars_categorized %>%
  lm(formula = Price ~ 1 + Model + Style)

summary(fit_4)

fit_5 <- cars_categorized %>%
  lm(formula = Price ~ 1 + Mileage + Model + Style)
summary(fit_5)

fit_6 <- cars_categorized %>%
  lm(formula = Price ~ 1 + Mileage:Model + Style)
summary(fit_6)

fit_7 <- cars_categorized %>%
  lm(formula = Price ~ 1 + Mileage:Model)
summary(fit_7)

fit_8 <- cars_categorized %>%
  lm(formula = Price ~ 1 + Mileage:Style)
summary(fit_8)

fit_9 <- cars_categorized %>%
  lm(formula = Price ~ 1 + Mileage)
summary(fit_9)

tbl_cat <- fit_mca$var$coord %>%
  as_tibble(rownames = 'cat')

tbl_cat %>%
  ggplot(aes(`Dim 1`, `Dim 2`)) +
  geom_text(aes(label = cat))

set.seed(1234)
library(modelr)
tbl_folds <- crossv_kfold(cars_categorized, k = 10) %>%
  mutate(
      mojo = map(train, as.data.frame)
    # , test = as.data.frame(test)
  )

train_model <- function(resamp){
  resamp %>%
    as.data.frame() %>%
    lm(formula = Price ~ 1 + Mileage:Style)
}

assess_fit <- function(trained_model, resamp_test){

  df_test <- resamp_test %>% as.data.frame()
  predicted <- predict(trained_model, newdata = df_test)
  (predicted - df_test$Price) ^ 2 %>% mean() %>% sqrt()

}

library(broom)
tbl_folds <- tbl_folds %>%
  mutate(
      trained_model = map(train, ~ lm(Price ~ 1 + Mileage:Style, data = .))
    , rmse = map2(trained_model, test, assess_fit)
  )

