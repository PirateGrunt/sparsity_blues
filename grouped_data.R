region <- c('north', 'south')
metro <- c('urban', 'suburban', 'rural')
sample_size <- 1e3
intercept <- 0
b1 <- 1.5

tbl <- tibble(
    x1 = base::sample(1:10, size = sample_size, replace = TRUE)
  , a1 = base::sample(region, size = sample_size, replace = TRUE) %>% as_factor()
  , a2 = base::sample(metro, size = sample_size, replace = TRUE) %>% as_factor()
) %>%
  mutate(
    e = rnorm(sample_size, 3)
  , y = intercept + x1 * b1 + e
  )

fit <- tbl %>%
  lm(formula = y ~ 0 + x1:a1:a2)

summary(fit)

fit_north_suburban <- tbl %>%
  filter(
      a1 == 'north'
    , a2 == 'suburban') %>%
  lm(formula = y ~ 0 + x1)

summary(fit_north_suburban)
