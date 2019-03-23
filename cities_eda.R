tbl_cities %>%
  ggplot(aes(longitude, population)) +
  geom_point() +
  scale_y_log10()

tbl_cities %>%
  ggplot(aes(latitude, population)) +
  geom_point() +
  scale_y_log10()

tbl_cities %>%
  ggplot(aes(latitude, land_area)) +
  geom_point()

tbl_cities %>%
  filter(longitude > -140) %>%
  ggplot(aes(longitude, land_area)) +
  geom_point() +
  geom_smooth(method = 'lm')

tbl_cities %>%
  filter(longitude > -140) %>%
  ggplot(aes(longitude, land_area)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_y_log10()

tbl_cities %>%
  filter(longitude > -140) %>%
  ggplot(aes(longitude, population_density)) +
  geom_point() +
  geom_smooth(method = 'lm')

tbl_cities %>%
  filter(longitude > -140) %>%
  ggplot(aes(latitude, population_density)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_y_log10()

tbl_cities %>%
  filter(longitude > -140) %>%
  ggplot(aes(longitude, latitude, size = population_density)) +
  geom_point()
