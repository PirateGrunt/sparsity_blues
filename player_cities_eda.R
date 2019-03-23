load('./data/nfl_data.rda')

tbl_players %>%
  ggplot(aes(latitude, multi_arrest_num)) +
  geom_point() +
  geom_smooth(method = glm,  method.args = list(family = "binomial"))

tbl_players %>%
  ggplot(aes(longitude, multi_arrest_num)) +
  geom_point() +
  geom_smooth(method = glm,  method.args = list(family = "binomial"))

tbl_players %>%
  ggplot(aes(population_density, multi_arrest_num)) +
  geom_jitter(width = 0, height = 0.1) +
  geom_smooth(method = glm,  method.args = list(family = "binomial"))

tbl_players %>%
  ggplot(aes(land_area, multi_arrest_num)) +
  geom_jitter(width = 0, height = 0.1) +
  geom_smooth(method = glm,  method.args = list(family = "binomial"))
