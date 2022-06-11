library(tidyverse)
library(tidymodels)

clean_data <- feather::read_feather("Seminar/clean_data.feather") %>%
  mutate(
    day = lubridate::day(owner_since),
    month = lubridate::month(owner_since),
    quarter = lubridate::quarter(owner_since),
    year = lubridate::year(owner_since)
  )


data_split <- initial_split(clean_data, prop = 0.80)

data_train <- training(data_split)
data_test <- testing(data_split)

rf_settings <- rand_forest(mode = "regression", mtry = .preds(), trees = 500) %>%
  set_engine("ranger")

randomForrestFit <-
  rf_settings %>%
  fit(
    log10(EUR) ~ .,
    data = data_train
  )

test_results <-
  ames_test %>%
  bind_cols(
    predict(rf_xy_fit, new_data = ames_test %>% select(timeStamp, player_slug, card_rarity, player_position))
  )

test_results %>% slice(1:5)

test_results %>% metrics(truth = EUR, estimate = .pred)

v <- "mats-hummels"

test_results %>%
  filter(player_slug == v) %>%
  filter(card_rarity == "limited") %>%
  ggplot(aes(x = owner_since)) +
  geom_line(aes(y = log10(EUR)), color = "#7f3030") +
  geom_line(aes(y = .pred))