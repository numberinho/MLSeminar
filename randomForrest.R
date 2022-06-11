library(tidyverse)
library(tidymodels)

if (Sys.info()["nodename"] == "Simons-MacBook-Pro.local") {
  clean_data <- feather::read_feather("clean_data.feather") %>%
    mutate(
      day = lubridate::day(owner_since),
      month = lubridate::month(owner_since),
      quarter = lubridate::quarter(owner_since),
      year = lubridate::year(owner_since)
    )
} else {
  clean_data <- feather::read_feather("Seminar/clean_data.feather") %>%
    mutate(
      day = lubridate::day(owner_since),
      month = lubridate::month(owner_since),
      quarter = lubridate::quarter(owner_since),
      year = lubridate::year(owner_since)
    )
}

clean_data %>% distinct(player_slug) %>% sample_n(200)

sample <- distinct(clean_data, player_slug) %>% sample_n(200)

data_train <- clean_data %>% inner_join(sample) %>% filter(owner_since < "2022-04-01")
data_test <- clean_data %>% inner_join(sample) %>% filter(owner_since >= "2022-04-01")

rf_settings <- rand_forest(mode = "regression", mtry = 3, trees = 500) %>%
  set_engine("ranger")

randomForrestFit <-
  rf_settings %>%
  fit(
    log10(EUR) ~ .,
    data = data_train
  )

test_results <-
  data_test %>%
  filter(player_slug %in% data_train$player_slug) %>%
  bind_cols(
    predict(randomForrestFit, new_data = data_test %>% filter(player_slug %in% data_train$player_slug))
  )

test_results %>% metrics(truth = EUR, estimate = .pred)

data_test %>% distinct(player_slug) %>% pull()

v <- "sven-michel"

test_results %>%
  filter(player_slug == v) %>%
  filter(card_rarity == "limited") %>%
  ggplot(aes(x = owner_since)) +
  geom_line(aes(y = EUR), color = "#7f3030") +
  geom_line(aes(y = 10^.pred))



