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



data_split <- initial_split(clean_data %>% arrange(player_slug) %>% head(100000) %>% drop_na(), prop = 0.75)

data_train <- training(data_split)
data_test <- testing(data_split)

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
  bind_cols(
    predict(randomForrestFit, new_data = data_test)
  )

test_results  %>% pull(player_slug)

test_results %>% metrics(truth = EUR, estimate = .pred)

v <- "alassane-plea" 

test_results %>%
  filter(player_slug == v) %>%
  filter(card_rarity == "limited") %>%
  ggplot(aes(x = owner_since)) +
  geom_line(aes(y = EUR), color = "#7f3030") +
  geom_line(aes(y = 10^.pred))



