library(tidyverse)
library(tidymodels)

clean_data <- feather::read_feather("clean_data.feather")

# sample <- distinct(clean_data, player_slug) %>% sample_n(300)
sample <- distinct(clean_data, player_slug)

data_train <- clean_data %>%
  inner_join(sample) %>%
  filter(owner_since < "2022-04-01")
data_test <- clean_data %>%
  inner_join(sample) %>%
  filter(owner_since > "2022-04-01")

rf_settings <- rand_forest(mode = "regression", mtry = 3, trees = 500) %>%
  set_engine("ranger")

randomForrestFit <-
  rf_settings %>%
  fit(
    log10(EUR) ~     
    + player_slug
    + club_slug
    + player_position
    + player_age
    + card_shirt
    + owner_number
    + last_scoreDays
    + lastScore
    + lastMins
    + cumScore
    + cumMins
    + day1_meanEUR,
    + eth_exchange
    + trades
    + timeStamp
    + day
    + month
    + quarter
    + year,
    data = data_train
  )

test_results <-
  data_test %>%
  filter(player_slug %in% randomForrestFit$preproc$xlevels$player_slug) %>%
  bind_cols(
    predict(randomForrestFit, new_data = data_test %>% filter(player_slug %in% randomForrestFit$preproc$xlevels$player_slug))
  )

test_results %>% metrics(truth = EUR, estimate = .pred)

data_test %>%
  distinct(player_slug) %>%
  pull()

test_results %>%
  bind_rows(data_train) %>%
  filter(player_slug == sample_n(test_results %>% distinct(player_slug), 1)$player_slug) %>%
  filter(card_rarity == "limited") %>%
  ggplot(aes(x = hms, color = player_slug)) +
  geom_line(aes(y = EUR), color = "#7f3030") +
  geom_line(aes(y = 10^.pred)) +
  scale_x_datetime(breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 90))

test_results %>%
  bind_rows(data_train) %>%
  filter(player_slug == "philipp-lienhart") %>%
  filter(card_rarity == "limited")

data_train %>% filter(player_slug == "philipp-lienhart")

clean_data %>% filter(player_slug == "danny-blum")