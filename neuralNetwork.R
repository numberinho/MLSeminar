library(tidyverse)
library(tidymodels)

clean_data <- feather::read_feather("clean_data.feather")

clean_data %>% write_csv("clean_data.csv")

# sample <- distinct(clean_data, player_slug) %>% sample_n(300)
sample <- distinct(clean_data, player_slug)

data_train <- clean_data %>%
  inner_join(sample) %>%
  filter(owner_since < "2022-04-01")
data_test <- clean_data %>%
  inner_join(sample) %>%
  filter(owner_since > "2022-04-01")

nn_preprocessing <- 
  recipe(
    EUR ~
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
    + EUR_lag_1
    + EUR_lag_2
    + EUR_lag_3
    + EUR_lag_4
    + EUR_lag_5
    + EUR_lag_6
    + EUR_lag_7
    + eth_exchange
    + trades
    + timeStamp
    + day
    + month
    + quarter
    + year,
    data = data_train
  ) %>%
  step_dummy(c(club_slug, player_slug, player_position)) %>%
  step_normalize(
    c(
      EUR_lag_1,
      EUR_lag_2,
      EUR_lag_3,
      EUR_lag_4,
      EUR_lag_5,
      EUR_lag_6,
      EUR_lag_7,
      card_shirt,
      player_age,
      owner_number,
      last_scoreDays,
      lastScore,
      lastMins,
      cumScore,
      cumMins,
      eth_exchange,
      trades,
      timeStamp,
      day,
      month,
      quarter,
      year
    )
  ) %>%
  prep(training = data_train, retain = TRUE)

# For validation:
val_normalized <- bake(nn_preprocessing, new_data = data_train, all_predictors())
# For testing when we arrive at a final model: 
test_normalized <- bake(nn_preprocessing, new_data = data_test %>% filter(player_slug %in% data_train$player_slug), all_predictors())

nnet_fit <-
  mlp(epochs = 100, hidden_units = 5, dropout = 0.1) %>%
  set_mode("regression") %>% 
  # Also set engine-specific `verbose` argument to prevent logging the results: 
  set_engine("keras", verbose = 1) %>%
  fit(EUR ~ ., data = bake(nn_preprocessing, new_data = NULL))


data_test
val_results <- 
  data_test %>% filter(player_slug %in% data_train$player_slug) %>%
  mutate(prediction = predict(nnet_fit, new_data = test_normalized)) %>%
  bind_rows(data_train)

val_results %>% slice(1:5)


val_results %>% head(10) %>%
  unnest()




val_results %>%
  unnest(cols = c(prediction)) %>%
  filter(player_slug == sample_n(val_results %>% distinct(player_slug), 1)$player_slug) %>%
  filter(card_rarity == "limited") %>%
  ggplot(aes(x = hms, color = player_slug)) +
  geom_line(aes(y = EUR), color = "#7f3030") +
  geom_line(aes(y = .pred)) +
  scale_x_datetime(breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 90))

test_results %>%
  bind_rows(data_train) %>%
  filter(player_slug == "philipp-lienhart") %>%
  filter(card_rarity == "limited")

data_train %>% filter(player_slug == "philipp-lienhart")

clean_data %>% filter(player_slug == "danny-blum")