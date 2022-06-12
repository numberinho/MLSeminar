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
  filter(owner_since >= "2022-04-01")

rf_settings <- rand_forest(mode = "regression", mtry = 5, trees = 501) %>%
  set_engine("ranger")

randomForrestFit <-
  rf_settings %>%
  fit(
    log10(EUR) ~
    +player_slug
    + club_slug
      + player_position
      + player_age
      + daysSinceLastScore # days past last score
      + lastScore # last scored points
      + lastMins # last played minutes
      + cumScore # cumulated score until timepoint
      + cumMins # cumulated minutes until timepoint
      + EUR_lag_1
      + EUR_lag_2
      + EUR_lag_3
      + EUR_lag_4
      + EUR_lag_5
      + EUR_lag_6
      + EUR_lag_7
      + eth_exchange # ETH - EUR exchange ratio
      + lastPlayerTrades # amount of trades of this player yesterday
      + daysSinceLastTrade # days since last trade of this player
      + lastTotalTrades # amount of trades yesterday
      + timeStamp # integer representing time
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
  ) %>%
  mutate(.pred = 10^.pred)

test_results %>% metrics(truth = EUR, estimate = .pred)


test_results %>%
  bind_rows(data_train) %>%
  filter(!is.na(EUR)) %>%
  filter(player_slug == sample_n(test_results %>% distinct(player_slug), 1)$player_slug) %>%
  ggplot(aes(x = owner_since, color = player_slug)) +
  geom_line(aes(y = EUR), color = "#7f3030") +
  geom_line(aes(y = .pred)) +
  scale_x_date(breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 90))


# simple metric
test_results %>%
  # filter(player_slug == sample_n(test_results %>% distinct(player_slug), 1)$player_slug) %>%
  group_by(player_slug) %>%
  arrange(owner_since) %>%
  mutate(
    goesUP = ifelse(EUR_lag_1 < EUR, 1, 0),
    pgoesUP = ifelse(EUR_lag_1 < .pred, 1, 0)
  ) %>%
  ungroup() %>%
  count(goesUP == pgoesUP) %>%
  drop_na() %>%
  mutate(n = n / sum(n))