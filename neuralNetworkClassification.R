library(tidyverse)
library(tidymodels)

clean_data <- feather::read_feather("clean_data.feather")

clean_data <- clean_data %>%
  group_by(player_slug) %>%
  arrange(owner_since) %>%
  mutate(goesUP = ifelse(lag(EUR) < EUR,T,F)) %>%
  mutate(goesUP = as.factor(goesUP)) %>%
  select(-EUR)

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
      goesUP ~
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
    ) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    prep(training = data_train, retain = TRUE)

# For validation:
val_normalized <- bake(nn_preprocessing, new_data = data_train, all_predictors())
# For testing when we arrive at a final model:
test_normalized <- bake(nn_preprocessing, new_data = data_test %>% filter(player_slug %in% data_train$player_slug), all_predictors())

nnet_fit <-
    mlp(epochs = 100, hidden_units = 20, dropout = 0.1) %>%
    set_mode("classification") %>%
    # Also set engine-specific `verbose` argument to prevent logging the results:
    set_engine("keras", verbose = 1) %>%
    fit(goesUP ~ ., data = bake(nn_preprocessing, new_data = NULL))

model <- keras::load_model_tf("model")

summary(model)

data_test
val_results <-
    data_test %>%
  ungroup() %>%
    filter(player_slug %in% data_train$player_slug) %>%
    mutate(prediction = predict(nnet_fit, new_data = test_normalized)) %>%
    bind_rows(data_train)

val_results %>% slice(1:5)


val_results %>%
    head(10) %>%
    unnest()

val_results %>%
  unnest(cols = c(prediction))  %>%
  mutate(right = goesUP == .pred_class) %>% filter(is.na(right)) %>% head(1000) %>% view
  count(goesUP == .pred_class) 



val_results %>%
    unnest(cols = c(prediction)) %>%
    filter(player_slug == sample_n(val_results %>% distinct(player_slug), 1)$player_slug) %>%
    ggplot(aes(x = owner_since, color = player_slug)) +
    geom_line(aes(y = .pred)) +
    geom_line(aes(y = EUR), color = "#7f3030") +
    scale_x_date(breaks = "1 week") +
    theme(axis.text.x = element_text(angle = 90))

test_results %>%
    bind_rows(data_train) %>%
    filter(player_slug == "philipp-lienhart") %>%
    filter(card_rarity == "limited")

data_train %>% filter(player_slug == "philipp-lienhart")

clean_data %>% filter(player_slug == "danny-blum")