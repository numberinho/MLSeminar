library(tidyverse)
library(tidymodels)

clean_data <- feather::read_feather("clean_data.feather")

model_data <- clean_data %>% select(-EUR_lag_0) # contains the actual sales price

initial_split <- initial_split(model_data, prop = 0.75)

train_data <- training(initial_split)
train_data <- model_data %>% filter(owner_since < "2022-04-01")

test_data <- testing(initial_split)
test_data <- model_data %>% filter(owner_since >= "2022-04-01")

cores <- parallel::detectCores()

rf_settings <- 
  rand_forest(mtry = 5, trees = 501) %>%
  set_engine("ranger", num.threads = cores) %>%
  set_mode("classification")

randomForrestFit <-
  rf_settings %>%
  fit(
    EUR_up_0 ~              # did the price rise?
      + player_slug         # name of player
    + club_slug           # name of club
    + player_position     # position of that player (goalkeeper, midfielder...)
    + player_age          # age of the player
    + daysSinceLastTrade  # days since last Trade
    + eth_exchange        # ETH - EUR exchange rate
    + lastTotalTrades     # last day total Trades of all players
    + daysSinceLastScore  # days since last score of the player
    + lastScore           # last score of the player
    + lastMins            # last mins on pitch of the player
    + cumScore            # total scored points cumulated
    + cumMins             # total mins on the pitch cumulated
    + timeStamp           # continous integer representing time
    + day                 # continous representing time
    + month               # continous representing time
    + quarter             # continous representing time
    + year                # continous representing time
    + EUR_lag_1           # price 1 day ago
    + EUR_lag_2           # price 2 days ago
    + EUR_lag_3           # price 3 days ago
    + EUR_lag_4           # price 4 days ago
    + EUR_lag_5           # price 5 days ago
    + EUR_lag_6           # price 6 days ago
    + EUR_lag_7           # price 7 days ago
    + EUR_up_1            # dummy variable if the price is higher than day before (lagged 1 day)
    + EUR_up_2            # dummy variable if the price is higher than day before (lagged 2 days)
    + EUR_up_3            # dummy variable if the price is higher than day before (lagged 3 days)
    + EUR_up_4            # dummy variable if the price is higher than day before (lagged 4 days)
    + EUR_up_5            # dummy variable if the price is higher than day before (lagged 5 days)
    + EUR_up_6            # dummy variable if the price is higher than day before (lagged 6 days)
    ,data = train_data
  )

tree_rec <- recipe(
  EUR_up_0 ~              # did the price rise?
    + player_slug         # name of player
  + club_slug           # name of club
  + player_position     # position of that player (goalkeeper, midfielder...)
  + player_age          # age of the player
  + daysSinceLastTrade  # days since last Trade
  + eth_exchange        # ETH - EUR exchange rate
  + lastTotalTrades     # last day total Trades of all players
  + daysSinceLastScore  # days since last score of the player
  + lastScore           # last score of the player
  + lastMins            # last mins on pitch of the player
  + cumScore            # total scored points cumulated
  + cumMins             # total mins on the pitch cumulated
  + timeStamp           # continous integer representing time
  + day                 # continous representing time
  + month               # continous representing time
  + quarter             # continous representing time
  + year                # continous representing time
  + EUR_lag_1           # price 1 day ago
  + EUR_lag_2           # price 2 days ago
  + EUR_lag_3           # price 3 days ago
  + EUR_lag_4           # price 4 days ago
  + EUR_lag_5           # price 5 days ago
  + EUR_lag_6           # price 6 days ago
  + EUR_lag_7           # price 7 days ago
  + EUR_up_1            # dummy variable if the price is higher than day before (lagged 1 day)
  + EUR_up_2            # dummy variable if the price is higher than day before (lagged 2 days)
  + EUR_up_3            # dummy variable if the price is higher than day before (lagged 3 days)
  + EUR_up_4            # dummy variable if the price is higher than day before (lagged 4 days)
  + EUR_up_5            # dummy variable if the price is higher than day before (lagged 5 days)
  + EUR_up_6            # dummy variable if the price is higher than day before (lagged 6 days)
  ,data = train_data
) %>%
  step_dummy(all_nominal(), -all_outcomes())

tree_prep <- prep(tree_rec)
juiced <- juice(tree_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")


tune_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(tune_spec)

set.seed(234)
trees_folds <- vfold_cv(train_data)

doParallel::registerDoParallel()

set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = 20
)

tune_res




######

test_results <-
  test_data %>%
  filter(player_slug %in% randomForrestFit$preproc$xlevels$player_slug) %>%
  bind_cols(
    predict(randomForrestFit, new_data = test_data %>% filter(player_slug %in% randomForrestFit$preproc$xlevels$player_slug))
  )


test_results %>% metrics(truth = EUR_up_0, estimate = .pred_class)


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
  mutate(
    goesUP = ifelse(EUR_lag_1 < EUR, 1, 0),
    pgoesUP = ifelse(EUR_lag_1 < .pred, 1, 0)
  ) %>%
  count(goesUP == pgoesUP) %>%
  drop_na() %>%
  mutate(n = n / sum(n))

data_test %>% filter(player_slug == "mats-hummels") %>% head(1) %>%
  mutate(.pred = predict(randomForrestFit))

glob = data_test %>% filter(player_slug == "mats-hummels") %>% head(1);
lapply(0:10, function(i){
  tmp <- glob
  glob$EUR_lag_7 <<- glob$EUR_lag_6
  glob$EUR_lag_6 <<- glob$EUR_lag_5
  glob$EUR_lag_5 <<- glob$EUR_lag_4
  glob$EUR_lag_4 <<- glob$EUR_lag_3
  glob$EUR_lag_3 <<- glob$EUR_lag_2
  glob$EUR_lag_2 <<- glob$EUR_lag_1
  glob$.pred <<- 10^predict(randomForrestFit, glob)$.pred
  glob$EUR_lag_1 <<- glob$.pred
  return(tmp)
}) -> res

res %>% bind_rows() %>% view
ggplot(aes(x=owner_since, y = .pred))+
  geom_line()




