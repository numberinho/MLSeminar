library(tidyverse)
library(RMySQL)

source("sql_queries.R")

do_connection <- function(db) {
  host <- ifelse(Sys.info()["user"] == "simonschoning", "127.0.0.1", "localhost")
  
  con <- RMySQL::dbConnect(RMySQL::MySQL(),
                           user = "root", password = "Wh3Ku2EidF",
                           dbname = "datenbank",
                           host = host,
                           port = 3308,
                           encoding = "UTF-8"
  )
  
  DBI::dbGetQuery(con, "SET NAMES 'utf8mb4'")
  
  return(con)
}

con <- do_connection()
transfer <- dbGetQuery(con, transfermarkt_sql) %>%
  tibble() %>%
  mutate(hms = as.POSIXct(owner_since, format = "%Y-%m-%d %H:%M:%S"), owner_since = as.Date(owner_since))
score <- dbGetQuery(con, score_sql) %>% tibble()
gameWeeks <- dbGetQuery(con, gameWeeks_sql) %>%
  tibble() %>%
  mutate(endDate = as.Date(endDate))
dbDisconnect(con)

lapply(1:(nrow(gameWeeks)-1), function(i){
  tibble(gameWeek = gameWeeks[[1]][i], owner_since = as.Date(gameWeeks[[2]][i]:(gameWeeks[[2]][i+1]-1),"1970-01-01"))
}) -> res

gameweek_long <- res %>% bind_rows()

# clean outliers

customLoess <- function(data) {
  if (nrow(data) > 10) {
    # define the model
    model <- loess(EUR ~ as.numeric(owner_since),
                   data = data,
                   span = .1
    )
    
    # predict fitted values for each observation in the original dataset
    return(mutate(data, fit = predict(model, se = F)))
  } else {
    return(mutate(data, fit = EUR))
  }
}

# applies "outlier function" to groups and filter
detect_outliers <- function(data) {
  data %>%
    arrange(hms) %>%
    group_by(player_slug, card_rarity) %>%
    group_map(~ customLoess(.x), .keep = T) %>%
    bind_rows() %>%
    filter(EUR > fit * 0.2 | card_serial == 1) %>%
    filter(EUR < fit * 1.5 | card_serial == 1) %>%
    select(-fit)
}

data_no_outliers <- transfer %>% detect_outliers()

# function to calculate market globals
get_market_globals <- function(transfer) {
  transfer %>%
    filter(EUR > 0, ETH > 0) %>%
    group_by(owner_since) %>%
    summarise(
      eth_exchange = mean(EUR / ETH),
      lastTotalTrades = n()
    ) %>%
    ungroup() %>%
    arrange(owner_since) %>%
    mutate(timeStamp = row_number(),
           owner_since = owner_since+1)
}

market_globals <- get_market_globals(data_no_outliers)

get_mean_data <- function(data){
  tmp <- data %>%
    group_by(player_slug, club_slug, owner_since, player_position, player_age) %>%
    summarise(EUR = mean(EUR),
              playerTrades = n()) %>%
    ungroup()
  
  crossing(
    tmp %>% distinct(owner_since),
    tmp %>% distinct(player_slug)
  ) %>%
    left_join(tmp, by = c("owner_since", "player_slug")) %>%
    mutate(daysSinceLastTrade = ifelse(!is.na(EUR),owner_since,NA)) %>%
    group_by(player_slug) %>%
    arrange(owner_since) %>%
    fill(everything(), .direction="down") %>% 
    ungroup() %>% 
    mutate(daysSinceLastTrade = as.numeric(owner_since-daysSinceLastTrade)) %>%
    mutate(playerTrades = ifelse(daysSinceLastTrade > 0, 0, playerTrades)) %>%
    filter(!is.na(EUR))
}

aggregated_data <- get_mean_data(data_no_outliers)

# function to fix the score per gameweek
fix_score <- function(gameWeeks, score) {
  crossing(gameWeeks, score %>% distinct(player_slug)) %>% #score for every player every gameweek
    arrange(gameWeek) %>%
    left_join(score, by = c("gameWeek", "player_slug")) %>%
    mutate(didScore = ifelse(is.na(score),0,1)) %>%
    mutate(daysSinceLastScore = ifelse(is.na(score), NA, endDate)) %>%
    group_by(player_slug) %>%
    arrange(gameWeek) %>%
    mutate(
      cumScore = replace_na(score, 0),
      cumMins = replace_na(mins_played, 0)
    ) %>%
    mutate(
      cumScore = cumsum(cumScore),
      cumMins = cumsum(cumMins)
    ) %>%
    fill(daysSinceLastScore, .direction = "down") %>%
    fill(score, .direction = "down") %>%
    fill(mins_played, .direction = "down") %>%
    mutate(daysSinceLastScore = as.Date(daysSinceLastScore, "1970-01-01")) %>%
    mutate(daysSinceLastScore = as.numeric(endDate - daysSinceLastScore)) %>%
    ungroup() %>%
    filter(!is.na(score)) %>%
    mutate(gameWeek = gameWeek+1) %>%
    select(gameWeek, daysSinceLastScore, player_slug, lastScore = score, lastMins = mins_played, cumScore, cumMins)
}

fixed_score <- fix_score(gameWeeks, score)

# put all together

clean_data <- aggregated_data %>%
  left_join(gameweek_long, by = "owner_since") %>%
  left_join(market_globals, by = "owner_since") %>%
  left_join(fixed_score, by = c("player_slug", "gameWeek")) %>%
  mutate(
    day = lubridate::day(owner_since),
    month = lubridate::month(owner_since),
    quarter = lubridate::quarter(owner_since),
    year = lubridate::year(owner_since)
  ) %>%
  rename(EUR_lag_0 = EUR) %>%
  group_by(player_slug) %>%
  arrange(owner_since) %>%
  mutate(
    EUR_lag_1 = lag(EUR_lag_0),
    EUR_lag_2 = lag(EUR_lag_0, 2),
    EUR_lag_3 = lag(EUR_lag_0, 3),
    EUR_lag_4 = lag(EUR_lag_0, 4),
    EUR_lag_5 = lag(EUR_lag_0, 5),
    EUR_lag_6 = lag(EUR_lag_0, 6),
    EUR_lag_7 = lag(EUR_lag_0, 7)
  ) %>%
  ungroup() %>%
  mutate(
    EUR_up_0 = ifelse(EUR_lag_1 < EUR_lag_0, 1, 0),
    EUR_up_1 = ifelse(EUR_lag_2 < EUR_lag_1, 1, 0),
    EUR_up_2 = ifelse(EUR_lag_3 < EUR_lag_2, 1, 0),
    EUR_up_3 = ifelse(EUR_lag_4 < EUR_lag_3, 1, 0),
    EUR_up_4 = ifelse(EUR_lag_5 < EUR_lag_4, 1, 0),
    EUR_up_5 = ifelse(EUR_lag_6 < EUR_lag_5, 1, 0),
    EUR_up_6 = ifelse(EUR_lag_7 < EUR_lag_6, 1, 0),
    EUR_up_0 = as.factor(EUR_up_0),
    EUR_up_1 = as.factor(EUR_up_1),
    EUR_up_2 = as.factor(EUR_up_2),
    EUR_up_3 = as.factor(EUR_up_3),
    EUR_up_4 = as.factor(EUR_up_4),
    EUR_up_5 = as.factor(EUR_up_5),
    EUR_up_6 = as.factor(EUR_up_6)
  ) %>%
  ungroup() %>%
  drop_na() %>%
  select(-gameWeek)

feather::write_feather(clean_data, "clean_data.feather")


clean_data %>%
  filter(player_slug == sample_n(clean_data %>% distinct(player_slug), 1)$player_slug) %>%
  ggplot(aes(x=owner_since,y=EUR_lag_0))+
  geom_line()+
  scale_x_date(breaks= "2 week")+
  theme(axis.text.x = element_text(angle=40))




