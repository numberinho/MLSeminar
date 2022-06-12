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
    mutate(timeStamp = row_number())
}

market_globals <- get_market_globals(data_no_outliers)

# function to fix the score per gameweek
fix_score <- function(gameWeeks, score) {
  crossing(gameWeeks, distinct(score, player_slug)) %>%
    arrange(gameWeek) %>%
    left_join(score, by = c("gameWeek", "player_slug")) %>%
    group_by(player_slug) %>%
    mutate(daysSinceLastScore = ifelse(is.na(score), NA, endDate)) %>%
    mutate(lastScore = score, lastMins = mins_played) %>%
    mutate(
      cumScore = replace_na(score, 0),
      cumMins = replace_na(mins_played, 0)
    ) %>%
    mutate(
      cumScore = cumsum(cumScore),
      cumMins = cumsum(cumMins)
    ) %>%
    fill(daysSinceLastScore, .direction = "down") %>%
    fill(lastScore, .direction = "down") %>%
    fill(lastMins, .direction = "down") %>%
    mutate(daysSinceLastScore = as.Date(daysSinceLastScore, "1970-01-01")) %>%
    mutate(daysSinceLastScore = as.numeric(endDate - daysSinceLastScore)) %>%
    ungroup() %>%
    select(gameWeek, daysSinceLastScore, player_slug, lastScore, lastMins, cumScore, cumMins)
}

fixed_score <- fix_score(gameWeeks, score)

# put all together

get_mean_data <- function(data){
  data %>%
    group_by(player_slug, club_slug, owner_since, gameWeek, player_position, player_age) %>%
    summarise(EUR = mean(EUR),
              lastPlayerTrades = n()) %>%
    ungroup() %>%
    mutate(daysSinceLastTrade = owner_since, lastPlayerTrades = ifelse(is.na(lastPlayerTrades),0,lastPlayerTrades))
}

mean_prices <- get_mean_data(data_no_outliers)

fill_missing_down <- function(data){
  crossing(
    data %>% distinct(owner_since),
    data %>% distinct(player_slug)
  ) %>%
    left_join(data, by = c("owner_since", "player_slug")) %>%
    group_by(player_slug) %>%
    fill(everything(), .direction="down") %>% 
    ungroup() %>% 
    mutate(daysSinceLastTrade = as.numeric(owner_since-daysSinceLastTrade)) %>%
    mutate(lastPlayerTrades = ifelse(daysSinceLastTrade > 0, 0, lastPlayerTrades)) %>%
    filter(!is.na(EUR))
}

imputed_data <- fill_missing_down(mean_prices)

agg_data <- imputed_data %>%
  left_join(market_globals %>% mutate(owner_since = owner_since + 1), by = "owner_since") %>% # offset "last"
  mutate(
    day = lubridate::day(owner_since),
    month = lubridate::month(owner_since),
    quarter = lubridate::quarter(owner_since),
    year = lubridate::year(owner_since)
  ) %>%
  group_by(player_slug) %>%
  arrange(owner_since) %>%
  mutate(daysSinceLastTrade = lag(daysSinceLastTrade),
         lastPlayerTrades = lag(lastPlayerTrades),
         lastTotalTrades = lag(lastTotalTrades)) %>%
  ungroup() %>%
  mutate(gameWeek = gameWeek - 1) %>%
  left_join(fixed_score, by = c("gameWeek", "player_slug")) %>%
  mutate(
    lastPlayerTrades = replace_na(lastScore, 0),
    lastScore = replace_na(lastScore, 0),
    lastTotalTrades = replace_na(lastTotalTrades, 0),
    lastMins = replace_na(lastMins, 0),
    cumScore = replace_na(cumScore, 0),
    cumMins = replace_na(cumMins, 0)
  ) %>%
  select(-gameWeek)

# compute lagged EUR values
compute_lagged <- function(data) {
  means <- data %>%
    group_by(player_slug, owner_since) %>%
    summarise(EUR_mean = mean(EUR)) %>%
    arrange(owner_since) %>%
    mutate(
      EUR_lag_1 = lag(EUR_mean),
      EUR_lag_2 = lag(EUR_mean, 2),
      EUR_lag_3 = lag(EUR_mean, 3),
      EUR_lag_4 = lag(EUR_mean, 4),
      EUR_lag_5 = lag(EUR_mean, 5),
      EUR_lag_6 = lag(EUR_mean, 6),
      EUR_lag_7 = lag(EUR_mean, 7)
    ) %>%
    ungroup() %>%
    select(-EUR_mean)
  
  data %>%
    left_join(means, by = c("owner_since", "player_slug"))
}

# final clean dataframe
clean_data <- compute_lagged(agg_data)

feather::write_feather(clean_data, "clean_data.feather")


## TODO put in one file
# check iago

p <- sample_n(test_results %>% distinct(player_slug), 1)$player_slug
clean_data %>%
  #filter(player_slug == p) %>%
  group_by(owner_since, player_slug) %>%
  summarise(mean = mean(EUR)) %>%
  ungroup() %>%
  ggplot(aes(x=owner_since,y=mean))+
  geom_line()

transfer %>%
  filter(player_slug == sample_n(transfer %>% distinct(player_slug), 1)$player_slug) %>%
  ggplot(aes(x=hms,y=EUR))+
  geom_line()+
  scale_x_datetime(breaks= "2 week")+
  theme(axis.text.x = element_text(angle=40))

clean_data %>%
  drop_na() %>%
  filter(player_slug == sample_n(clean_data %>% distinct(player_slug), 1)$player_slug) %>%
  ggplot(aes(x=owner_since,y=EUR))+
  geom_line()+
  scale_x_date(breaks= "2 week")+
  theme(axis.text.x = element_text(angle=40))

