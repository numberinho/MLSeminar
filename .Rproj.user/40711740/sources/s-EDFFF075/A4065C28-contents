library(tidyverse)
library(RMySQL)

source("sql_queries.R")

do_connection <- function(db){
  host <- ifelse(Sys.info()["user"] == "simonschoning", "127.0.0.1", "localhost")
  
  con <- RMySQL::dbConnect(RMySQL::MySQL(),
                           user="root", password="Wh3Ku2EidF",
                           dbname="datenbank",
                           host=host,
                           port = 3308,
                           encoding = "UTF-8")
  
  DBI::dbGetQuery(con, "SET NAMES 'utf8mb4'")
  
  return(con)
}

con <- do_connection()
transfer <- dbGetQuery(con, transfermarkt_sql) %>% tibble() %>% mutate(hms = as.POSIXct(owner_since,format="%Y-%m-%d %H:%M:%S"), owner_since = as.Date(owner_since))
score <- dbGetQuery(con, score_sql) %>% tibble()
gameWeeks <- dbGetQuery(con, gameWeeks_sql) %>% tibble() %>% mutate(endDate = as.Date(endDate))
dbDisconnect(con)

get_market_globals <- function(transfer){
  transfer %>%
    filter(EUR > 0, ETH > 0) %>%
    group_by(owner_since) %>%
    summarise(eth_exchange = mean(EUR/ETH),
              trades = n()) %>%
    ungroup() %>%
    arrange(owner_since) %>%
    mutate(timeStamp = row_number())
}

market_globals <- get_market_globals(transfer)

fix_score <- function(gameWeeks, score){
  crossing(gameWeeks,distinct(score, player_slug)) %>%
    arrange(gameWeek) %>%
    left_join(score, by = c("gameWeek", "player_slug")) %>%
    group_by(player_slug) %>%
    mutate(last_scoreDays = ifelse(is.na(score),NA,endDate)) %>%
    mutate(lastScore = score, lastMins = mins_played) %>%
    mutate(cumScore = replace_na(score, 0),
           cumMins = replace_na(mins_played, 0)) %>%
    mutate(cumScore = cumsum(cumScore),
           cumMins = cumsum(cumMins)) %>%
    fill(last_scoreDays, .direction = "down") %>%
    fill(lastScore, .direction = "down") %>%
    fill(lastMins, .direction = "down") %>%
    mutate(last_scoreDays = as.Date(last_scoreDays,"1970-01-01")) %>%
    mutate(last_scoreDays = as.numeric(endDate-last_scoreDays)) %>%
    ungroup() %>%
    select(gameWeek,last_scoreDays, player_slug, lastScore, lastMins, cumScore, cumMins)
}


fixed_score <- fix_score(gameWeeks, score)

sorare_data <- transfer %>%
  mutate(gameWeek = gameWeek-1) %>%
  left_join(fixed_score, by = c("gameWeek", "player_slug")) %>%
  mutate(lastScore = replace_na(lastScore, 0),
         lastMins = replace_na(lastMins, 0),
         cumScore = replace_na(cumScore, 0),
         cumMins = replace_na(cumMins, 0)) %>%
  left_join(market_globals %>% mutate(owner_since = owner_since+1), by = "owner_since") %>% #offset "last"
  mutate(
    day = lubridate::day(owner_since),
    month = lubridate::month(owner_since),
    quarter = lubridate::quarter(owner_since),
    year = lubridate::year(owner_since)
  ) %>%
  select(-player_opta_uuid)

source("clean_outliers.R")

clean_data <- cleanup_data(sorare_data)

feather::write_feather(clean_data, "clean_data.feather")


## TODO put in one file
#check iago



