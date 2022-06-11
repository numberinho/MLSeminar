library(tidyverse)
library(RMySQL)

do_connection <- function(){
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
market <- dbGetQuery(con,"SELECT pm.*, pc.player_position, pc.player_age FROM datenbank.sorare_publicMarket pm left join sorare_club sc on sc.club_slug = pm.club_slug left join sorare_playercopy pc on pc.player_slug = pm.player_slug where league_slug = 'bundesliga-de' and card_rarity = 'limited'") %>% tibble()
score <- dbGetQuery(con,"select player_slug, score, date from sorare_score") %>% tibble()
dbDisconnect(con)

market <- market %>% mutate(owner_since = as.Date(owner_since))
score <- score %>% mutate(score = ifelse(is.na(score),0,score), date = as.Date(date))

score1 <- score %>% arrange(player_slug, date) %>% group_by(player_slug) %>% mutate(dateEnd = lead(date)) %>% ungroup() %>% filter(!is.na(dateEnd))

market_big <- market %>% 
    left_join(score1, by = "player_slug")

full <- market_big %>% filter(owner_since > date, owner_since < dateEnd)

full %>% filter(player_slug == "sven-ulreich") %>% view

lm(EUR ~ score, data = full %>% filter(player_slug == "christopher-nkunku")) %>% summary()

score %>% filter(player_slug == "christopher-nkunku") %>%
filter(date > "2021-10-21") %>%
ggplot(aes(x=date,y=score))+
geom_line()+
geom_point()+
geom_smooth()+
scale_x_date(date_breaks = "1 month")

market %>% filter(player_slug == "christopher-nkunku") %>%
    arrange(owner_since)



##############################################################################################################


library(tidyverse)
library(RMySQL)

do_connection <- function(){
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
shifted <- dbGetQuery(con, "select * from sorare_gameWeeks") %>% tibble() %>% mutate(continuousEnd = lead(endDate)) %>% mutate(continuousEnd = ifelse(is.na(continuousEnd),endDate,continuousEnd))

sql_qry <- paste0("insert into datenbank.sorare_gameWeeks (gameWeek, startDate, endDate, continuousEnd) values ")
sql_qry <- paste0(sql_qry,
                  paste(sprintf('(%s, "%s", "%s", "%s")',
                                shifted$gameWeek,
                                shifted$startDate,
                                shifted$endDate,
                                shifted$continuousEnd
                  ),
                  collapse = ","),
                  " as new on duplicate key update continuousEnd = new.continuousEnd") %>%
                  str_replace_all("NA","2025-05-03 09:59:59")

dbExecute(con, sql_qry)
dbDisconnect(con)
