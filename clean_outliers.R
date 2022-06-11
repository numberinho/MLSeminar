library(tidyverse)

sorare_data <- feather::read_feather("sorare_data.feather")

# loess function & prediction that can be applied to grouped tibble
customLoess <- function(data){
  if(nrow(data) > 10){
  # define the model
  model <- loess(EUR ~ as.numeric(owner_since), data = data,
                 span = .1)
  
  # predict fitted values for each observation in the original dataset
  return(mutate(data, fit = predict(model, se = F)))
  } else {
    return(mutate(data, fit = EUR))
  }
}

# applies "outlier function" to groups and filter
detect_outliers <- function(data){
  data %>%
    arrange(hms) %>%
    group_by(player_slug, card_rarity) %>%
    group_map(~customLoess(.x),.keep = T) %>%
    bind_rows() %>%
    filter(EUR > fit*0.2, EUR < fit*1.5) %>%
    select(-fit)
}

# run
clean_data <- detect_outliers(sorare_data) %>%
  arrange(hms) %>%
  group_by(hms) %>%
  mutate(timeStamp = row_number()) %>%
  ungroup()

clean_data

feather::write_feather(clean_data, "clean_data.feather")

clean_data %>%
  filter(player_slug == "mats-hummels") %>%
  filter(card_rarity == "limited") %>%
  mutate(lagEUR = lag(EUR)) %>%
  arrange(owner_since) %>%
  #group_by(player_slug, card_rarity) %>%
  #group_map(~customLoess(.x)) %>%
  #bind_rows() %>%
  #filter(EUR > fit*0.1, EUR < fit*1.5) %>%
  ggplot(aes(x=owner_since, y=EUR))+
  #geom_line(aes(y=lagEUR),color="red")+
  geom_line()+
  #geom_smooth(se = F, span=.15)+
  #geom_line(aes(y=fit))+
  scale_x_date(breaks="2 week")+
  theme(axis.text.x = element_text(angle=20))


