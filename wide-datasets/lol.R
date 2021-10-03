library(tidyverse)
library(readxl)
library(tidyjson)
library(rvest)

# get json from webpage with rvest read_html
page <- read_html('https://ddragon.leagueoflegends.com/cdn/11.19.1/data/en_US/champion.json')

# parse the html with rvest, return just the contents of the <body> element
json <- page %>% html_elements("body") %>% html_text()

# check out the structure
json %>% gather_object %>% json_types

# get only the 'data' object from the json and spread_all into a 
# json table, then convert back to regular dataframe
champions <- json %>% 
  enter_object(data) %>% 
  gather_object %>% 
  spread_all %>% 
  as_data_frame.tbl_json 


df <- champions %>% 
  select(c('name','stats.hp','stats.mp','stats.movespeed','stats.armor'))

ggplot(df) +
 aes(x = stats.hp) +
 geom_histogram(binwidth = 10, color = "#112446", fill="#ffffff") +
 theme_minimal()

ggplot(df) +
  aes(x = stats.mp) +
  geom_histogram(binwidth=50, color = "#112446", fill="#ffffff") +
  theme_minimal()

ggplot(df) +
  aes(x = stats.armor) +
  geom_histogram(binwidth=2, color = "#112446", fill="#ffffff") +
  theme_minimal()

ggplot(df) +
  aes(stats.hp, stats.armor) +
  geom_point() +
  theme_minimal()
