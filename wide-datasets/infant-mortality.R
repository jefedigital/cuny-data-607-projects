library(tidyverse)

# https://data.worldbank.org/indicator/SH.DYN.MORT


df_mort <- read_csv('data/worldbank/API_SH.DYN.MORT_DS2_en_csv_v2_3012069.csv', skip=4)
df_meta <- read_csv('data/worldbank/Metadata_Country_API_SH.DYN.MORT_DS2_en_csv_v2_3012069.csv')


# cleanup
df_mort <- df_mort %>% 
  select(!('2020':'...66')) %>% 
  select(!('Indicator Name':'Indicator Code')) %>% 
  rename(Name = 'Country Name', Code = 'Country Code')

df_meta <- df_meta %>%
  select(!SpecialNotes) %>%
  select(!last_col())

# impute missing Region values
df_meta <- df_meta %>% 
  mutate(`Record Type` = ifelse(is.na(Region),'Region','Country')) %>%
  mutate(Region = ifelse(is.na(Region), TableName, Region)) %>% 
  rename(Code = 'Country Code') %>%
  select(!TableName)

# pivot
df_mort <- df_mort %>% 
  pivot_longer(!c('Name','Code'), 
               names_to = 'Year', 
               values_to = 'Mortality Rate')

# join
df <- left_join(df_mort, df_meta, by='Code')

# correct to actual rate (/1000)
df <- df %>% mutate(`Mortality Rate` = `Mortality Rate`/1000)

# analysis
df_regions <- df %>% filter(`Record Type` == 'Region')
df_countries <- df %>% filter(`Record Type` == 'Country')

unique(df_regions$Name)
unique(df_countries$Region)

df_countries_latam <- df_countries %>%
  filter(Region == 'Latin America & Caribbean')


ggplot(df_countries_latam) +
  aes(x = Year, y = `Mortality Rate`, group = Name, label=Name) +
  geom_line() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
