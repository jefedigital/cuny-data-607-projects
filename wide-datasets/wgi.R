library(tidyverse)
library(readxl)
library(curl)

# import excel sheet and skip header rows
curl_download('http://info.worldbank.org/governance/wgi/Home/downLoadFile?fileName=wgidataset.xlsx', 'data/wgi/wgidataset.xlsx')

df_raw <- read_excel('data/wgi/wgidataset.xlsx', sheet='ControlofCorruption', skip=12)


# need to combine the first two rows 

# convert the column names and first row into two vectors

df_col_1 <- colnames(df_raw)
df_col_2 <- as.character(slice(df_raw,1))

# get rid of the auto numbering in the columns
df_col_1 <- str_remove(df_col_1, '\\..*$')

# now join each element of the two vectors with paste()
df_cols <- paste0(df_col_1, df_col_2)



# make new df with values and new column names
df <- df_raw[-1,]
names(df) <- df_cols

head(df)

# replace all the instances of '#N/A'  with NA

nona <- function(s) {str_replace(s,'#N/A','NA')}
df <- df %>% mutate(across(everything(), nona))

# convert columns to numeric type
df <- type_convert(df)

# pivot time
# we want seven cols in total:
# country, code, year, category, measure

df <- df %>% pivot_longer(
  cols = `1996Estimate`:`2020Upper`,
  names_to = c('Year','Measure'),
  names_pattern = '(.{4})(.*)',
  values_to = 'Value'
)


# done, now let's demonstrate a couple of simple visualizations

df_rank_2020 <- df %>% 
  filter(Measure=='Rank', Year==2020) %>% 
  filter(Value >= 90) %>%
  arrange(desc(Value))

df_rank_2020 %>% ggplot(aes(x=reorder(Code, -Value), y=Value)) +
  geom_bar(stat='identity') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x='Year')

df_rank_annual <- df %>% 
  filter(Measure=='Rank', Code=='AFG') %>% 
  arrange(Year)

df_rank_annual %>% ggplot(aes(x=Year, y=Value)) +
  geom_bar(stat='identity') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90))

          