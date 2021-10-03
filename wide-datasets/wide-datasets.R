library(tidyverse)
library(readxl)

# import excel sheet and skip header rows
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
 names_to = c('Year','Category'),
 names_pattern = '(.{4})(.*)',
 values_to = 'Measure'
)


# done, now let's demonstrate a couple of simple visualizations

df_rank_2020 <- df %>% 
  filter(Category=='Rank', Year==2020) %>% 
  filter(Measure >= 90) %>%
  arrange(desc(Measure))

df_rank_2020 %>% ggplot(aes(x=reorder(Code, -Measure), y=Measure)) +
  geom_bar(stat='identity')


df_rank_annual <- df %>% 
  filter(Category=='Rank', Code=='AFG') %>% 
  arrange(Year)

df_rank_annual %>% ggplot(aes(x=Year, y=Measure)) +
  geom_bar(stat='identity')

          