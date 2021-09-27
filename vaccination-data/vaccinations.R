library(tidyverse)

# load data
df <- read_csv('data/israel_vax_data.csv', skip=1)
df <- select(df,!last_col()) # drop last col

# bind alternating rows
df_odd <- filter(df, row_number() %% 2 == 0)
df_even <- filter(df, row_number() %% 2 == 1)
df <- bind_cols(df_even, df_odd)

df <- select(df,-6,-9,-10) # drop extra columns

# relabel and fix types
names(df) <- c('age','count_non','count_full','severe_non','severe_full','pct_non','pct_full')

df$pct_non <- str_replace(df$pct_non,'%','')
df$pct_full <- str_replace(df$pct_full,'%','')
df <- type_convert(df)

df$pct_non <- round(df$pct_non / 100,4) # to pct
df$pct_full <- round(df$pct_full / 100,4)

# final dataframe
df <- df %>% 
  mutate(pct_partial = 1 - (pct_non + pct_full)) %>%
  mutate(count_partial = round(count_full / pct_full * pct_partial)) %>% 
  mutate(efficacy = 1- ((severe_full/10^4) / (severe_non/10^4)))

# analysis
total_pop <- 9227700

total_non <- sum(df$count_non)
total_full <- sum(df$count_full)
total_partial <- sum(df$count_partial)

total_eligible <- sum(total_non, total_full, total_partial)

total_ineligible <- total_pop - total_eligible
pct_ineligible <- round(total_ineligible / total_pop,4)
