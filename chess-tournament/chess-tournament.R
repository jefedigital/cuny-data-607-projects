library(tidyverse)

## create players df

df <- read_delim('data/tournamentinfo.txt', delim='|',skip=1, trim_ws=TRUE, show_col_types = FALSE) # load

df_players <- select(df,1:2) %>% 
  filter(!is.na(df['Player Name'])) %>% 
  filter(Pair!='Num') # cleanup

df_players_odd <- filter(df_players,row_number() %% 2 == 1) # separate even and odd rows
df_players_even <- filter(df_players,row_number() %% 2 == 0)

df_players <- bind_cols(df_players_odd, df_players_even) # bind columns

names(df_players) <- c('player_id','name','state','player_info') # rename multiple columns

df_players <- df_players %>% 
  mutate(pre_rating = as.numeric(str_extract(player_info,'(?<=: ).{4}'))) %>% # extract the players pre-rating
  mutate(player_id = as.numeric(player_id)) %>% # convert to numeric
  select(!player_info) # cleanup


## create matches df

df_matches <- filter(df, !is.na(as.numeric(df$Pair))) # only keep rows with a numeric value in first column

df_matches <- select(df_matches,1,4:10) # drop extra cols

names(df_matches) <- c('player_id','1','2','3','4','5','6','7') # rename multiple columns

df_matches <- pivot_longer(df_matches,!player_id, names_to='round', values_to='match_info') # pivot all columns except player_id

df_matches <- df_matches %>% 
  mutate(outcome=str_sub(match_info,1,1), opponent_id=str_sub(match_info,str_length(match_info)-1)) %>% # break out outcome and opponent id
  filter(outcome == 'W' | outcome =='L' | outcome == 'D') %>% # keep only wins, losses and draws
  mutate(match_points = ifelse(outcome == 'W', 1, ifelse(outcome == 'D', 0.5, 0))) %>% # calculate points per match
  mutate(player_id = as.numeric(player_id), opponent_id = as.numeric(opponent_id)) %>% # convert to numeric
  select(!match_info) # drop extra cols

df_matches <- df_matches %>% 
  left_join(df_players[c('player_id','pre_rating')], by = c('opponent_id' = 'player_id')) %>% # join opponent scores
  rename(opponent_pre_rating = pre_rating) # rename single column


# count of matches per player

df_matches_played <- df_matches %>%
  group_by(player_id) %>%
  summarize(matches_played = n())


## stats for all players

df_player_scores <- df_matches %>% 
  group_by(player_id) %>% 
  summarize(total_points=sum(match_points), avg_opponent_pre_rating=round(mean(opponent_pre_rating))) %>%
  arrange(player_id)

df_final <- df_players %>% 
  left_join(df_player_scores, by='player_id') %>%
  relocate(total_points, .after=state) # rearrange column order


# stats by match participation

df_final_some <- inner_join(df_final, df_matches_played, by='player_id') %>% 
  filter(matches_played != 7) %>%
  select(!matches_played)

df_final_all <- inner_join(df_final, df_matches_played, by='player_id') %>% 
  filter(matches_played == 7) %>%
  select(!matches_played)


# output

write_csv(df_final, 'output/all_players.csv')
write_csv(df_final_all, 'output/played_all_matches.csv')
write_csv(df_final_some, 'output/played_some_matches.csv')
