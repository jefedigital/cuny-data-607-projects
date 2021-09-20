library(tidyverse)

## create players df

df <- read_delim('data/tournamentinfo.txt', delim='|',
                 skip=2, trim_ws=TRUE, show_col_types = FALSE) %>% 
  rename(player_id=1, player_name=2)
  
df <- select(df,1:length(df)-1) # drop last col


df_players <- select(df,1:2) %>% 
  filter(!is.na(df['player_name'])) 

df_players_odd <- filter(df_players,row_number() %% 2 == 1) # separate even and odd rows
df_players_even <- filter(df_players,row_number() %% 2 == 0)

df_players <- bind_cols(df_players_odd, df_players_even) # bind columns
names(df_players) <- c('player_id','player_name','state','player_info') # rename multiple columns

df_players <- df_players %>% 
  mutate(pre_rating = as.numeric(str_extract(player_info,'(?<=: ).{4}'))) %>% # extract the players pre-rating
  mutate(post_rating = as.numeric(str_extract(player_info,'(?<=>).{4}'))) %>%
  mutate(player_id = as.numeric(player_id)) %>% # convert to numeric
  select(!player_info) # cleanup

## create matches df

df_matches <- filter(df, !is.na(as.numeric(df$player_id))) # only keep rows with a numeric value in first column

df_matches <- select(df_matches,-player_name,-Pts) # drop extra cols

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

# ELO rating

# cribbed from wikipedia
df_elo <- df_matches %>%
  mutate(elo_match = ifelse(outcome == 'W', opponent_pre_rating+400, ifelse(outcome == 'L', opponent_pre_rating-400,0))) %>%
  group_by(player_id) %>%
  summarize(elo_rating=round(mean(elo_match)))

## stats for all players

df_player_scores <- df_matches %>% 
  group_by(player_id) %>% 
  summarize(total_points=sum(match_points), avg_opponent_pre_rating=round(mean(opponent_pre_rating))) %>%
  arrange(player_id)

df_final <- df_players %>%
  left_join(df_player_scores, by='player_id') %>%
  left_join(df_matches_played, by='player_id') %>% 
  left_join(df_elo, by='player_id') %>%
  relocate(matches_played, .after=state) %>% # rearrange column order
  relocate(total_points, .after=matches_played) %>%
  relocate(elo_rating, .after=post_rating) 


# stats by match participation

df_final_all_players <- df_final %>% 
  select(-matches_played,-post_rating,-elo_rating)

df_final_all_matches <- df_final %>% 
  filter(matches_played == 7) %>%
  select(-matches_played,-post_rating,-elo_rating)

df_final_some_matches <- df_final %>%
  filter(matches_played < 7) %>%
  select(-matches_played,-post_rating,-elo_rating)

# elo analysis

df_elo_analysis <- df_final %>%
  mutate(elo_exceeded = post_rating - elo_rating) %>%
  filter(elo_exceeded > 0) %>%
  arrange(desc(elo_exceeded))

# output

write_csv(df_final_all_players, 'output/all_players.csv')
write_csv(df_final_all_matches, 'output/played_all_matches.csv')
write_csv(df_final_some_matches, 'output/played_some_matches.csv')


# https://medium.com/purple-theory/what-is-elo-rating-c4eb7a9061e0
  
  
  
