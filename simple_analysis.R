#Load libraries
library(tidyverse)
library(lubridate)


#Read data file
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')


#First look at data
glimpse(spotify_songs)

# Artists with tracks
artists_tracks <- spotify_songs %>% 
  distinct(track_id, .keep_all = TRUE) %>% 
  count(track_artist, sort = TRUE) %>%
  top_n(n = 20, wt = n)

artist_plot <- ggplot(data = artists_tracks, aes(x = reorder(track_artist, n), y = n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = 'Top 20 artist with most tracks in list', x = 'Artists', y = 'Number if tracks')

## Time passed between album release year
album_release_years <- 
  spotify_songs %>% 
  mutate(release_year = as.numeric(str_sub(track_album_release_date, 1, 4))) %>% #get only year information
  distinct(track_id, .keep_all = TRUE) %>% 
  distinct(track_name, track_artist, .keep_all = TRUE) %>% 
  group_by(track_artist) %>% 
  mutate(first_release_year = min(release_year),
         last_release_year = max(release_year),
         year_diff = last_release_year - first_release_year) %>%
  ungroup()  %>% 
  mutate(track_artist = fct_reorder(track_artist, year_diff)) 



album_release_years %>%  
  filter(year_diff > 50) %>% 
  ggplot() +
  geom_path(aes(x = release_year, y = track_artist)) +
  geom_point(aes(release_year, track_artist, color = track_artist, alpha = 0.1), size = 2) +
  labs(title = '', x = 'Album release years', y = 'Artists') +
  theme_light()


  
  
  
  






  
  