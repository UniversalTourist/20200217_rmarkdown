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
  
  