library(tidyverse)
library(spotifyr)
#authentication
Sys.setenv(SPOTIFY_CLIENT_ID = '175c4c5873884eccb00ea8cec2b44ada')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '7986d3572e40498686f8ebb03a0c9e7c')

access_token <- get_spotify_access_token()

get_artist_audio_features('The Beatles') %>%
  mutate(album_release_year = lubridate::year(album_release_year)) %>%
  filter(album_release_date < 1994) %>%
  write.csv(file = "./data/beatles.csv", row.names=FALSE, na="")

get_artist_audio_features('John Lennon') %>%
  mutate(album_release_year = lubridate::year(album_release_year)) %>%
  write.csv(file = "./data/john_lennon.csv", row.names=FALSE, na="")

get_artist_audio_features('Paul McCartney') %>%
  mutate(album_release_year = lubridate::year(album_release_year)) %>%
  write.csv(file = "./data/paul_mc.csv", row.names=FALSE, na="")

