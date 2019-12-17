## Set working directory

setwd("your-working-directory")

## Load libraries, you might need to install them first (install.packages)

library(tidyverse)
library(lubridate)

library(spotifyr)

library(magick)
library(imager)

library(scales)

## Custom ggplot2 theme

theme_palette <- function () { 
  theme_bw(base_size=10, base_family="Avenir") %+replace% 
    theme(
      panel.background  = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "black"), 
      plot.title = element_blank(),
      plot.margin = margin(10),
      legend.position="none",
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )
}

## Set up credentials for working with the Spotify API

Sys.setenv(SPOTIFY_CLIENT_ID = 'your-spotify-client-id')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'your-spotify-client-secret')

access_token <- get_spotify_authorization_code()

## Let's go!

username <- "your-username" ## Spotify username of the owner of the playlist. If it's not you, the playlist must be public
playlist_id <- "your-playlist-id" ## Spotify playlist ID of the playlist you want to generate the cover for
red <- "danceability" ## Feature which determines the red component of the color
green <- "energy" ## Feature which determines the green component of the color
blue <- "valence" ## Feature which determines the blue component of the color
height_feature <- "popularity" ## Feature which determines the height of the individual bars

## Make sure that dplyr can get the column names as they are stored in a character vector

red <- ensym(red)
green <- ensym(green)
blue <- ensym(blue)

## Get track features for the given playlist

features <- get_playlist_audio_features(username = username,
                                        playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id,
         track_name = track.name, track_id = track.id,
         popularity = track.popularity,
         danceability, energy, speechiness, acousticness,
         instrumentalness, liveness, valence,
         key_name, mode_name, key_mode,
         tempo)

## Create color palette based on the chosen features

cover_palette <- features %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, 
                             g = !!green, 
                             b = !!blue, 
                             maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, 
                     g = !!green, 
                     b = !!blue,
                     maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")

## Generate plot

p <- ggplot(cover_palette, aes(id, get(height_feature), fill = color)) +
  geom_bar(stat = "identity") +
  theme_palette() +
  scale_fill_identity() +
  scale_x_discrete() +
  scale_y_continuous(breaks = NULL) +
  coord_polar() +
  expand_limits(y = -2) 

p
