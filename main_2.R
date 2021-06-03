#Importing libraries and reading the data

library(spotifyr)
library(jsonlite)
library(lubridate)
library(tidyverse)
library(knitr)
library(ggplot2)
library(ggpubr)
library(plotly)
library(gghighlight)

# Establishing connection and authorizing

Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)

get_spotify_authorization_code()

# Spotify Playlist Features - 1

p_username <- "AM"
p_url <- "1TJEqUewyHTLD2wBmBCjMb"

playlist <- get_playlist_audio_features(p_username, p_url)

# Plotting less and more popular tracks in the playlist \\m/ based on "track.popularity" variable

p <- playlist %>%
  group_by(track.popularity) 

less_than_50 <- p %>%
  filter(track.popularity <= "50") %>%
  ggplot(aes(x = track.name, y = track.popularity)) +
  geom_col(aes(fill = track.album.name)) +
  labs(x = "Track Name", y = "Popularity") +
  ggtitle("The least popular tracks in my playlist \\m/ on Spotify", "Popularity ranking <= 50") +
  theme(axis.text.x = element_text(angle=90))

more_than_80 <- p %>%
  filter(track.popularity >= "80") %>%
  ggplot(aes(x = track.name, y = track.popularity)) +
  geom_col(aes(fill = track.album.name)) +
  scale_y_continuous(limits = c(70,85), oob = scales::squish) +
  labs(x = "Track Name", y = "Popularity") +
  ggtitle("The most popular tracks in my playlist \\m/ on Spotify", "Popularity ranking >= 80") +
  theme(axis.text.x = element_text(angle = 90))

figure <- ggarrange(less_than_50,NULL,more_than_80, ncol = 1, heights = c(1, 0.1, 1))

figure
ggplotly()

# Spotify Playlist Features - 2

p_url_2 <- "0IrhdY1o9Z4orHOdnCXDJC"
playlist_2 <- get_playlist_audio_features(p_username, p_url_2)

# Plotting less and more popular tracks in the playlist \\m/ based on "track.popularity" variable

p_2 <- playlist_2 %>%
  group_by(track.popularity) 

less_than_50 <- p_2 %>%
  filter(track.popularity <= "50") %>%
  ggplot(aes(x = track.name, y = track.popularity)) +
  geom_col(aes(fill = track.album.name)) +
  labs(x = "Track Name", y = "Popularity") +
  ggtitle("The least popular tracks in my playlist Yeet on Spotify", "Popularity ranking <= 50") +
  theme(axis.text.x = element_text(angle=90))

more_than_80 <- p_2 %>%
  filter(track.popularity >= "80") %>%
  ggplot(aes(x = track.name, y = track.popularity)) +
  geom_col(aes(fill = track.album.name)) +
  scale_y_continuous(limits = c(70,90), oob = scales::squish) +
  labs(x = "Track Name", y = "Popularity") +
  ggtitle("The most popular tracks in my playlist Yeet on Spotify", "Popularity ranking >= 80") +
  theme(axis.text.x = element_text(angle = 90))

figure <- ggarrange(less_than_50,NULL,more_than_80, ncol = 1, heights = c(1, 0.1, 1))

figure
ggplotly()

# Getting features of my top four favorite artists

fav1 <- get_artist_audio_features(artist = "Green Day")
fav2 <- get_artist_audio_features(artist = "Coldplay")
fav3 <- get_artist_audio_features(artist = "Fall Out Boy")
fav4 <- get_artist_audio_features(artist = "Taylor Swift")

# Creating a data frame from combining the above data

top_4 <- rbind(fav1,fav2,fav3,fav4)

# Plotting the emotional quadrant scatter plot

eq <- ggplot(data = top_4, aes(x = valence, y = energy, color = artist_name)) +
  geom_point(position = "jitter") +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  geom_label(x = 0.25/2, y = 0.95, label = "Angry / Turbulent", color = "black", fill = "aliceblue") +
  geom_label(x = 1.75/2, y = 0.95, label = "Joyful / Happy", color = "black", fill = "aliceblue") +
  geom_label(x = 1.75/2, y = 0.05, label = "Peace / Chill", color = "black", fill = "aliceblue") +
  geom_label(x = 0.25/2, y = 0.05, label = "Depressing / Sad", color = "black", fill = "aliceblue") +
  labs(x= "Valence", y= "Energy") +
  ggtitle("Emotional Quadrant - Top Four Artists", "Energy vs Valence")  

eq  
ggplot()