# Importing libraries and reading the data

library(jsonlite)
library(lubridate)
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)
library(gghighlight)

s1 <-fromJSON("StreamingHistory0.json",flatten = TRUE)
s2 <-fromJSON("StreamingHistory1.json",flatten = TRUE)

streaming_history <- rbind(s1,s2)
head(streaming_history,10)

# Changing into Indian Standard Time Zone, creating Date and Time columns

data <- streaming_history %>%
  as_tibble() %>%
  mutate_at("endTime",ymd_hm) %>%
  mutate(endTime = endTime + minutes(330)) %>%
  mutate(date = floor_date(endTime,"day") %>%
  as_date, seconds = msPlayed/1000, minutes = seconds/60)  

# Playback activity per week and hours

streaming_hrs <- data %>%
  group_by(date = floor_date(date,"week")) %>%
  summarise(hours = sum(minutes/60)) %>%
  arrange(date) %>%
  ggplot(aes(x = date, y= hours)) + geom_col(aes(fill = hours)) +
  scale_x_date(date_labels= "%b %y",date_breaks  = "1 month")+
  scale_fill_gradient(low = 'lightblue1',high = 'blue') +
  labs(x = "Date", y = "Hours of music played") +
  ggtitle("Time I've listened to music on Spotify", "Playback activity per week")

streaming_hrs
ggplotly()

# Playback activity time per artist

artist_hrs <- data %>%
  group_by(artistName, date = floor_date(date,"month")) %>%
  summarise(hours = sum(minutes)/60) %>%
  ggplot(aes(x = date, y = hours, group = artistName)) +
  labs(x = "Date", y = "Hours of music played") + 
  scale_x_date(date_labels = "%b %y",date_breaks  = "1 month")+
  ggtitle("Time I've listened to different artists", "Playback activity time per artist") +
  geom_line(aes(date,hours,color = artistName),lwd = 0.8) +
  gghighlight(artistName == "Fall Out Boy" || artistName == "Coldplay" || artistName == "Taylor Swift" || artistName == "Green Day")

artist_hrs
ggplotly()

# Playback activity of most listened-to artists

most_listened <- data %>% 
  group_by(artistName) %>% 
  summarize(hours_listened = sum(minutes)/60) %>% 
  filter(hours_listened >= 5) %>%
  ggplot(aes(x = artistName, y = hours_listened)) + 
  geom_col(aes(fill = hours_listened)) +
  scale_fill_gradient(low = "lightblue1", high = "blue") + 
  labs(x = "Artist", y = "Hours of music playback") + 
  ggtitle("Most listened-to artists on my Spotify", "For more than 5 hours listen") +
  theme(axis.text.x = element_text(angle = 90))

most_listened
ggplotly()

# Activity by date and time of day

times <- data %>%  
  group_by(date, hour = hour(endTime))  %>% 
  summarize(mins_listened = sum(minutes)) %>% 
  ggplot(aes(x = hour, y = date, fill = mins_listened)) + 
  geom_tile() + 
  labs(x = "Time of the day (0-24)", y = "Date") + 
  scale_y_date(date_labels="%b %y",date_breaks  ="1 month") +
  ggtitle("Playback activity at different times on my Spotify", "Activity by date and time of day") +
  scale_fill_gradient(low = "lightblue1", high = "blue")

times
ggplotly()

# Playback Activity from 0 to 24 hours

day_times <- data %>%
  group_by(date, hour = hour(endTime), weekday = wday(date, label = TRUE)) %>%
  summarise(mins_listened = sum(minutes)) 

day_times %>%
  ggplot(aes(x = hour, y = mins_listened, group = date)) +
  geom_col(fill = "violet") +
  labs(x = "Time of the Day", y = "Minutes Listened") +
  ggtitle("Minutes of music listened to on Spotify by time of day", "Playback Activity fromn 0 to 24 hours")

ggplotly()

# Weekly playback activity from 0 to 24 hours

day_times %>% 
  group_by(weekday, hour) %>% 
  summarize(minutes = sum(mins_listened)) %>% 
  ggplot(aes(x = hour, weekday, fill = minutes)) + 
  geom_tile() + 
  scale_fill_gradient(low = "lightblue1", high = "blue") +
  labs(x = "Time of the day", y = "Weekday") + 
  ggtitle("Weekday and time of day of music played on Spotify", "Weekly activity from 0 to 24 hours")

ggplotly()

# Weekday and weekend activity from 0 to 24 hours
day_type <- day_times %>% 
  mutate(day_type = if_else(weekday %in% c("Sat", "Sun"), "weekend", "weekday")) %>% 
  group_by(day_type, hour) %>% 
  summarize(minutes = sum(mins_listened)) %>% 
  ggplot(aes(x = hour, y = minutes, color = day_type)) + 
  geom_line(size = 1) +
  labs(x = "Time of the day", y = "Minutes of music played") + 
  ggtitle("Playback activity on week day type", "Weekday and weekend activity from 0 to 24 hours") 

day_type
ggplotly()

