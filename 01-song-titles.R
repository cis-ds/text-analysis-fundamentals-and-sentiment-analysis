library(tidyverse)
library(tidytext)
library(here)

# import song lyrics and population data
song_lyrics <- here("data", "billboard_lyrics_1964-2015.csv") %>%
  read_csv()
glimpse(song_lyrics)

pop_df <- here("data", "pop2016.csv") %>%
  read_csv()

# Use tidytext to create a data frame with one row for each token in each song
# Hint: To search for matching state names, this data frame should include both
# unigrams and bi-grams.



# Find all the state names occurring in the song lyrics
# - First create a data frame that meets this criteria
# - Save a new data frame that only includes one observation for each matching song.
#   That is, if the song is "New York, New York", there should only be one row in
#   the resulting table for that song.



# Calculate the frequency for each state's mention in a song and
# create a new column for the frequency adjusted by the state's population


