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

## tokenize
lyrics_unigrams <- unnest_tokens(
  tbl = song_lyrics,
  output = word,
  input = Lyrics)
lyrics_bigrams <- unnest_tokens(
  tbl = song_lyrics,
  output = word,
  input = Lyrics, 
  token = "ngrams", n = 2
)

## combine together
tidy_lyrics <- bind_rows(lyrics_unigrams, lyrics_bigrams)
tidy_lyrics

# Find all the state names occurring in the song lyrics
# - First create a data frame that meets this criteria
# - Save a new data frame that only includes one observation for each matching song.
#   That is, if the song is "New York, New York", there should only be one row in
#   the resulting table for that song.

## use inner_join() to combine and only keep words that are state names
## distinct() to deduplicate the states per song
tidy_lyrics <- inner_join(tidy_lyrics, pop_df, by = c("word" = "state_name")) %>%
  distinct(Rank, Song, Artist, Year, word, .keep_all = TRUE)
tidy_lyrics

# Calculate the frequency for each state's mention in a song and
# create a new column for the frequency adjusted by the state's population

## aggregate per state
(state_counts <- tidy_lyrics %>% 
    count(word) %>% 
    arrange(desc(n)))

## normalize for population
pop_df <- pop_df %>% 
  left_join(state_counts, by = c("state_name" = "word")) %>% 
  mutate(rate = n / population * 1e6)

## which are the top ten states by rate?
pop_df %>%
  arrange(desc(rate)) %>%
  top_n(10)
