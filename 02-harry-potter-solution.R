library(tidyverse)
library(tidytext)

## if the harrypotter package is not present, use the function
## below to install it.
## DO NOT USE install.packages("harrypotter") - that is a different package
# devtools::install_github("bradleyboehmke/harrypotter")
library(harrypotter)

# load Harry Potter text
# names of each book
hp_books <- c(
  "philosophers_stone", "chamber_of_secrets",
  "prisoner_of_azkaban", "goblet_of_fire",
  "order_of_the_phoenix", "half_blood_prince",
  "deathly_hallows"
)

# combine books into a list
hp_words <- list(
  philosophers_stone,
  chamber_of_secrets,
  prisoner_of_azkaban,
  goblet_of_fire,
  order_of_the_phoenix,
  half_blood_prince,
  deathly_hallows
) %>%
  # name each list element
  set_names(hp_books) %>%
  # convert each book to a data frame and merge into a single data frame
  map_df(as_tibble, .id = "book") %>%
  # convert book to a factor
  mutate(book = factor(book, levels = hp_books)) %>%
  # remove empty chapters
  drop_na(value) %>%
  # create a chapter id column
  group_by(book) %>%
  mutate(chapter = row_number(book)) %>%
  ungroup() %>%
  # tokenize the data frame
  unnest_tokens(word, value)

hp_words

# most frequent words, by book (excluding stop words)
hp_words %>%
  # delete stopwords
  anti_join(stop_words) %>%
  # summarize count per word per book
  count(book, word) %>%
  # get top 15 words per book
  group_by(book) %>%
  slice_max(order_by = n, n = 15) %>%
  mutate(word = reorder_within(word, n, book)) %>%
  # create barplot
  ggplot(aes(x = word, y = n, fill = book)) +
  geom_col(color = "black") +
  scale_x_reordered() +
  labs(
    title = "Most frequent words in Harry Potter",
    x = NULL,
    y = "Word count"
  ) +
  facet_wrap(~book, scales = "free") +
  coord_flip() +
  theme(legend.position = "none")

# Generate data frame with sentiment derived from the Bing dictionary
(hp_bing <- hp_words %>%
  inner_join(get_sentiments("bing")))

# Visualize the most frequent positive/negative words in the entire series
# using the Bing dictionary, and then separately for each book
#
# Hint: check out reorder_within() and scale_x_reordered()
# https://juliasilge.com/blog/reorder-within/

## all series
hp_bing %>%
  # generate frequency count for each word and sentiment
  group_by(sentiment) %>%
  count(word) %>%
  # extract 10 most frequent pos/neg words
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 10) %>%
  # prep data for sorting each word independently by facet
  mutate(word = reorder_within(word, n, sentiment)) %>%
  # generate the bar plot
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  # used with reorder_within() to label the axis tick marks
  scale_x_reordered() +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(
    title = "Sentimental words used in the Harry Potter series",
    x = NULL,
    y = "Number of occurences in all seven books"
  ) +
  coord_flip()

# per book
hp_pos_neg_book <- hp_bing %>%
  # generate frequency count for each book, word, and sentiment
  group_by(book, sentiment) %>%
  count(word) %>%
  # extract 10 most frequent pos/neg words per book
  group_by(book, sentiment) %>%
  slice_max(order_by = n, n = 10)

## positive words
hp_pos_neg_book %>%
  filter(sentiment == "positive") %>%
  mutate(word = reorder_within(word, n, book)) %>%
  ggplot(aes(word, n)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~book, scales = "free_y") +
  labs(
    title = "Positive words used in the Harry Potter series",
    x = NULL,
    y = "Number of occurences"
  ) +
  coord_flip()

## negative words
hp_pos_neg_book %>%
  filter(sentiment == "negative") %>%
  mutate(word = reorder_within(word, n, book)) %>%
  ggplot(aes(word, n)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~book, scales = "free_y") +
  labs(
    title = "Negative words used in the Harry Potter series",
    x = NULL,
    y = "Number of occurences"
  ) +
  coord_flip()

# Generate data frame with sentiment derived from the AFINN dictionary
(hp_afinn <- hp_words %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(book, chapter))

# Visualize which words in the AFINN sentiment dictionary appear most frequently
library(ggwordcloud)

library(ggwordcloud)

set.seed(123) # ensure reproducibility of the wordcloud
hp_afinn %>%
  # count word frequency across books
  ungroup() %>%
  count(word) %>%
  # keep only top 100 words for wordcloud
  slice_max(order_by = n, n = 100) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(70, 30))) %>%
  ggplot(aes(label = word, size = n, angle = angle)) +
  geom_text_wordcloud(rm_outside = TRUE) +
  scale_size_area(max_size = 15) +
  ggtitle("Most frequent tokens in Harry Potter") +
  theme_minimal()

# filter out "moody"
hp_afinn <- hp_afinn %>%
  filter(word != "moody")

# Visualize the positive/negative sentiment for each book over time
## using the AFINN dictionary
hp_words %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(book, chapter) %>%
  summarize(value = sum(value)) %>%
  ggplot(mapping = aes(x = chapter, y = value, fill = book)) +
  geom_col() +
  facet_wrap(~book, scales = "free_x") +
  labs(
    title = "Emotional arc of Harry Potter books",
    subtitle = "AFINN sentiment dictionary",
    x = "Chapter",
    y = "Emotional score"
  ) +
  theme(legend.position = "none")

## cumulative value
hp_words %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(book) %>%
  mutate(cumvalue = cumsum(value)) %>%
  ggplot(mapping = aes(x = chapter, y = cumvalue, fill = book)) +
  geom_step() +
  facet_wrap(~book, scales = "free_x") +
  labs(
    title = "Emotional arc of Harry Potter books",
    subtitle = "AFINN sentiment dictionary",
    x = "Chapter",
    y = "Cumulative emotional value"
  )
