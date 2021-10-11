library(tidyverse)
library(tidytext)

## if the harrypotter package is not present, use the function
## below to install it.
## DO NOT USE install.packages("harrypotter") - that is a different package
# devtools::install_github("bradleyboehmke/harrypotter")
library(harrypotter)

# load Harry Potter text
# names of each book
hp_books <- c("philosophers_stone", "chamber_of_secrets",
              "prisoner_of_azkaban", "goblet_of_fire",
              "order_of_the_phoenix", "half_blood_prince",
              "deathly_hallows")

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
  labs(title = "Most frequent words in Harry Potter",
       x = NULL,
       y = "Word count") +
  facet_wrap(facets = vars(book), scales = "free") +
  coord_flip() +
  theme(legend.position = "none")

# Generate data frame with sentiment derived from the Bing dictionary


# Visualize the most frequent positive/negative words in the entire series
# using the Bing dictionary, and then separately for each book
#
# Hint: check out reorder_within() and scale_x_reordered()
# https://juliasilge.com/blog/reorder-within/



# Generate data frame with sentiment derived from the AFINN dictionary
hp_afinn <- ...

# Visualize which words in the AFINN sentiment dictionary appear most frequently
library(ggwordcloud)

set.seed(123)   # ensure reproducibility of the wordcloud
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
# using the AFINN dictionary


