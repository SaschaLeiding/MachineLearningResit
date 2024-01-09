library(tidyr)
library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(ggplot2)

# load text of Jane Austen's books
tidy_books <- austen_books() %>% # Load library with several Jane Austen book which contains two columns 'name of book' and 'line'
  group_by(book) %>%
  mutate(
    linenumber = row_number(), # Create variable counting linenumber
    chapter = cumsum(str_detect(text, # Create variable detecting association to chapter
                                regex("^chapter [\\divxlc]",
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text) # Split column 'text' into tokens in column 'word', flattening the table into one-token-per-row


# load dictionary assign sentiment (positive/negative) to each word
get_sentiments("bing") # Get specific sentiment lexicons in a tidy format

# count most frequently occuring positive words
sentiments <- get_sentiments("bing") %>%
  filter(sentiment=="positive") # Filter for only positive sentiments

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(sentiments) %>% # select only 'positive' words from book 'Emma'
  count(word, sort = TRUE) # count occurrences of 'positive' words


# --------- dictionary method to assign sentiment to document

# document: 80-line section of text within a book
# assign sentiment score to each 80-line section by
# - counting number of positive words
# - counting number of negative words
# - take difference

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# plot sentiment scores across plot trajectories
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")