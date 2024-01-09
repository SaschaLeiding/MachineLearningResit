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

jane_austen_sentiment <- tidy_books %>% # Take 'tidy_books' data which has each word individually
  inner_join(get_sentiments("bing")) %>% # include only words that are also part of dictionary "bing"
  count(book, index = linenumber %/% 80, sentiment) %>% # Counts for each book, each 80-line paragraph and both sentiments the number of words associated to each sentiment
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% #transforms table so each row is a 80-line paragraph within a book
  mutate(sentiment = positive - negative) # calculates sentiment index

# plot sentiment scores across plot trajectories
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")
# Plots development of overall sentiment for each 80-line paragraph for each book