# goal: run text regression to classify tweets from Trumps account
# into whether tweet came from Trump himself or from one of his staffers


library(tidyverse)
library(tidytext)
library(tidymodels)
library(lubridate)
library(SnowballC)


# --------- prepare data

# load Trump tweets
load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))
glimpse(trump_tweets_df)

# Trump tweeted from Android phone
# staffers tweeted from iPhone
# extract "source" (i.e. type of phone) from html code
tweets <- trump_tweets_df |> # Select trump data
  select(.id = id, # Select only specific variables
         .source = statusSource,
         .text = text,
         .created = created) |>
  extract(.source, '.source', "Twitter for (.*?)<") |> # Extract source (Iphone, Adnroid or NA)
  filter(.source %in% c('iPhone', 'Android')) |> # Drop source NA
  mutate(.source = factor(.source)) # transform source to factor format

# plot percentage of tweets by hour of day and source
tweets |>
  count(.source, hour = hour(with_tz(.created, "EST"))) |> # counts number of tweets per source(iphone or Android) per hour of day
  mutate(percent = n / sum(n)) |> # calculates share of number of tweets in total not by source
  ggplot(aes(hour, percent, color = .source)) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "") +
  theme_minimal()


# pick the words to keep as predictors
words_to_keep <- tweets |>
  unnest_tokens(input = '.text', # Split tweets from '.text' column into individual tokens in 'word' column
                output = 'word') |>
  anti_join(get_stopwords()) |> # Drop words that are in dictionary stopwords, e.g.: I , me, my, myself... 
  count(word) |> # counts each individual word
  filter(str_detect(word, '.co|.com|.net|.edu|.gov|http', negate = TRUE)) |> # return all entries in 'word' that do NOT contain the listed words of URLs
  filter(str_detect(word, '[0-9]', negate = TRUE)) |> # return all entries in 'words' that contain no numbers
  filter(n > 2) |> # take only words that occur more than two times
  pull(word) # extract the column word as vector


tidy_tweets <- tweets |>
  unnest_tokens(input = '.text',
                output = 'word') |>
  filter(word %in% words_to_keep) |>
  count(.id, word) |>
  # compute term frequencies
  bind_tf_idf(term = 'word',
              document = '.id',
              n = 'n') |>
  select(.id, word, tf) |>
  # pivot wider into a document-term matrix
  pivot_wider(id_cols = '.id',
              names_from = 'word',
              values_from = 'tf',
              values_fill = 0)

# join with the training labels
tidy_tweets <- tweets |>
  select(.id, .source, .created) |>
  right_join(tidy_tweets, by = '.id')

dim(tidy_tweets)


# split sample into traininig and test sample
tweet_split <- initial_split(tidy_tweets, prop = 0.8)

train <- training(tweet_split)
test <- testing(tweet_split)


# --------- (1) simple model (under-fitting)

# logistic regression only with a few hand-picked words
model1 <- logistic_reg() |>
  fit(formula = .source ~ crooked + dumb + emails +
        crowds + hillary + winning + weak,
      data = train)

tidy(model1)

# in-sample fit: percentage of correct predictions
train |>
  bind_cols(predict(model1, train)) |>
  accuracy(truth = .source, estimate = .pred_class) 

# out-of-sample fit: number of correct and false predictions
test |>
  bind_cols(predict(model1, test)) |>
  conf_mat(truth = .source, estimate = .pred_class) |>
  autoplot(type = 'heatmap')



# --------- (2) complex model (over-fitting)

# logistic regression with all words
model2 <- logistic_reg() |>
  fit(formula = .source ~ .,
      data = train |>
        select(-.id, -.created))

tidy(model2)

# in-sample fit: percentage of correct predictions
train |>
  bind_cols(predict(model2, train)) |>
  accuracy(truth = .source, estimate = .pred_class) 

# out-of-sample fit: number of correct and false predictions
test |>
  bind_cols(predict(model2, test)) |>
  conf_mat(truth = .source, estimate = .pred_class) |>
  autoplot(type = 'heatmap')


# --------- (3) regularized model

# logistic regression with LASSO penalty
model3 <- logistic_reg(penalty = 0.01, mixture = 1) |>
  set_engine('glmnet') |>
  fit(formula = .source ~ .,
      data = train |>
        select(-.id, -.created))

tidy(model3)


# in-sample fit: percentage of correct predictions
train |>
  bind_cols(predict(model3, train)) |>
  accuracy(truth = .source, estimate = .pred_class) 

# out-of-sample fit: number of correct and false predictions
test |>
  bind_cols(predict(model3, test)) |>
  conf_mat(truth = .source, estimate = .pred_class) |>
  autoplot(type = 'heatmap')

# plot words most strongly related to Trump
tidy(model3) |> 
  filter(estimate < -10) |> 
  ggplot(mapping = aes(x = estimate,
                       y = fct_reorder(term, estimate, .desc = TRUE))) + 
  geom_col() +
  labs(x = 'estimated coefficient',
       y = 'word', title="words most strongly related to Trump")

# plot words most strongly related to staffers
tidy(model3) |> 
  filter(estimate > 10) |> 
  ggplot(mapping = aes(x = estimate,
                       y = fct_reorder(term, estimate, .desc = FALSE))) + 
  geom_col() +
  labs(x = 'estimated coefficient',
       y = 'word', title="words most strongly related to staffers")

