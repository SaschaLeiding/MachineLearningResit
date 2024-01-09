library(tidyverse)
library(tidytext)
library(sotu)
library(SnowballC)
library(wordcloud2)
library(textdata)


# ------- Step 1: choose unit of analysis (here: sentence)

# load the data from the sotu package (State of the Union speeches)
df <- sotu::sotu_meta |> 
  mutate(text = sotu::sotu_text,
         speech_id = 1:length(sotu::sotu_text)) |> 
  select(speech_id, president, year, text)

df <- as_tibble(df)
df



# split into sentences
tidy_sotu <- df |> 
  unnest_tokens(input = 'text',
                output = 'sentence',
                token = 'sentences') |> 
  # keep only the sentences that contain the word stem "manufactu"
  filter(str_detect(sentence, 'manufactu'))

tidy_sotu


# ------- Step 2: create bag of words

tidy_sotu <- tidy_sotu |> 
  unnest_tokens(input = 'sentence',
                output = 'word') |> 
  # we're just interested in words that occur near manufacturing, so remove
  # the manufacturing words themselves
  filter(str_detect(word, 'manufactu', negate = TRUE))

tidy_sotu


# ------- Step 3: reduce dimensionality

# remove stopwords (articles, prepositions, etc.)
tidy_sotu <- tidy_sotu |> 
  anti_join(get_stopwords())

tidy_sotu 

# represent words by their stem (e.g., "duties" and "duty" are both represented by the stem "duti")
wordStem('duty')
wordStem('duties')

tidy_sotu <- tidy_sotu |> 
  mutate(word_stem = wordStem(word))

tidy_sotu


# ------- Step 4: create document-term matrix

# create document-term matrix
count_sotu <- tidy_sotu |> 
  count(word_stem)

print(count_sotu, n=1000)

# create word cloud
tidy_sotu |> 
  count(word_stem) |>
  wordcloud2()


