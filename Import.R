"
this Script is to Import the Data

Variables:
speaker = identifying number of the politician
party = party affiliation of the politician; there are only two parties (“A” and “B”)
age0 = age of the politician when giving the first speech in parliament
birthplace = identifying number of the region in which the politician was born
married = indicator for marriage (1=married, 0=not married)
corrindex = corruption index (larger values indicate larger probability of being corrupt)
income = average annual income of the politician (average over entire working life)
allspeeches = string containing all speeches the politician has given in parliament

Outcome Variables:
1. corrindex contains an index produced by an independent non-governmental institution (NGO), 
which is is supposed to measure how corrupt a politician is. 
The index has mean equal to zero, so positive (negative) values indicate that a politician
is more (less) corrupt than the average politician. More precisely, the NGO argues
that, under some assumptions, larger values of the index indicate a politician who is
more likely to engage in corrupt behavior at some point during their political career.

2. The variable income measures average annual income of the politician from age 40
to age 50.
"

# Install & Load Packages
{
  #install.packages("tidyverse")
  #install.packages("tidytext")
  
  library(tidyverse)
  library(tidytext)
}

# Load Data
{
  load("./Data/politicians.rdata")
}

# Old speech counter
{
  speech_count <- str_count(politicians$allspeeches, "\\t House of Commons Hansard Debates for ") + 1
  data_split <- politicians %>%
    # separate speeches of a speaker
    separate(col = allspeeches,
             into = paste0("speech", 1:max(speech_count)),
             sep = "\\t House of Commons Hansard Debates for ",
             fill = "right") %>%
    pivot_longer(cols = starts_with("speech"),
                 names_to = "speech_number",
                 values_to = "speech_text",
                 values_drop_na = TRUE) %>%
    mutate(speech_number = parse_number(speech_number))
}

# Get quick overview of data
{
  summary(politicians)
  "
  - age between 35 and 70 with mean of 47.33
  - 23 birthplaces
  - corruption index between -2.633 and 3.292 with mean of 0
  - income between 52,231 and 532,325 with mean of 112,303
  "
  
  # Plot variable by Party association
  plot_test <- ggplot(data = (politicians %>% mutate(income = log(income))), 
                      aes(x = income, y = corrindex, color = party, group = party)) +
    geom_point()
  plot_test
  
  "
  Plot indicates that for both parties there is a positive correlation between
  corrupt behavior and income, thus more corrupt behavior indicates higher income.
  Slightly indicates that individuals associated to party B have less corrupt
  behavior
  "
}

# Mutate and Tidy data
{
  new_colnames <- paste0(".", colnames(politicians))
  colnames(politicians) <- new_colnames
  
  data <- politicians %>% # Select Data Politicians as base
    # More or Less vocal politicians could indicate party affiliation
    mutate(number_speeches = str_count(.allspeeches, "\\t House of Commons Hansard Debates for ") + 1, # create variable with number of speeches per speaker in variable 'allspeeches'
           .birthplace = as.factor(.birthplace), # Transform birthplace to factor or categorical variable as higher or lower values have no ranking
           .party = as.factor(.party))# Transform party affiliation to factor
  
  # Split column 'allspeeches' into tokens in column 'word', flattening the table into one-token-per-row
  data_unnested <- data %>%
    unnest_tokens(.word, .allspeeches)
  
  rm(politicians)
}

# Get overall Sentiment per speaker
{
  "
  when using dictionary 'bing no need to filter out the stem as it contains all
  word alterations
  "
  sentiment_dict <- get_sentiments("bing")
  sentiment <- data_unnested %>% 
    inner_join(sentiment_dict, relationship = "many-to-many", join_by(.word == word)) %>% # include only words that are part of the defined sentiment dictionary
    count(.speaker, sentiment) %>% # Counts for each speaker and both sentiments the number of words associated to each sentiment
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% #transforms table so each row is a speaker again
    # Mutate Command works only for 'bing' dictionary 
    mutate(.sentiment = positive - negative) # calculate overall sentiment index
  
  data <- data %>% left_join(sentiment, join_by(.speaker == .speaker))
}

# Plot overall Sentiment
{
  plot_sentiment_inc <- ggplot(data = (data %>% mutate(income = log(income))),
                           aes(x=sentiment, y = income, color = party, group = party)) +
    geom_point()
  plot_sentiment_inc
  
  plot_sentiment_corr <- ggplot(data = data,
                               aes(x=sentiment, y = corrindex, color = party, group = party)) +
    geom_point()
  plot_sentiment_corr
}

# 
{
  # pick the words to keep as predictors
  {
    words_to_keep <- data_unnested %>%
      anti_join(get_stopwords(), by = join_by(word)) %>% # Drop words that are in dictionary stopwords, e.g.: I , me, my, myself... 
      count(word) %>% # counts each individual word
      
      # is first filter necessary?
      filter(str_detect(word, '.co|.com|.net|.edu|.gov|http', negate = TRUE)) |> # return all entries in 'word' that do NOT contain the listed words of URLs
      filter(str_detect(word, '[0-9]', negate = TRUE)) |> # return all entries in 'words' that contain no numbers
      
      # How do I justify the value of 2?
      filter(n > 2) |> # take only words that occur more than two times
      pull(word) # extract the column word as vector
  }

  # Construct Term Frequencies
  {
    tidy_speech <- data_unnested %>%
      filter(word %in% words_to_keep) |> # Take only words that are in previous created list
      count(id_speaker, word) |> # count per tweet, identified through '.id' the number of a word occuring
      # Bind the term frequency and inverse document frequency of the data to the dataset
      bind_tf_idf(term = 'word', # Column containing terms as string or symbol
                  document = 'id_speaker', # Column containing document IDs as string or symbol
                  n = 'n') |> # Column containing document-term counts as string or symbol
      select(id_speaker, word, tf) |> # select ID, word and term-frequency column
      # pivot wider into a document-term matrix
      pivot_wider(id_cols = id_speaker,
                  names_from = word,
                  values_from = tf,
                  values_fill = 0)
  }
  
  # Join Term Frequency with dataset
  {
    tidy_speech <- data %>%
      select(id_speaker:income) %>%
      right_join(tidy_speech, by = 'id_speaker')
  }
}
  
