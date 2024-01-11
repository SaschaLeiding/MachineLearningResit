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
  data <- politicians %>% # Select Data Politicians as base
    # More or Less vocal politicians could indicate party affiliation
    mutate(number_speeches = str_count(allspeeches, "\\t House of Commons Hansard Debates for ") + 1, # create variable with number of speeches per speaker in variable 'allspeeches'
           birthplace = as.factor(birthplace))# Transform birthplace to factor or categorical variable as higher or lower values have no ranking
}

# Get overall Sentiment per speaker
{
  sentiment_dict <- get_sentiments("bing")
  sentiment <- politicians %>%
    unnest_tokens(word, allspeeches) %>% # Split column 'allspeeches' into tokens in column 'word', flattening the table into one-token-per-row
    inner_join(sentiment_dict, relationship = "many-to-many") %>% # include only words that are part of the defined sentiment dictionary
    count(speaker, sentiment) %>% # Counts for each speaker and both sentiments the number of words associated to each sentiment
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% #transforms table so each row is a 80-line paragraph within a book
    mutate(sentiment = positive - negative) # calculates sentiment index
}

  
