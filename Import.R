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
  
  library(tidyverse)
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

# Counting number of speeches per speaker
data <- politicians %>%
  mutate(number_speeches = str_count(allspeeches, "\\t House of Commons Hansard Debates for ") + 1)
  
