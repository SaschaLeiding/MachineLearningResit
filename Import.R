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

Task:
- To which extent does PARTY AFFILIATIOn affect CORRUPTION and INCOME
-> causal effect of PARTY AFFILIATION
-> Heterogeneity of causal effect (what TYPE of politician benefits more/less from PARTY AFFILIATION)

- Explore two possibilities for establishing causality
1. Use ALLSPEECHES to control for the TYPE of politician
 -> text may contain a signal about what kind of politician they are
 -> may proxy for confounding variables (those that simultaneously affect PARTY AFFILITION and the Outcome)

2. Use BIRTHPLACE as an IV for PARTY AFFILIATION
 -> speeches may not perfectly measure all confounders
 -> endogeneity even conditional on the type
"

# Strategy - To-Do
{"
  In General:
  (a) corrindex ~ PARTY + (age + birthplace + married) + TYPE OF POLITCIAN 
  (b) income ~ PARTY + (age + birthplace + married) + TYPE OF POLITCIAN
  => as in (a) & (b) and with INTERACTION terms of PARTY with CONFOUNDERS
  
  Prior need to analyse ALLSPEECHES for TYPE OF POLITICIAN
Done  (i) .number_speeches = How Vocal is politician
Done  (ii) .speechlength = avg. length of speech
  (iii) generality = how many basic terms per speech
  (iv) content = TOPIC MODELLING by various indizes
    -> may need to transform numerical corrindex and income into categorical
    
  Birthplace:
  - number indicating the region a speaker was born in
  => How to deal with it?
  - Categorical value, as values from 1 to 23 have no meaning, ranking etc,
    except lower values indicate rather type A and higher values rather type B
    affiliation with region, BUT only more or less
Done  - construct var. for dominance of a party, where values all positive, no distinguishing between A & B, 
      the higher the values the less dominated a region is by a party, thus the more intense potential campaigns
Done  - construct var. for political direction of a region, e.g. 3 mostly party A, -3 mostly party B speaker
"}

# Install & Load Packages
{
  #install.packages("tidyverse")
  #install.packages("tidytext")
  #install.packages("tidymodels")
  
  library(tidyverse)
  library(tidytext)
  library(tidymodels)
}

# Load Data
{
  load("./Data/politicians.rdata")
}

# Mutate and Tidy data
{
  # Split column 'allspeeches' into tokens in column 'word', flattening the table into one-token-per-row
  data_unnested <- politicians %>%
    unnest_tokens(.word, allspeeches) %>%
    select(speaker, .word) %>%
    rename(.speaker = speaker)
  
  new_colnames <- paste0(".", colnames(politicians))
  colnames(politicians) <- new_colnames
  
  data <- politicians %>% # Select Data Politicians as base
    # More or Less vocal politicians could indicate party affiliation
    mutate(.corrupt = ifelse(.corrindex > 0, 1, 0), # categorical variable whether a politician is more or less corrupt than the average politician
           .number_speeches = str_count(.allspeeches, "\\t House of Commons Hansard Debates for ") + 1, # create variable with number of speeches per speaker in variable 'allspeeches'
           .number_words = str_count(.allspeeches, " ")+1, # Count the total number of words in .allspeeches
           .speechlength = .number_words/.number_speeches, # calc. average length of speech
           .birthplace = as.factor(.birthplace), # Transform birthplace to factor or categorical variable as higher or lower values have no ranking
           .party = as.factor(.party), # Transform party affiliation to factor
           .logincome = log(.income)) %>% # Transform income into log
    group_by(.birthplace) %>%
    # ASSUMPTION: all politicians within a birthplace are included in the data, or distribution and choice of entries are representative for a region
    mutate(.regionideology = sum(.party == 'A') - sum(.party == 'B'), # Identify Ideological tendence for each region
           .regionintensity = 1 - sum(.party == ifelse(sum(.party == 'A') > sum(.party == 'B'), 'A', 'B')) / n())  %>% # Identify the more prevalent party and calculate the ratio
    ungroup()
  
  apply(is.na(data), 2, which)
}
rm(politicians)

# Get quick overview of data - Summary of Data, Plotting main variables (corrindex, income, birthplace by Party)
# To-Do: Select output for paper and delete irrelevant memory things
{
  summary(data)
  "
  - age between 35 and 70 with mean of 47.33
  - 23 birthplaces
  - corruption index between -2.633 and 3.292 with mean of 0
  - income between 52,231 and 532,325 with mean of 112,303
  "
  
  # Plot corrindex against income by Party association
  {
    plot_outcome <- ggplot(data = data, 
                           aes(x = .logincome, y = .corrindex, color = .party, group = .party)) +
      geom_point()
    print(plot_outcome)
    
    "
  Plot indicates that for both parties there is a positive correlation between
  corrupt behavior and income, thus more corrupt behavior indicates higher income.
  Slightly indicates that individuals associated to party B have less corrupt
  behavior
  "
  }
  
  # Plot Birthplace and Party Affiliation
  {
    plot_birthplace <- ggplot(data = data, aes(x = .party, fill = .party)) +
      geom_bar(position = "dodge") +
      facet_wrap(~.birthplace, scales = "free_x", ncol = 3) +
      labs(title = "Speakers by Party and Birthplace",
           x = "Party",
           y = "Number of Speakers") +
      theme_minimal()
    
    print(plot_birthplace)
  }
  
  # Density Plot of Income and Corruption and  by Party
  {
    plot_density_income <- ggplot(data = data,
                                aes(x = .logincome, color = .party, group = .party)) +
      geom_density(linewidth = 1) +
      labs(x = "Income",
           y = "Density")
    print(plot_density_income)
    
    
    plot_density_corr <- ggplot(data = data,
                                aes(x = .corrindex, color = .party, group = .party)) +
      geom_density(linewidth = 1) +
      labs(x = "Corruption Index",
           y = "Density")
    print(plot_density_corr)
  }
  
  # Linear Regression to see correlations with Corruption index and Income 
  {
    # Create string with names of independent variables
    independent_var <- colnames(data %>% select(!c(".speaker", ".corrindex", ".birthplace", ".income", ".allspeeches", ".corrupt")))
    
    # Create linear regression models for corruption index and income
    basemodel_corr <- lm((reformulate(independent_var, response =".corrindex")), data = data)
    basemodel_income <- lm((reformulate(independent_var, response =".logincome")), data = data)
    
    # Pritn out results of simple regression models
    summary(basemodel_corr)
    summary(basemodel_income)
  }
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
    mutate(.sentiment = positive - negative) %>% # calculate overall sentiment index
    rename(.positive = positive,
           .negative = negative)
  
  data <- data %>% left_join(sentiment, join_by(.speaker == .speaker))
}

# Plot overall Sentiment
{
  plot_sentiment_inc <- ggplot(data = data,
                               aes(x=.sentiment, y = .logincome, color = .party, group = .party)) +
    geom_point() +
    geom_smooth(method='lm', formula=y~x, se=FALSE)
  plot_sentiment_inc
  
  plot_sentiment_corr <- ggplot(data = data,
                                aes(x=.sentiment, y = .corrindex, color = .party, group = .party)) +
    geom_point() +
    geom_smooth(method='lm', formula=y~x, se=FALSE)
  plot_sentiment_corr
}

rm(sentiment)
rm(sentiment_dict)

# Using words in speeches to predict Corruption and Income
{
  # Set a seed to reconstruct analyses
  set.seed(12345)
  # Define outcome variable
  y_var <- '.corrindex'
  
  # pick the words to keep as predictors
  {
    words_to_keep <- data_unnested %>%
      anti_join(get_stopwords(), join_by(.word  == word)) %>% # Drop words that are in dictionary stopwords, e.g.: I , me, my, myself... 
      count(.word) %>% # counts each individual word
      
      # is first filter necessary?
      filter(str_detect(.word, '.co|.com|.net|.edu|.gov|http', negate = TRUE)) |> # return all entries in 'word' that do NOT contain the listed words of URLs
      filter(str_detect(.word, '[0-9]', negate = TRUE)) |> # return all entries in 'words' that contain no numbers
      
      # How do I justify the value of 2?
      filter(n > 2) |> # take only words that occur more than two times
      pull(.word) # extract the column word as vector
  }
  
  # Construct Term Frequencies
  {
    tidy_speech <- data_unnested %>%
      filter(.word %in% words_to_keep) |> # Take only words that are in previous created list
      count(.speaker, .word) |> # count per speaker, identified through '.word' the number of a word occuring
      # Bind the term frequency and inverse document frequency of the data to the dataset
      bind_tf_idf(term = '.word', # Column containing terms as string or symbol
                  document = '.speaker', # Column containing document IDs as string or symbol
                  n = 'n') |> # Column containing document-term counts as string or symbol
      select(.speaker, .word, tf) |> # select ID, word and term-frequency column
      # pivot wider into a document-term matrix
      pivot_wider(id_cols = .speaker,
                  names_from = .word,
                  values_from = tf,
                  values_fill = 0)
  }
  
  # Join Term Frequency with dataset
  {
    tidy_speech <- data %>%
      select(!c(".allspeeches")) %>%
      right_join(tidy_speech, by = '.speaker') %>%
      ungroup()
  }
  
  # Create train and test sample from data merge with Term Frequency
  # NEED TO JUSTIFY 08. - 0.2 division
  {
    # split sample into training and test sample
    data_split <- initial_split(tidy_speech, prop = 0.8) # split into 80% training and 20% test
    
    train <- training(data_split)
    test <- testing(data_split)
  }
  
  # Remove dataset and clear memory for computation
  {
    rm(data_split)
    rm(data_unnested)
    rm(words_to_keep)
    gc()
  }
  
  # Create Model with corruption index as outcome variable
  {
    # (1) Simple Model
    
    # (2) Complex Model - Overfitting
    {
      # Linear Regression with all words
      model_comp_corr <- linear_reg () %>%
        fit(reformulate('.', response = y_var),
            data = train %>% select(-.speaker))
      
      model_comp_corr_t <- lm(reformulate('.', response = y_var), data = train %>% select(-.speaker))
      
      # Plot Fits
      {
        # Plot in-sample Fit
        plot_fit_comp_corr_in <- ggplot(data = (train %>% bind_cols(predict(model_comp_corr, train))),
                                        aes(x = .pred, y = .corrindex)) + geom_point()
        print(plot_fit_comp_corr_in)
        
        # Plot out-of-sample fit
        plot_fit_comp_corr_out <- ggplot(data = (test %>% bind_cols(predict(model_comp_corr, test))),
                                         aes(x = .pred, y = .corrindex)) +  geom_point()
        print(plot_fit_comp_corr_out)
        }
      
      # Plot Words most strongly correlated
      {
        tidy(model_comp_corr)
      }
    }
    
    # (3) Regularized Model - LASSO
    {
      # Linear regression with LASSO penalty
      model_regul_corr <- linear_reg(penalty = 0.01, mixture = 1) %>%
        set_engine('glmnet') %>%
        fit(reformulate('.', response = y_var), data = train %>% select(-.speaker))
      
      # Plot Fits
      {
        plot_fit_regul_corr_in <- ggplot(data = (train %>% bind_cols(predict(model_regul_corr, train))),
                                         aes(x = .pred, y = .corrindex)) + geom_point()
        print(plot_fit_regul_corr_in)
        
        # Plot out-of-sample fit
        plot_fit_regul_corr_out <- ggplot(data = (test %>% bind_cols(predict(model_regul_corr, test))),
                                          aes(x = .pred, y = .corrindex)) +  geom_point()
        print(plot_fit_regul_corr_out)
        }
      
      tidy(model_regul_corr)
    }
  }
}

