#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("tidymodels")
#install.packages("glmnet")
#install.packages("glmnetUtils")
#install.packages("wordcloud")
#install.packages("patchwork")
#install.packages("hdm")
#install.packages("grf")
#install.packages("topicmodels")
#install.packages("SnowballC")

library(tidyverse)
library(tidytext)
library(tidymodels)
library(glmnet)
library(glmnetUtils)
library(wordcloud)
library(patchwork)
library(hdm)
library(grf)
library(topicmodels)
library(SnowballC)

load("./Data/politicians.rdata")


### MUTATE
# Split column 'allspeeches' into tokens in column 'word', flattening the table into one-token-per-row
data_unnested <- politicians %>%
  unnest_tokens(.word, allspeeches) %>%
  select(speaker, .word) %>%
  rename(.speaker = speaker)

new_colnames <- paste0(".", colnames(politicians))
colnames(politicians) <- new_colnames

data <- politicians %>% 
  mutate(.corrupt = ifelse(.corrindex > 0, 1, 0), # Dummy whether more or less corrupt than the avg. politician
         .number_speeches = str_count(.allspeeches, "\\t House of Commons Hansard Debates for ") + 1, # create variable with number of speeches per speaker in variable 'allspeeches'
         .number_words = str_count(.allspeeches, " ") + 1, # Count total number of words
         .partydummy = ifelse(.party == "B", 1, 0), # Transform to dummy for processing power
         .logincome = log(.income))# Transform income into log

# Transform Categorical variables to dummies, as transformation to as.factor() needs to much processing
for (i in 1:21) {
  new_col <- paste0(".birthplace", i)
  data[new_col] <- ifelse(data$.birthplace==i, 1, 0)
}

apply(is.na(data), 2, which)

rm(i)
rm(new_col)
rm(new_colnames)



### SENTIMENT
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

rm(sentiment)
rm(sentiment_dict)



### Prepare Speech Analysis
# Set a seed to reconstruct analyses
set.seed(12345)
# Define variables
word_type <- '.word' # Choose between '.word' or '.stem'

# pick the words to keep as predictors
# Choose between taking full words or word stems only (words = 16.6k vs. stem = 12k entries)
{
  words_to_keep <- unique(data_unnested %>% # Drop double entries (drops words from 484k to 16,603) with smart to 16,280 and .stem dropping to 12,030
                            anti_join(get_stopwords(source="smart"), # standard=175 words vs, smart 571 words
                                      join_by(.word  == word)) %>% # Drop words that are in dictionary stopwords, e.g.: I , me, my, myself...
                            mutate(.wordtype = if(word_type == '.word'){.word}
                                   else{wordStem(.word)}) %>%
                            count(.wordtype) %>%
                            filter(str_detect(.wordtype, '.co|.com|.net|.edu|.gov|http', negate = TRUE)) |> # return all entries in 'word' that do NOT contain the listed words of URLs
                            filter(str_detect(.wordtype, '[0-9]', negate = TRUE)) |> # return all entries in 'words' that contain no numbers
                            filter(n > 2) |> # take only words that occur more than two times
                            pull(.wordtype)) # extract the column word as vector
}

# Construct Term Frequencies
{
  tidy_speech <- data_unnested %>%
    transmute(.wordtype = if(word_type == '.word'){.word}
              else{wordStem(.word)}) %>%
    rename(.word = .wordtype) %>%
    filter(.word %in% words_to_keep) %>% # Take only words that are in previous created list
    count(.speaker, .word) %>% # count per speaker, identified through '.word' the number of a word occuring
    # Bind the term frequency and inverse document frequency of the data to the dataset
    bind_tf_idf(term = '.word', # Column containing terms as string or symbol
                document = '.speaker', # Column containing document IDs as string or symbol
                n = 'n') %>% # Column containing document-term counts as string or symbol
    select(.speaker, .word, tf) %>% # select ID, word and term-frequency column
    
    filter(.word != "") %>% # Filter out EMPTY strings
    # pivot wider into a document-term matrix
    pivot_wider(id_cols = '.speaker',
                names_from = '.word',
                values_from = 'tf',
                values_fill = 0)
}

# Join Term Frequency with dataset
{
  tidy_speech <- data %>%
    select(!c(".allspeeches")) %>%
    right_join(tidy_speech, by = '.speaker') %>%
    ungroup()
}



# 2 Conduct Speech Analysis
## Split Data and Define Variables
# Set a seed to reconstruct analyses
set.seed(12345)
y_var <- '.logincome' # Choose between '.corrindex' or '.logincome'

# Create train and test sample
# Too little data for stratification by BIRTHPLACE
{
  # split sample into training and test sample
  data_split <- initial_split(tidy_speech, prop = 0.8) # split into 80% training and 20% test
  
  train <- training(data_split)
  test <- testing(data_split)
}

# Define Variables
{
  confounder_names <- c(".speaker", # Identifier
                        ".corrindex", ".corrupt", ".income", ".logincome", # Outcome Variables
                        ".party", ".partydummy", # Target Parameter
                        grep("\\.birthplace", colnames(train), value =TRUE)) # Instrument
  # Train Data
  Y_train <- train[y_var] # Outcome Variable
  W_train <- train[, !(colnames(train) %in% confounder_names)]# Confounders - Nuisance Parameters
  D_train <- train['.partydummy'] # Target Parameter
  Z_train <- train %>% select(starts_with(".birthplace") & !(".birthplace")) # Instrument
  X_train <- cbind(D_train, Z_train, W_train) # Observed Regressors - 16,635 variables
  
  # Test Data
  Y_test <- test[y_var] # Outcome Variable
  W_test <- test[, !(colnames(test) %in% confounder_names)] # Covariates - Nuisance Parameters
  D_test <- test['.partydummy'] # Target Parameter
  Z_test <- test %>% select(starts_with(".birthplace") & !(".birthplace")) # Instrument
  X_test <- cbind(D_test, Z_test, W_test) # Observed Regressors - 16,635 variables
  
  # Full Data
  Y_full <- tidy_speech[y_var] # Outcome Variable
  W_full <- tidy_speech[, !(colnames(tidy_speech) %in% confounder_names)] # Covariates - Nuisance Parameters
  D_full <- tidy_speech['.partydummy'] # Target Parameter
  Z_full <- tidy_speech %>% select(starts_with(".birthplace") & !(".birthplace")) # Instrument
  X_full <- cbind(D_full, Z_full, W_full) # Observed Regressors - 16,635 variables
  
  # Other
  simplemodels_list <- list() # List containing models
  in_sample_plots <- list() # List for in-sample fits of models
  out_sample_plots <- list() # List for out-of-sample fits of models
  name_outcome <- ifelse(y_var == ".corrindex", "corr", "inc")
  words_hetero <- c() # vector containing non-zero coefficients
}


### 2.1 Variable Selection
{
# Set a seed to reconstruct analyses
set.seed(12345)

# Train Models
{
  # (3) Regularized Model - LASSO - lambda Cross-validated (CV)
  simplemodels_list[[paste0("model_cvregul_", name_outcome)]]<- cv.glmnet(x = as.matrix(X_train), 
                                                                          y = as.matrix(Y_train),
                                                                          nfolds = 10, 
                                                                          type.measure = "mse")
  
  # (4) Double LASSO - CV
  {
    # Partial out W (confounders) from Y (outcome)
    fit.lasso.Y <- cv.glmnet(x=as.matrix(W_train), y=as.matrix(Y_train))
    fitted.lasso.Y <- predict(fit.lasso.Y, newx=as.matrix(W_train), s="lambda.min")
    Ytilde_corr <- as.matrix(Y_train) - fitted.lasso.Y
    
    # Partial out w(confounders) from D(Party Affiliation)
    fit.lasso.D <- cv.glmnet(x=as.matrix(W_train), y=as.matrix(D_train))
    fitted.lasso.D <- predict(fit.lasso.D, newx=as.matrix(W_train), s="lambda.min")
    Dtilde_corr <- as.matrix(D_train) - fitted.lasso.D
    
    # Run OLS of Ytilde on Dtilde
    fit.doubleLASSO <- lm(Ytilde_corr ~ Dtilde_corr)
    beta1hat <- coef(fit.doubleLASSO)[2]
    print(beta1hat)
  }
}

# Extract non-zero coefficients from Regularizing models
{
  # (3) LASSO - lambda = CV
  words_hetero <- append(words_hetero,
                         as.data.frame(
                           as.matrix(
                             coef(simplemodels_list[[paste0("model_cvregul_", 
                                                            name_outcome)]],
                                  s = "lambda.min"))) %>%
                           tibble::rownames_to_column(var = "term") %>%
                           filter(s1 != 0 & !grepl("^\\.", term)) %>%
                           pull(term))
  
  # (4) Double LASSO --- fit.lasso.D; fit.lasso.Y
  words_hetero <- append(words_hetero,
                         as.data.frame(as.matrix(coef(fit.lasso.D, s = "lambda.min"))) %>%
                           tibble::rownames_to_column(var = "term") %>%
                           filter(s1 != 0 & !grepl("^\\.", term)) %>%
                           pull(term))
  words_hetero <- append(words_hetero,
                         as.data.frame(as.matrix(coef(fit.lasso.Y, s = "lambda.min"))) %>%
                           tibble::rownames_to_column(var = "term") %>%
                           filter(s1 != 0 & !grepl("^\\.", term)) %>%
                           pull(term))
  
  words_hetero <- setdiff(unique(words_hetero), "(Intercept)")
}


### 2.2 Estimate Party Affiliation effect on Outcome Variables
{
  W_selec <- W_full %>% select(starts_with("."), all_of(words_hetero))
  
  # Create interaction term - TO BE ADAPTED
  {
    # Create  Interaction terms of .partydummy with all previously identified covariates
    D_interact <- cbind(D_full, W_selec) %>%
      mutate(across(everything(), ~ . * .partydummy, .names = ".partydummyx{.col}")) %>%
      select(where(~ var(.) != 0) & starts_with(".party")) %>%
      select(!.partydummyx.partydummy)
    X_interact <- cbind(D_interact, W_selec)
  }
  
  index_interact <- grep("^\\.party", colnames(X_interact))
}

# Double LASSO with all units of observation but only selected words - HETEROGENITY
# OLS estimation of residuals  not inherently robust to treatment effect heterogeneity
{
  # Heterogeneity for Logincome with selected words
  fit.doubleLASSO.inter.inc <- rlassoEffects(x=X_interact,
                                             y=tidy_speech$.logincome,
                                             index=index_interact,
                                             method='partialling out')
  fit.doubleLASSO.inter.corr <- rlassoEffects(x=X_interact,
                                              y=tidy_speech$.corrindex,
                                              index=index_interact,
                                              method='partialling out')
  save(fit.doubleLASSO.inter.inc, file="fit.doubleLASSO.inter.inc.RData")
  save(fit.doubleLASSO.inter.corr, file="fit.doubleLASSO.inter.corr.RData")
}

  
  
  
  
### TOPIC MODEL
{
  # Some standard words are in every parliamentary speech and not filtered out by 'stopwords'
  special_list <- c("hon", "government", "people", "mr", "mrs", "one", "can", 
                    "also","member", "members", "house", "commitee")
  
  # Transform data in readable format for 'LDA'
  speech_dtm <- data_unnested %>%
    transmute(.wordtype = if(word_type == '.word'){.word}
              else{wordStem(.word)}) %>%
    rename(.word = .wordtype) %>%
    filter(.word %in% words_to_keep) %>% # Take only words that are in previous created list
    count(.speaker, .word) %>%
    filter(n > 2 & nchar(.word) > 2 & !(.word %in% special_list)) %>% # words that occurred more than two times
    cast_dtm(.speaker, .word, n)

  # Run LDA
  topic_model <- LDA(speech_dtm, k=10, control=list(seed = 12345))
  
  # Derive
  topicsprob10 <- as.data.frame(posterior(topic_model)$topics)# Extract topic probabilities
  colnames(topicsprob10) <- paste0(".topic", 1:ncol(topicsprob10)) # Rename columns
  tidy_speech <- cbind(tidy_speech, topicsprob10)
}
  
## DEFINE Variables
{
  tidy_speech <- tidy_speech %>% select(starts_with("."))
    # Set a seed to reconstruct analyses
    set.seed(12345)
    y_var <- '.logincome' # Choose between '.corrindex' or '.logincome'
    
    # Create train and test sample
    # Too little data for stratification by BIRTHPLACE
    {
      # split sample into training and test sample
      data_split <- initial_split(tidy_speech, prop = 0.8) # split into 80% training and 20% test
      
      train <- training(data_split)
      test <- testing(data_split)
    }
    
    # Define Variables
    {
      confounder_names <- c(".speaker", # Identifier
                            ".corrindex", ".corrupt", ".income", ".logincome", # Outcome Variables
                            ".party", ".partydummy", # Target Parameter
                            grep("\\.birthplace", colnames(train), value =TRUE)) # Instrument
      
      # Full Data
      Y_full <- tidy_speech[y_var] # Outcome Variable
      W_full <- tidy_speech[, !(colnames(tidy_speech) %in% confounder_names)] # Covariates - Nuisance Parameters
      D_full <- tidy_speech['.partydummy'] # Target Parameter
      Z_full <- tidy_speech %>% select(starts_with(".birthplace") & !(".birthplace")) # Instrument
      X_full <- cbind(D_full, Z_full, W_full) # Observed Regressors - 16,635 variables
      
      name_outcome <- ifelse(y_var == ".corrindex", "corr", "inc")
    }
}
  
# Filter data on relevant words only
  {
    W_selec <- W_full 
    
    # Create interaction term - TO BE ADAPTED
    {
      # Create  Interaction terms of .partydummy with all previously identified covariates
      D_interact <- cbind(D_full, W_selec) %>%
        mutate(across(everything(), ~ . * .partydummy, .names = ".partydummyx{.col}")) %>%
        select(where(~ var(.) != 0) & starts_with(".party")) %>%
        select(!.partydummyx.partydummy)
      X_interact <- cbind(D_interact, W_selec)
    }
    
    index_interact <- grep("^\\.party", colnames(X_interact))
  }

  ### ANALYSIS
  # Heterogeneity for Logincome with selected words
  fit.doubleLASSO.inter.inc.topic <- rlassoEffects(x=X_interact,
                                             y=tidy_speech$.logincome,
                                             index=index_interact,
                                             method='partialling out')
  fit.doubleLASSO.inter.corr.topic <- rlassoEffects(x=X_interact,
                                              y=tidy_speech$.corrindex,
                                              index=index_interact,
                                              method='partialling out')
  save(fit.doubleLASSO.inter.inc.topic, file="fit.doubleLASSO.inter.inc.topicRData")
  save(fit.doubleLASSO.inter.corr.topic, file="fit.doubleLASSO.inter.corr.topicRData")