# goal: estimate topic model (Latent Dirichlet Model) for news articles from AP
# to study major topics

library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)


# load data of news articles from the Associated Press
data("AssociatedPress")

# fit an LDA model wiht 10 topics
AP_topic_model <- LDA(AssociatedPress, k=10, control = list(seed = 321))

# obtain the topic-specific probability vectors theta_1,...,theta_K
# (in the LDA output, these are denoted by "beta")
AP_topics <- tidy(AP_topic_model, matrix = "beta")

# find top 10 terms in each topic
ap_top_terms <- 
  AP_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# plot topic-specific probabilities of the top words in each topic
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

