# Read in libraries
library(tidyverse)
library(sentimentr) # For sentiment analysis 
library(caret)
library(quanteda)
library(broom)
library(syuzhet) ) # For nrc sentiment analysis 
library(vroom)

# Read the data with comments included
SS <- vroom("SSComments.csv")
IE <- vroom("IEComments.csv")
CB <- vroom("CBComments.csv")
LLS <- vroom("LLSComments.csv")

# Read in the csvs created in TextAnalyticsCleaning.R
csv_SS <- vroom("StudentRatingsSS.csv")
csv_IE <- vroom("StudentRatingsIE.csv")
csv_CB <- vroom("StudentRatingsCB.csv")
csv_LLS <- vroom("StudentRatingsLLS.csv")

# Why sentiment analysis? "For reviews, aspect-based sentiment analysis is easier because the entity (i.e., product name)
# is usually known. Reviewers simply express positive and negative opinions on different aspects of the entity."


# Methods I tried to add sentiment analysis to the existing data:
# 1. At the sentence level 
      # - Using syuzhet NRC 
      # - Using sentimentr library 
# 2. At the document level

# Syuzhet Sentence Level-------------------------------------------------------------------
# This method did a bad job at registering the complexity/nuances in student responses

# Compute NRC sentiment
nrc_data <- get_nrc_sentiment(SS$Response_1)
df_combined <- bind_cols(SS, nrc_data)
View(df_combined)

# Can specify the comments for each emotion that you are interested in seeing
angry_items <- which(nrc_data$anger > 0)
SS$Response_1[angry_items]

# Bar plot
barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Student Ratings", xlab="Percentage",
)

#Sentimentr Sentence Level---------------------------------------------------------------------------------------------------
# 1. Sentence level sentiment analysis 
# Using sentimentr we can calculate text polarity sentiment at the sentence level and 
# aggregate by rows or grouping variable(s). Can address negations and other valence shifters.

# Join Faculty_Member, Semester, and course to create the document_id
data <- LLS |> 
  rename(Faculty_Member = "Faculty Member") |> 
  mutate(document_id = paste(Semester, Faculty_Member, Course, sep = "_"))

# Select document id, response, and response1, 
data <- data %>%
  dplyr::select(document_id, Response, Response_1)

# Checks instances of group id
data |> 
  dplyr::select(document_id) |> 
  group_by(document_id) |> 
  summarize(total = n()) |> 
  View()

# Checks the responses to see if that lines up with our out dataframe
data |> 
  group_by(document_id) |> 
  View()

# Realized issue with the disparity between the # of document ids is because I grouped the sentiments by responses originally
incorrect_method <- with(
  data,
  sentiment_by(
    get_sentences(Response_1), # Reviews are stored in variable Description
    list(document_id, Response) # grouping variables
  ))

# What it looks like
incorrect_method |> 
  group_by(document_id) |> 
  summarize(total = n()) |> 
  View()

# This is how we actually want to do the analysis 
# If we only group by document_id, output is only one per doc which lines up with our final data frames 
out <- with(
  data,
  sentiment_by(
    get_sentences(Response_1), # Reviews are stored in variable Description
    list(document_id) # grouping variables
  ))

# Note: word_count represents the total words from the combined reviews per document_id

# There are instances of NA in the out dataset.
# This happens when there is only one instance of a review for a certain document_id
out %>% filter(is.na(sd))

# Replacing the NA standard deviations with zero
out$sd[is.na(out$sd)] <- 0.00000000


# Combining datasets based on document_id
# Be sure to change based on which csv you are using
finalized_csv <- csv_LLS |> 
  left_join(out |> group_by(document_id)) |> 
  arrange(document_id)

# Write out the file
vroom::vroom_write(x=finalized_csv, file="SentimentAnalysisLLS.csv", delim=",")

#Document Level Sentiment Analysis---------------------------------------------------------------------------------------------------
# Computes at level of documents (comments in our case)

# Creating a corpus
my_corpus <-corpus(data, text_field="Response_1")

# How to view just the comments
head(my_corpus,5)

# How to view entire corpus (aka practically the dataframe)
summary(my_corpus,10)

# Bing Liu's words
positive_words_bing <- scan("positive-words.txt", what = "char", sep = "\n", skip = 35, quiet = T)
negative_words_bing <- scan("negative-words.txt", what = "char", sep = "\n", skip = 35, quiet = T)

# Our dictionary, a list of words to be applied to our dataset
sentiment_bing <- dictionary(list(positive = positive_words_bing, negative = negative_words_bing))

# Can use dictionary to create lexicon and test for sentiment
dfm_sentiment <- my_corpus %>% tokens() %>% dfm %>% dfm_lookup(sentiment_bing)
dfm_sentiment

# Can do a document level summary
dfm_sentiment_df<-convert(dfm_sentiment, to ='data.frame')
dfm_sentiment_df$net<-(dfm_sentiment_df$positive)-(dfm_sentiment_df$negative)
summary(dfm_sentiment_df)# document level summary

# Proportions instead of numbers
dfm_sentiment_prop <- dfm_weight(dfm_sentiment, scheme = "prop")
dfm_sentiment_prop

## Plotting the sentiments
sentiment <- convert(dfm_sentiment_prop, "data.frame") %>%
  gather(positive, negative, key = "Polarity", value = "Share") %>%
  mutate(document = as_factor(doc_id)) %>%
  rename(Comment = document)

ggplot(sentiment, aes(Comment, Share, fill = Polarity, group = Polarity)) +
  geom_bar(stat='identity', position = position_dodge(), size = 1) +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ggtitle("Sentiment scores in Student Ratings Comments (relative)")