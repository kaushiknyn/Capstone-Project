#Load libraries
library(dplyr)
library(ggplot2)
library(tm)
library(RWeka)
library(SnowballC)
library(wordcloud)
library(ngram)
library(textstem)

# Load the data


en_US_blog <- readLines("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
en_US_news <- readLines("./Coursera-SwiftKey/final/en_US/en_US.news.txt")
en_US_twitter <- readLines("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt")


# Get basic information about the files

## Word counts

Wordcount_blog <- wordcount(en_US_blog, sep = " ", count.function = sum)

Wordcount_news <- wordcount(en_US_news, sep = " ", count.function = sum)

Wordcount_twitter <- wordcount(en_US_twitter, sep = " ", count.function = sum)


## Length 

Length_blog <- length(en_US_blog)

Length_news <- length(en_US_news)

Length_twitter <- length(en_US_twitter)

## File size

Size_blog <- file.info("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt")$size / (1024*1024)

Size_news <- file.info("./Coursera-SwiftKey/final/en_US/en_US.news.txt")$size / (1024*1024)

Size_twitter <- file.info("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt")$size / (1024*1024)

## Summarry

Data_summary <- data.frame(c("Blog", "News", "Twitter"),
                           c(Wordcount_blog,Wordcount_news,Wordcount_twitter),
                           c(Length_blog, Length_news, Length_twitter),
                           c(round(Size_blog, 2), round(Size_news, 2), round(Size_twitter,2)))

colnames(Data_summary) <- c("Data", "Word Count", "Length", "File Size (MB)")

Data_summary

# Sample data

set.seed(15687)

blog_sample <- en_US_blog[sample(1:length(en_US_blog), 10000)]

news_sample <- en_US_news[sample(1:length(en_US_news), 10000)]

twitter_sample <- en_US_twitter[sample(1:length(en_US_twitter), 10000)]

sample_data <- c(blog_sample, news_sample, twitter_sample)

writeLines(sample_data, "./Sample/sample_data.txt")

# Remove temporary variables

rm(en_US_blog, en_US_news, en_US_twitter, blog_sample, news_sample, twitter_sample, Length_blog,
   Length_news, Length_twitter, Size_blog, Size_news, Size_twitter, Wordcount_blog, Wordcount_news,
   Wordcount_twitter)


# Create Corpus

docs <- Corpus(DirSource('./Sample'))

# Remove URL, Twitter handles and email patterns

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
docs <- tm_map(docs, toSpace, "@[^\\s]+")
docs <- tm_map(docs, toSpace, "\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b")

# # Remove profane words
# 
# ## Download profane words file
# 
# badWordsURL <- "http://www.idevelopment.info/data/DataScience/uploads/full-list-of-bad-words_text-file_2018_07_30.zip"
# badWordsFile <- "data/full-list-of-bad-words_text-file_2018_07_30.txt"
# if (!file.exists('data')) {
#   dir.create('data')
# }
# if (!file.exists(badWordsFile)) {
#   tempFile <- tempfile()
#   download.file(badWordsURL, tempFile)
#   unzip(tempFile, exdir = "data")
#   unlink(tempFile)
# }

# Convert to lowercase

docs <- tm_map(docs, content_transformer(tolower))

# Other Transformations


docs <- tm_map(docs, toSpace, "/|@|\\|")

# Remove punctuation

docs <- tm_map(docs, removePunctuation)

# Remove numbers

docs <- tm_map(docs, removeNumbers)

# Strip White Space

docs <- tm_map(docs, stripWhitespace)

# Remove stop words

docs <- tm_map(docs, removeWords, stopwords("english"))

# Perform lemmatization

docs <-tm_map(docs, lemmatize_strings)

docs <- tm_map(docs, PlainTextDocument)

# N-gram tokenization

## Unigram toeknization
token_uni <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

documentMatrix_uni <- DocumentTermMatrix(docs, control = list(tokenize = token_uni))

## Bigram tokenization
token_bi <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

documentMatrix_bi<- DocumentTermMatrix(docs, control = list(tokenize = token_bi))

## Trigram tokenization
token_tri <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

documentMatrix_tri <- DocumentTermMatrix(docs, control = list(tokenize = token_tri))

# Unigram Exploratory data analysis

## Calculate frequency of words
freq_uni <- sort(colSums(as.matrix(documentMatrix_uni)), decreasing = TRUE)

## Create histogram of unigrams with frequency > 1000

df_uni <- data.frame(word =  names(freq_uni), frequency = freq_uni)

df_uni %>% filter(frequency > 1000) %>%
            ggplot(aes(word, frequency)) +
            geom_bar(stat = "identity") +
            ggtitle("Unigrams with frequencies > 1000") +
            xlab("Unigrams") + ylab("Frequency") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

## Create wordcloud of unigrams with frequency > 1000

set.seed(15)

wordcloud(names(freq_uni), freq_uni, min.freq = 1000, colors = brewer.pal(6, "Dark2"))



# Bigram Exploratory data analysis

## Calculate frequency of words
freq_bi <- sort(colSums(as.matrix(documentMatrix_bi)), decreasing = TRUE)


## Create histogram of unigrams with frequency > 100

df_bi <- data.frame(word =  names(freq_bi), frequency = freq_bi)

df_bi %>% filter(frequency > 100) %>%
  ggplot(aes(word, frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Bigrams with frequencies > 100") +
  xlab("Bigrams") + ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

## Create wordcloud of bigrams with frequency > 100

set.seed(45)

wordcloud(names(freq_bi), freq_bi, min.freq = 100, colors = brewer.pal(6, "Dark2"))

# Trigram Exploratory data analysis

## Calculate frequency of words
freq_tri <- sort(colSums(as.matrix(documentMatrix_tri)), decreasing = TRUE)


## Create histogram of trigrams with frequency > 10

df_tri <- data.frame(word =  names(freq_tri), frequency = freq_tri)

df_tri %>% filter(frequency > 10) %>%
  ggplot(aes(word, frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Trigrams with frequencies > 10") +
  xlab("Trigrams") + ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

## Create wordcloud of bigrams with frequency > 10

set.seed(45)

wordcloud(names(freq_tri), freq_tri, min.freq = 10, colors = brewer.pal(6, "Dark2"))


