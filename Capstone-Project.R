# Load libraries
library(dplyr)
library(ggplot2)
library(tm)
library(RWeka)
library(SnowballC)
library(wordcloud)
library(ngram)
library(textstem)

# Load the data


en_US_blog <- readLines("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt", encoding = 'UTF-8')
en_US_news <- readLines("./Coursera-SwiftKey/final/en_US/en_US.news.txt", encoding = 'UTF-8')
en_US_twitter <- readLines("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt", encoding = 'UTF-8')


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


# Convert to lowercase

docs <- tm_map(docs, content_transformer(tolower))

# Other Transformations

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/|@|\\|")

# Remove punctuation

docs <- tm_map(docs, removePunctuation)

# Remove numbers

docs <- tm_map(docs, removeNumbers)

# Strip White Space

docs <- tm_map(docs, stripWhitespace)

# Remove stop words

docs <- tm_map(docs, removeWords, stopwords("english"))

# Perform stemming

docs <-tm_map(docs, stemDocument)

docs <- tm_map(docs, PlainTextDocument)

# N-gram tokenization

## Unigram toeknization
Unigram <- NGramTokenizer(docs, Weka_control(min = 1, max = 1))


## Bigram tokenization
Bigram <- NGramTokenizer(docs, Weka_control(min = 2, max = 2)) 


## Trigram tokenization
Trigram <- NGramTokenizer(docs, Weka_control(min = 3, max = 3)) 


# Unigram Exploratory data analysis


## Create histogram of unigrams with frequency > 1000

df_uniwordfreq <- data.frame(table(Unigram))

df_uniwordfreq <- df_uniwordfreq[order(df_uniwordfreq$Freq, decreasing = TRUE),]

df_uniwordfreq %>% 
  filter(Freq > 1000) %>%
  ggplot(aes(Unigram,Freq)) +
  geom_bar(stat="identity") +
  ggtitle("Unigrams with frequencies > 1000") +
  xlab("Unigrams") + ylab("Frequency") +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        axis.text.x=element_text(angle=45, hjust=1))

## Create wordcloud of top 50 unigrams

set.seed(15)

wordcloud(df_uniwordfreq$Unigram, df_uniwordfreq$Freq, max.words=50, colors = brewer.pal(6, "Dark2"))


# Bigram Exploratory data analysis

## Create histogram of unigrams with frequency > 100

df_biwordfreq <- data.frame(table(Bigram))

df_biwordfreq <- df_biwordfreq[order(df_biwordfreq$Freq, decreasing = TRUE),]

df_biwordfreq %>% 
  filter(Freq > 100) %>%
  ggplot(aes(Bigram,Freq)) +
  geom_bar(stat="identity") +
  ggtitle("Bigrams with frequencies > 100") +
  xlab("Bigrams") + ylab("Frequency") +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        axis.text.x=element_text(angle=45, hjust=1))

## Create wordcloud of top 50 unigrams

set.seed(45)

wordcloud(df_biwordfreq$Bigram, df_biwordfreq$Freq, max.words=50, colors = brewer.pal(6, "Dark2"))
# Trigram Exploratory data analysis

## Calculate frequency of words
tm_trifreq <- sort(colSums(as.matrix(tridtm)), decreasing=TRUE)

## Create histogram of trigrams with frequency > 10

## Create histogram of trigrams with frequency > 10

df_triwordfreq <- data.frame(table(Trigram))

df_triwordfreq <- df_triwordfreq[order(dftriwordfreq$Freq, decreasing = TRUE),]

df_triwordfreq %>% 
  filter(Freq > 10) %>%
  ggplot(aes(Trigram,Freq)) +
  geom_bar(stat="identity") +
  ggtitle("Trigrams with frequencies > 10") +
  xlab("Trigrams") + ylab("Frequency") +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        axis.text.x=element_text(angle=45, hjust=1))

## Create wordcloud of top 50 unigrams

set.seed(75)

wordcloud(df_triwordfreq$Trigram, df_triwordfreq$Freq, max.words=50, colors = brewer.pal(6, "Dark2"))

one_gram <- NGramTokenizer(docs, Weka_control(min = 1, max = 1))

two_gram <- NGramTokenizer(docs, Weka_control(min = 2, max = 2)) 

three_gram <- NGramTokenizer(docs, Weka_control(min = 3, max = 3)) 


