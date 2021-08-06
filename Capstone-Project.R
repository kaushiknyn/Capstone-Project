#Load libraries
library(dplyr)
library(ggplot2)
library(tm)

# Create connections and load the data

con_blog <- file("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt","r")
con_news <- file("./Coursera-SwiftKey/final/en_US/en_US.news.txt","r")
con_twitter <- file("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt","r")

en_US_blog <- readLines("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
en_US_news <- readLines("./Coursera-SwiftKey/final/en_US/en_US.news.txt")
en_US_twitter <- readLines("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt")



#Get basic information about the files

#Word counts

Wordcount_blog <- wordcount(en_US_blog, sep = " ", count.function = sum)

Wordcount_news <- wordcount(en_US_news, sep = " ", count.function = sum)

Wordcount_twitter <- wordcount(en_US_twitter, sep = " ", count.function = sum)


# Length 

Length_blog <- length(en_US_blog)

Length_news <- length(en_US_news)

Length_twitter <- length(en_US_twitter)

# File size

Size_blog <- file.info("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt")$size / (1024*1024)

Size_news <- file.info("./Coursera-SwiftKey/final/en_US/en_US.news.txt")$size / (1024*1024)

Size_twitter <- file.info("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt")$size / (1024*1024)

# Summarry

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

writeLines(sample_data, "./sample_data.txt")

# Remove temporary variables

rm(en_US_blog, en_US_news, en_US_twitter, blog_sample, news_sample, twitter_sample)


# Create Corpus

docs <- Corpus(DirSource('./Sample'))


# Convert to lowercase

docs <- tm_map(docs, content_transformer(tolower))

# Remove stop words

docs <- tm_map(docs, removeWords, stopwords("english"))

toSpace <- content_transformer(function(x, pattern){return (gsub(pattern, " ", x))})

docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, ";")
docs <- tm_map(docs, toSpace, ",")
docs <- tm_map(docs, toSpace, "<")
docs <- tm_map(docs, toSpace, ".")
docs <- tm_map(docs, toSpace, ">")
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "?")
docs <- tm_map(docs, toSpace, "[")
docs <- tm_map(docs, toSpace, "{")
docs <- tm_map(docs, toSpace, "]")
docs <- tm_map(docs, toSpace, "}")
docs <- tm_map(docs, toSpace, "\\")
docs <- tm_map(docs, toSpace, "|")
docs <- tm_map(docs, toSpace, "!")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "#")
docs <- tm_map(docs, toSpace, "$")
docs <- tm_map(docs, toSpace, "%")
docs <- tm_map(docs, toSpace, "^")
docs <- tm_map(docs, toSpace, "&")
docs <- tm_map(docs, toSpace, "*")
docs <- tm_map(docs, toSpace, "(")
docs <- tm_map(docs, toSpace, ")")
docs <- tm_map(docs, toSpace, "_")
docs <- tm_map(docs, toSpace, "+")
docs <- tm_map(docs, toSpace, "=")
