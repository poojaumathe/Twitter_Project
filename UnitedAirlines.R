# Install packages

install.packages("ROAuth")

install.packages("twitteR")
library(twitteR)

install.packages("NLP")
library("NLP")

install.packages("tm")
library(tm)

install.packages("tmap")
library(tmap)

install.packages("syuzhet")
library("syuzhet")

install.packages("SnowballC")
library("SnowballC")

library("stringi")

install.packages("topicmodels")
library("topicmodels")

install.packages("wordcloud")
library("wordcloud")

install.packages("ggplot2")
library("ggplot2")



# Defining keys to access twitter data

consumer_key <- "XXXX"
consumer_secret <-"XXXX"
access_token <- "XXXX"
access_secret <- "XXXX" 


# Getting authentication from twitter to access data 

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Getting Data 

tweets <- searchTwitter("#UnitedAirlines", n=1000,lang = "en")

# Converting this extracted data to a dataframe

tweets_UN <- twListToDF(tweets)



#Displaying tweets

View(tweets_UN)


# Data Preprocessing

UN_text<- tweets_UN$text



#convert all text to lower case

UN_text<- tolower(UN_text)


# Replace blank space ("rt")

UN_text <- gsub("rt", "", UN_text)



# Replace @UserName

UN_text <- gsub("@\\w+", "", UN_text)



# Remove punctuation

UN_text <- gsub("[[:punct:]]", "", UN_text)



# Remove links

UN_text <- gsub("http\\w+", "", UN_text)



# Remove tabs

UN_text <- gsub("[ |\t]{2,}", "", UN_text)


# Remove blank spaces at the beginning

UN_text <- gsub("^ ", "", UN_text)



# Remove blank spaces at the end

UN_text <- gsub(" $", "", UN_text)



library("tm")

#create corpus

UN_tweets.text.corpus <- Corpus(VectorSource(UN_text))


#clean up by removing stop words

UN_tweets.text.corpus <- tm_map(UN_tweets.text.corpus, function(x)removeWords(x,stopwords()))


# Using the TM package to build the document term matrix and the transpose of the document term matrix.

# The document term matrix 

dtm_UN <- DocumentTermMatrix(UN_tweets.text.corpus)
dtm_UN


# The transpose of the document term matrix

tdm_UN <- TermDocumentMatrix(UN_tweets.text.corpus)
tdm_UN


# Most frequent words
(freq.terms <- findFreqTerms(tdm_UN, lowfreq = 50))

term.freq <- rowSums(as.matrix(tdm_UN))
term.freq <- subset(term.freq, term.freq >= 50)
df2 <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df2, aes(x=term, y=freq)) + geom_bar(stat="identity", fill="dark green") +xlab("Emotions") + ylab("Count") + coord_flip() + theme(axis.text=element_text(size=7))



# generate wordcloud

wordcloud(UN_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)


#getting emotions using in-built function

mysentiment_UN<-get_nrc_sentiment((UN_text))

mysentiment_UN



#calculationg total score for each sentiment

Sentimentscores_UN<-data.frame(colSums(mysentiment_UN[,]))

Sentimentscores_UN


names(Sentimentscores_UN)<-"Score"
Sentimentscores_UN<-cbind("sentiment"=rownames(Sentimentscores_UN),Sentimentscores_UN)
rownames(Sentimentscores_UN)<-NULL



#plotting the sentiments with scores

ggplot(data=Sentimentscores_UN,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on UnitedAirlines")






