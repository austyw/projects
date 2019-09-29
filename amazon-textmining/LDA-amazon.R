amazon=read.csv('amazon.csv')
library(tm)
library(topicmodels)
#there are 3 products (ASIN)
#B00DVFLJKQ = nexus 7 tablet $160
#B01EUC7NPI = samsung galaxy tablet $180
#B07BTS2KWK = apple ipad $330
library(stringr)
#only look at nexus 7 reviews
amazon=subset(amazon,ASIN=='B07BTS2KWK')
amazon=subset(amazon,Rating<3)
#see how many low reviews have "screen" or "charge" in them
#ipad related to "hours" "keyboard" "battery"
test=subset(amazon,str_detect(amazon$ReviewText,"box"))
docs=Corpus(VectorSource(amazon$ReviewText))
#inspect a particular document in corpus
writeLines(as.character(docs[[30]]))
#start preprocessing
#Transform to lower case
docs <-tm_map(docs,content_transformer(tolower))


#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, ' ', x))})
docs <- tm_map(docs, toSpace, '-')
docs <- tm_map(docs, toSpace, ''')
docs <- tm_map(docs, toSpace, ''')
docs <- tm_map(docs, toSpace, '.')
docs <- tm_map(docs, toSpace, '"')
docs <- tm_map(docs, toSpace, '"')


#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords('english'))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Good practice to check every now and then
writeLines(as.character(docs[[30]]))
#Stem document
#docs <- tm_map(docs,stemDocument)



#define and eliminate all custom stopwords
myStopwords <- c('can', 'say','one','way','use',
                 'also','howev','tell','will',
                 'much','need','take','tend','even',
                 'like','particular','rather','said',
                 'get','well','make','ask','come','end','tablet','device')
docs <- tm_map(docs, removeWords, myStopwords)
#inspect a document as a check
writeLines(as.character(docs[[30]]))

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#convert rownames to filenames
#rownames(dtm) <- filenames
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]
write.csv(freq[ord],'word_freq.csv')
library(wordcloud)
wordcloud(docs, min.freq = 10, max.words=300,random.order = FALSE,
          colors=brewer.pal(8, "Dark2"))

#clean dtm of 0 rows
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm   <- dtm[rowTotals> 0, ]   
#create LDA 
text_lda=LDA(dtm,k=2,control=list(seed=1234))
text_lda
library(tidytext)
text_topics=tidytext::tidy(text_lda,matrix='beta')
text_topics
library(ggplot2)
library(dplyr)
text_top_terms=text_topics%>%group_by(topic)%>%top_n(20,beta)%>%ungroup()%>%
  arrange(topic,-beta)
text_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
#beta spread
library(tidyr)

beta_spread <- text_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

percent_with_screen=list(160/414,105/399,13/89)
percent_with_charge=list(81/414,43/399,10/89)

