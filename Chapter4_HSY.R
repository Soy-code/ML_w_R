setwd("E:\\BITAmin\\Machine Learning with R, Second Edition_Code\\Chapter 04")

## Exploring and preparing the data 
options(stringsAsFactors=F)
sms<-readLines(file('sms_spam.csv',encoding='UTF-8'))
sms<-strsplit(sms,'\\"')
sms_s<-data.frame()
for (i in 2:length(sms)) {
  sms_s[i-1,1]<-sms[[i]][2]
  sms_s[i-1,2]<-sms[[i]][4] }
colnames(sms_s)<-c('type','text')
sms_raw<-sms_s
str(sms_raw)

sms_raw$type = factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

## Data preparation - cleaning and standardizing test data
library(tm)
sms_corpus = VCorpus(VectorSource(sms_raw$text)) 
lapply(sms_corpus[1:2], as.character)
sms_corpus_clean = tm_map(sms_corpus, content_transformer(tolower))
sms_corpus_clean = tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean = tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean = tm_map(sms_corpus_clean, removePunctuation)

library(SnowballC)
sms_corpus_clean = tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean = tm_map(sms_corpus_clean, stripWhitespace)

as.character(sms_corpus[1:3]) 
as.character(sms_corpus_clean[1:3])

## Data preparation - splitting text documents into words
sms_dtm = DocumentTermMatrix(sms_corpus_clean)
sms_dtm2 = DocumentTermMatrix(sms_corpus, 
                              control = list(tolewer = T, 
                                             removeNumbers = T, 
                                             stopwords = T, 
                                             removePunctuation = T, 
                                             stemming = T))
sms_dtm
sms_dtm2  

## Data preparation - creating training and test datasets
sms_dtm_train = sms_dtm[1:4169, ]
sms_dtm_test = sms_dtm[4170:5574, ]
sms_train_labels = sms_raw[1:4169, ]$type
sms_test_labels = sms_raw[4170:5574, ]$type
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

## Visualizing text data - word clouds
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = F)

spam = subset(sms_raw, type == 'spam')
ham = subset(sms_raw, type == 'ham')
wordcloud(spam$text, max.words = 40, scale=c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale=c(3, 0.5))

## Data preparation - creating indicator features for frequent words
sms_freq_words = findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)
sms_dtm_freq_train = sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test = sms_dtm_test[ , sms_freq_words]
convert_counts = function(x) {
  x = ifelse(x>0, "Yes", "No")
}
sms_train = apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test = apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)       

## Training a model on the data
library(e1071)
sms_classifier = naiveBayes(sms_train, sms_train_labels)

## evaluating model performance
sms_test_pred = predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels, prop.chisq = F, prop.t=F,
           dnn=c("predicted", "actual"))

## improving model performance
sms_classifier2 = naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 = predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels, prop.chisq=F, prop.t=F, 
           dnn=c("predicted", "actual"))
