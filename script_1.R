library(tidyverse)
library(stringr)
library(dummies)
library(XML)
library(tm)

setwd("~/Documents/Fallsem/SYS/kagglecomp3/all-4")
train<- read_csv(file = "train.csv")
test<-read_csv(file="test.csv")

test$age <- NA
combined <- bind_rows(train, test)


combined$gender <- factor(combined$gender)
combined$topic <- factor(combined$topic)
combined$sign <- factor(combined$sign)

# 
# combined$lower_string <- tolower(combined$text)
# combined$temp <- stringr::str_replace_all(combined$lower_string,"[^a-zA-Z\\s]", " ")
# combined$temp <- stringr::str_replace_all(combined$temp,"[\\s]+", " ")
# combined$temp <- stringr::str_split(combined$temp, " ")
# head(combined$temp)

# combined$num_tok <- length(combined$temp)
# head(combined$num_tok)



# combined$num_uniq<-unlist(lapply(combined$temp, function(x) length(unique(x))))
# head(combined$num_uniq)


text_data <- combined[,c(1,7)]
head(text_data)

document.data.frame = as.data.frame(text_data, stringsAsFactors = FALSE)
names(document.data.frame) = c("doc_id","text")
head(document.data.frame)
blogs = VCorpus(DataframeSource(document.data.frame))


#blogs.tfidf = DocumentTermMatrix(blogs, control = list(weighting = weightTfIdf))

#blogs.tfidf[1:5,1:5]
#as.matrix(blogs.tfidf[1:5,1:5])

#combined %>% filter(user.id == 14423)



blogs.clean = tm_map(blogs, stripWhitespace)                          # remove extra whitespace
blogs.clean = tm_map(blogs.clean, removeNumbers)                      # remove numbers
blogs.clean = tm_map(blogs.clean, removePunctuation)                  # remove punctuation
blogs.clean = tm_map(blogs.clean, content_transformer(tolower))       # ignore case
blogs.clean = tm_map(blogs.clean, removeWords, stopwords("english"))  # remove stop words
blogs.clean = tm_map(blogs.clean, stemDocument)                       # stem all words


blogs[[1]]$content
blogs.clean[[1]]$content  




# recompute TF-IDF matrix using the cleaned corpus
blogs.clean.tfidf = DocumentTermMatrix(blogs.clean, control = list(weighting = weightTfIdf))

# reinspect the first 5 documents and first 5 terms
blogs.clean.tfidf[1:10,1:10]
as.matrix(blogs.clean.tfidf[1:10,1:10])





# we've still got a very sparse document-term matrix. remove sparse terms at various thresholds.
tfidf.99 = removeSparseTerms(blogs.clean.tfidf, 0.99)  # remove terms that are absent from at least 99% of documents (keep most terms)
tfidf.99
as.matrix(tfidf.99[1:5,1:5])

tfidf.80 = removeSparseTerms(blogs.clean.tfidf, 0.80)  # remove terms that are absent from at least 70% of documents
tfidf.80
tfidf.80<-as.matrix(tfidf.80)
#news.clean[[1]]$content

#as.matrix(tfidf.70)
word_feat <- tfidf.80
nrow(word_feat)

train_set<-combined[1:442961,]
test_set<-combined[1:238323,]
data_for_model <-cbind(train_set[,-7],word_feat[1:442961,])
data_for_model.recs <- sample(1:442961, size = 442961/2)

data_for_model.temp <- data_for_model[data_for_model.recs,] 

head(data_for_model.temp)

(l <- sapply(data_for_model, function(x) is.factor(x)))
m <- data_for_model[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")

# data_for_model.temp$Docs <- as.numeric(data_for_model.temp$Docs)
# data_for_model.temp$Terms <- as.character(data_for_model.temp$Terms)
# data
ex_model <- lm(age~.-post.id-user.id-date, data=train_set) 
summary(ex_model)


