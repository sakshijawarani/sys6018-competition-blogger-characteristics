library(readr)
library(tidyverse)
library(stringr)
library(dummies)
library(XML)
library(tm)
library(zoo)
library(textfeatures)
library(randomForest)

###Data exploration
train<- read_csv(file = "train.csv")
test<-read_csv(file="test.csv")
test$age <- NA
combined <- bind_rows(train, test)

###Data cleaning
combined$gender <- factor(combined$gender)
combined$topic <- factor(combined$topic)
signs <- c("Aries","Leo", "Sagittarius","Taurus", "Virgo", "Capricorn","Gemini", "Libra", "Aquarius","Cancer", "Scorpio", "Pisces")
combined$sign[combined$sign != signs] <- "Other"
combined$sign <- factor(combined$sign)
combined$date <- as.Date(combined$date, format = "%d,%B,%Y")
combined$date <- na.locf(combined$date)
summary(combined$sign)

#Text Featuring
textfeatures <- textfeatures(combined[,"text"])
combinedwfeatures <- cbind(combined, textfeatures)
combinedwfeatures[is.na(combinedwfeatures)] <- 0
combinedwfeatures <- combinedwfeatures %>% mutate(year = str_sub(date,1,4),
                             month = str_sub(date,6,7),
                             day = str_sub(date,9,10))
combinedwfeatures$year <- as.numeric(combinedwfeatures$year)
combinedwfeatures$month <- as.numeric(combinedwfeatures$month)
combinedwfeatures$day <- as.numeric(combinedwfeatures$day)
write.table(combinedwfeatures, file = "combinedwfeatures.csv", row.names=FALSE, sep=",")


text_data <- combined[,c(1,7)]
head(text_data)

document.data.frame = as.data.frame(text_data, stringsAsFactors = FALSE)
names(document.data.frame) = c("doc_id","text")
head(document.data.frame)
blogs = VCorpus(DataframeSource(document.data.frame))

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

tfidf.80 = removeSparseTerms(blogs.clean.tfidf, 0.80)  # remove terms that are absent from at least 80% of documents
tfidf.80
tfidf.80<-as.matrix(tfidf.80)
dim(tfidf.80)

word_feat <- tfidf.80
nrow(word_feat)


train_set<-combinedwfeatures[1:442961,]
test_set<-combinedwfeatures[442962:681284,]

#train
data_for_model <-cbind(train_set[,-7],word_feat[1:442961,])
data_for_model_byuser <- data_for_model %>% group_by(user.id)
write.table(data_for_model_byuser, file = "data_for_model_byuser.csv", row.names=FALSE, sep=",")
head(data_for_model_byuser)
data_for_model_byuser[is.na(data_for_model_byuser)] <- 0

###Cross Validation
data_for_model_byuser_train <- data_for_model_byuser[1:354000, ]
data_for_model_byuser_test  <- data_for_model_byuser[354001:1442961, ]
ex_model <- lm(age~.-post.id-user.id,data=data_for_model_byuser_train) 
p <- predict(ex_model, data_for_model_byuser_test)
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

RMSE(p, data_for_model_byuser_test$age)

###Linear model
##Rationale for the selected statistical modeling methods:
#Data was very numeric, linear model was also a requirement
ex_model <- lm(age~.-post.id-user.id,data=data_for_model_byuser) 
summary(ex_model)


#test
data_for_test <-cbind(test_set[,-7],word_feat[442962:681284,])
data_for_test_byuser <- data_for_test %>% group_by(user.id)
write.table(data_for_test_byuser, file = "data_for_test_byuser.csv", row.names=FALSE, sep=",")
head(data_for_test_byuser)
data_for_test_byuser[is.na(data_for_test_byuser)] <- 0

#linear prediction
p <- predict(ex_model, data_for_test_byuser)
predictions <- data.frame("user.id" = data_for_test_byuser$user.id, "age" = p)
uniqueages <- aggregate(.~user.id, data=predictions, mean)
write.table(uniqueages,file = "predictions.csv", row.names=FALSE, col.names=c("user.id", "age"), sep=",")
