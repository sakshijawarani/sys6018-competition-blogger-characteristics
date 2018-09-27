#library reference for reading and writing files
library(readr)
#library reference for data handling
library(dplyr)
library(stringr)

library(dummies)

train <- read.csv("all/train.csv", stringsAsFactors = FALSE)
test <- read.csv("all/test.csv", stringsAsFactors = FALSE)

train$gender <- factor(train$gender)
train$topic <- factor(train$topic)
train$sign <- factor(train$sign)

train$textlength <- nchar(train$text)
train$wordcount <- length(strsplit(test$text, " "))
#train$doubledot <- str_count(train$text, "..")

model <- lm(age ~ topic+textlength+sign+wordcount, data=train)
summary(model)

y.hat <- predict(model, newdata=train)

mae <- mean(abs(y.hat - train$age))
mae

test$gender <- factor(test$gender)
test$topic <- factor(test$topic)
test$sign <- factor(test$sign)

test$textlength <- nchar(test$text)
test$wordcount <- length(strsplit(test$text, " "))
#test$doubledot <- str_count(test$text, "..")

test.user.ids <- unique(test$user.id)

test.age <- predict(model, newdata = test)

test.predict <- data.frame(matrix(nrow=nrow(test), ncol=2))
names(test.predict) <- c('user.id','age')
test.predict$user.id <- test$user.id
test.predict$age <- test.age

test.unique <- data.frame(matrix(nrow=length(test.user.ids), ncol=2))
names(test.unique) <- c('user.id','age')

test.unique <- aggregate(test.predict[, 2], list(test.predict$user.id), mean) %>% as.data.frame()

write.table(test.unique, file = "test_blogger_age_predictions.csv", row.names=F, col.names=c('user.id', 'age'), sep=",")

