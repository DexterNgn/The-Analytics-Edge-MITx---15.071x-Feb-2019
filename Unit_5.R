# Unit_5: Text Analytics

# Read the data
tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)
View(tweets)

tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

install.packages("tm")
library(tm)

install.packages("SnowballC")
library(SnowballC)

# Convert for preprocessing
corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]$content

# Change text to lower case
corpus = tm_map(corpus, tolower)
corpus[[1]]$content

# Remove Punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]$content

# Remove stopwords
stopwords("english") [1:10]
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]$content

# Stem words
corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content

# Extract the word frequency to be used in the prediction
frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005,505:515])

# Minimum of times appeared, 0.5% 
findFreqTerms(frequencies, lowfreq = 100)
sparse = removeSparseTerms(frequencies, 0.995)
sparse

# Convert the sparse matrix into a data frame
tweetsSparse = as.data.frame(as.matrix(sparse))

# To make sure we have the good colume variable name
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Add the dependent variable to the data
tweetsSparse$Negative = tweets$Negative

library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split == TRUE)
testSparse = subset(tweetsSparse, split == FALSE)

# Build a CART model
library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class")
table(testSparse$Negative, predictCART)

# Compute accuracy
(294+18)/(294+6+37+18)

# Baseline accuracy: Compare with the accuracy of the baseline model
table(testSparse$Negative)
300/(300+55)

# Random forest model
library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)

# Accuracy:
(293+21)/(293+7+34+21)

# Make predictions using the logistic regression model:
tweetLog = glm(Negative ~ ., data=trainSparse, family=binomial)
summary(QualityLog)
predictions = predict(tweetLog, newdata=testSparse, type="response")
table(testSparse$Negative, predictions > 0.5)
(251+36) / (251+36+19+49)

# Recitation 5
# Video 2
# Load the dataset
emails = read.csv("energy_bids.csv", stringsAsFactors=FALSE)
str(emails)

# Look at emails
emails$email[1]
emails$responsive[1]
emails$email[2]
emails$responsive[2]

# Responsive emails
table(emails$responsive)

# Video 3
# Load tm package
library(tm)

# Create corpus
corpus = VCorpus(VectorSource(emails$email))
corpus[[1]]$content

# Pre-process data
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

# Look at first email
corpus[[1]]$content

# Video 4
# Create matrix
dtm = DocumentTermMatrix(corpus)
dtm

# Remove sparse terms
dtm = removeSparseTerms(dtm, 0.97)
dtm

# Create data frame
labeledTerms = as.data.frame(as.matrix(dtm))

# Add in the outcome variable
labeledTerms$responsive = emails$responsive
str(labeledTerms)

# Video 5
# Split the data
library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive, 0.7)
train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)

# Build a CART model
library(rpart)
library(rpart.plot)
emailCART = rpart(responsive~., data=train, method="class")
prp(emailCART)
#(Remember that if you add the argument type="class" when making predictions, the output of predict will automatically use a threshold of 0.5.)

# Video 6
# Make predictions on the test set
pred = predict(emailCART, newdata=test)
pred[1:10,]
pred.prob = pred[,2]

# Compute accuracy
table(test$responsive, pred.prob >= 0.5)
(195+25)/(195+25+17+20)

# Baseline model accuracy
table(test$responsive)
215/(215+42)

# Video 7
# ROC curve
library(ROCR)
predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values

# Assignment 5:
# 5.1.Wikipedia

# Read the data
wiki = read.csv("wiki.csv", stringsAsFactors = FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
str(wiki)

# How many cases of vandalism were detected in the history of this page?
table(wiki$Vandal)

# Pre-process the data
library(tm)

# Create the corpus for the Added column, and call it "corpusAdded"
corpusAdded = VCorpus(VectorSource(wiki$Added))
corpusRemoved = VCorpus(VectorSource(wiki$Removed))
summary(corpusAdded)
summary(corpusRemoved)
corpusAdded[[1]]$content
corpusRemoved[[1]]$content

# Remove the English-language stopwords
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

# Stem the words
corpusAdded = tm_map(corpusAdded, stemDocument)
corpusAdded[[1]]$content
corpusRemoved = tm_map(corpusRemoved, stemDocument)
corpusRemoved[[1]]$content

# Build the DocumentTermMatrix, and call it dtmAdded.
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved


# Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions, and call the new matrix sparseAdded. How many terms appear in sparseAdded?
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved


# Convert sparseAdded to a data frame called wordsAdded, and then prepend all the words with the letter A
wordsAdded = as.data.frame(as.matrix(sparseAdded))
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
summary(corpusRemoved)

# Combine the two data frames into a data frame called wikiWords with the following line of code:
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

# Create the model
library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)
train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

# What is the accuracy on the test set of a baseline method that always predicts "not vandalism" (the most frequent outcome)?
table(test$Vandal)
618/(618+545)

# Build a CART model to predict Vandal, using all of the other variables as independent variables. Use the training set to build the model and the default parameters (don't set values for minbucket or cp).
library(rpart)
library(rpart.plot)
wikiCART = rpart(Vandal~., data=train, method="class")
prp(wikiCART)

predictCART = predict(wikiCART, newdata=test, type="class")
table(test$Vandal, predictCART)
(618+12)/(618+0+533+12)

# Plot the CART tree
prp(wikiCART)

# We hypothesize that given that a lot of vandalism seems to be adding links to promotional or irrelevant websites, the presence of a web address is a sign of vandalism.

# Create a copy of your dataframe from the previous question:
wikiWords2 = wikiWords

# Make a new column in wikiWords2 that is 1 if "http" was in Added:
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

# Based on this new column, how many revisions added a link?
table(wikiWords2$HTTP)

# make new training and testing sets:
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

# create a new CART model using this new variable as one of the independent variables.
wikiCART2 = rpart(Vandal~., data=wikiTrain2, method="class")
prp(wikiCART2)

# What is the new accuracy of the CART model on the test set, using a threshold of 0.5?
predictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")
table(test$Vandal, predictCART2)
(609+57)/(609+9+488+57)

# Sum the rows of dtmAdded and dtmRemoved and add them as new variables in your data frame wikiWords2 (called NumWordsAdded and NumWordsRemoved) by using the following commands:
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

# Create the CART model again (using the training set and the default parameters).
wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)
wikiCART3 = rpart(Vandal~., data=wikiTrain3, method="class")
prp(wikiCART3)

# What is the new accuracy of the CART model on the test set?
predictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
cmat_CART<-table(wikiTest3$vandal,predictCART3)
cmat_CART

#What is the new accuracy of the CART model on the test set, using a threshold of 0.5?
#lets now compute the overall accuracy
accu_CART <- (cmat_CART[1,1] + cmat_CART[2,2])/sum(cmat_CART)
accu_CART  #(514+248)/(514+104+297+248) = 0.6552021

# We have two pieces of "metadata" (data about data) that we haven't yet used. Make a copy of wikiWords2, and call it wikiWords3:
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)
wikiCART4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")

# Build a CART model using all the training data
predictTestCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, predictTestCART4)
(595+241)/(595+241+23+304)
prp(wikiCART4)

# 5.2. Automating Reviews in Medicine

# Read the data
trials = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
View(trials)
str(trials)
summary(trials)

# How many characters are there in the longest abstract? 
which.max(nchar(trials$abstract))
nchar(trials$abstract[[664]])

# How many search results provided no abstract? (HINT: A search result provided no abstract if the number of characters in the abstract field is zero.)
table(nchar(trials$abstract) == 0) #or 
sum(nchar(trials$abstract) == 0)

# Find the observation with the minimum number of characters in the title (the variable "title") out of all of the observations in this dataset. What is the text of the title of this article? Include capitalization and punctuation in your response, but don't include the quotes.
which.min(nchar(trials$title))
trials$title[[1258]]

# Preprocessing the data
corpusTitle = VCorpus(VectorSource(trials$title)) 
corpusAbstract = VCorpus(VectorSource(trials$abstract)) 
corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower))
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmTitle
dtmAbstract

# How many terms remain in dtmTitle after removing sparse terms (aka how many columns does it have)?
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmTitle
dtmAbstract
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

# What is the most frequent word stem across all the abstracts? Hint: you can use colSums() to compute the frequency of a word across all the abstracts.
which.max(colSums(dtmAbstract))

# We want to combine dtmTitle and dtmAbstract into a single data frame to make predictions. However, some of the variables in these data frames have the same names. To fix this issue, run the following commands:
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

# Using cbind(), combine dtmTitle and dtmAbstract into a single data frame called dtm:
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
str(dtm)

# Set the random seed to 144 and use the sample.split function from the caTools package to split dtm into data frames named "train" and "test", putting 70% of the data in the training set.
set.seed(144)
split = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, split==TRUE)
test = subset(dtm, split==FALSE)
table(test$trial)
313/(313+245)

# Build a CART model called trialCART, using all the independent variables in the training set to train the model
library(rpart)
library(rpart.plot)
trialCART = rpart(trial~., data=train, method="class")
prp(trialCART)

# What is the maximum predicted probability for any result?
predTrain= predict(trialCART)
max(predTrain[,2])
#Ans:The maximum predicted probability will likely be exactly the same in the testing set.
#EXPLANATION:Because the CART tree assigns the same predicted probability to each leaf node and there are a small number of leaf nodes compared to data points, we expect exactly the same maximum predicted probability.

# use a threshold probability of 0.5 to predict that an observation is a clinical trial.
table(train$trial,predTrain >= 0.5)

# Evaluating the model on the testing set
predictCART = predict(trialCART, newdata=test, type="class")
table(test$trial, predictCART)
predictCART
(261+162)/(261+52+83+162)
162/(162+83)
261/(261+52)

library(ROCR)
predROCR = prediction(pred.prob, test$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# 5.3. Seperating Spam from Ham

# Read the data
emails = read.csv("emails.csv", stringsAsFactors = FALSE)
View(emails)
str(emails)
summary(emails)
table(emails$spam)

# Which word appears at the beginning of every email in the dataset? Respond as a lower-case word with punctuation removed.
which.max(nchar(emails$text))
emails$text[[2651]]

# The nchar() function counts the number of characters in a piece of text. How many characters are in the longest email in the dataset (where longest is measured in terms of the maximum number of characters)?
max(nchar(emails$text))
min(nchar(emails$text))

# Which row contains the shortest email in the dataset? (Just like in the previous problem, shortest is measured in terms of the fewest number of characters.)
which(nchar(emails$text) == 13)
which.min(nchar(emails$text))

# Pre process data
library(tm)
# Create Corpus
corpus <- Corpus(VectorSource(emails$text))
# Convert to lower case
corpus <- tm_map(corpus, tolower)
# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)
# Remove Stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Stem the words
corpus <- tm_map(corpus, stemDocument)
# Create matrix
dtm <- DocumentTermMatrix(corpus)
dtm

# To obtain a more reasonable number of terms, limit dtm to contain terms appearing in at least 5% of documents, and store this result as spdtm 
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

# Build a data frame called emailsSparse from spdtm, and use the make.names function to make the variable names of emailsSparse valid.
emailsSparse = as.data.frame(as.matrix(spdtm))

#  What is the word stem that shows up most frequently across all the emails in the dataset? Hint: think about how you can use sort() or which.max() to pick out the maximum frequency.
colnames(emailsSparse) = make.names(colnames(emailsSparse))
colSums(emailsSparse)
sort(colSums(emailsSparse))
which.max(colSums(emailsSparse))

# How many word stems appear at least 5000 times in the ham emails in the dataset? Hint: in this and the next question, remember not to count the dependent variable we just added.
emailsSparse$spam = emails$spam
sum(colSums(subset(emailsSparse, emailsSparse$spam==0)) >= 5000)
sort(colSums(subset(emailsSparse, spam == 0)))

# How many word stems appear at least 1000 times in the spam emails in the dataset?
sum(colSums(subset(emailsSparse, emailsSparse$spam==1)) >= 1000) - 1
# We can limit the dataset to the spam emails with subset(emailsSparse, spam == 1). Therefore, we can read the most frequent terms with sort(colSums(subset(emailsSparse, spam == 1))). "subject", "will", and "compani" are the three stems that appear at least 1000 times. Note that the variable "spam" is the dependent variable and is not the frequency of a word stem.
sort(colSums(subset(emailsSparse, spam == 1)))

# Building machine learning models
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)
spamLog <- glm(spam ~ ., train, family=binomial)

# How many variables are labeled as significant (at the p=0.05 level) in the logistic regression summary output?
summary(spamLog)


# How many of the training set predicted probabilities from spamLog are less than 0.00001?
predLog <- predict(spamLog, type = "response")
sum(predLog < 1e-05)
sum(predLog > 0.99999)
sum(predLog > 1e-05 & predLog < 0.99999)

# Accuracy
tLog <- table(train$spam, predLog >= 0.5)
(tLog[1, 1] + tLog[2, 2])/sum(tLog)

# How many of the word stems "enron", "hou", "vinc", and "kaminski" appear in the CART tree? Recall that we suspect these word stems are specific to Vincent Kaminski and might affect the generalizability of a spam filter built with his ham data.
library(rpart)
library(rpart.plot)
spamCART = rpart(spam~., data=train, method="class")
prp(spamCART)

# Predict using the trainig set.
predTrain <- predict(spamCART)[, 2]
# Accuracy on the training set
tCART <- table(train$spam, predTrain >= 0.5)
(tCART[1, 1] + tCART[2, 2])/(sum(tCART))

# What is the training set accuracy of spamRF, using a threshold of 0.5 for predictions?
library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~ ., data=train)

#Get predicted spam probabilities for the training set for each model:
predTrainLog <- predict(spamLog, type="response")
predTrainCART<- predict(spamCART)[,2]
predTrainRF<- predict(spamRF, type="prob")[,2]

#First we compute the confusion matrix
cmat_log<-table(train$spam, predTrainLog > 0.5)
cmat_log
##     FALSE TRUE
##   0  3052    0
##   1     4  954
#lets now compute the overall accuracy
accu_log <- (cmat_log[1,1] + cmat_log[2,2])/sum(cmat_log)
accu_log #(3052+954)/nrow(train) = 0.9990025
or
table(train$spam, predTrainLog > 0.5)

library(ROCR)
predictionTrainLog = prediction(predTrainLog, train$spam)
perf <- performance(predictionTrainLog, "tpr", "fpr")
as.numeric(performance(predictionTrainLog, "auc")@y.values)

cmat_CART<-table(train$spam, predTrainCART > 0.5)
cmat_CART

library(ROCR)
predictionTrainCART = prediction(predTrainCART, train$spam)
perf <- performance(predictionTrainCART, "tpr", "fpr")
as.numeric(performance(predictionTrainCART, "auc")@y.values)

# What is the training set accuracy of spamRF, using a threshold of 0.5 for predictions?
predictRF = predict(spamRF, newdata=test)
table(test$spam, predictRF)

# What is the training set AUC of spamRF?
library(ROCR)
predTrainRF = prediction(predTrainRF, train$spam)
perf <- performance(predTrainRF, "tpr", "fpr")
as.numeric(performance(predTrainRF, "auc")@y.values)

predTestLog<- predict(spamLog, newdata=test, type="response")
predTestCART <- predict(spamCART, newdata=test)[,2]
predTestRF <- predict(spamRF, newdata=test, type="prob")[,2]

# Obtain predicted probabilities for the testing set for each of the models, again ensuring that probabilities instead of classes are obtained.
# What is the testing set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(test$spam, predTestLog > 0.5)
(1257+376)/(1257+376+51+34)

# What is the testing set AUC of spamLog?
predTestLog = prediction(predTestLog, test$spam)
perf <- performance(predTestLog, "tpr", "fpr")
as.numeric(performance(predTestLog, "auc")@y.values)

# What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?
cmat_CART<-table(test$spam, predTestCART > 0.5)
cmat_CART
(1228+386)/(1228+386+80+24)

# What is the testing set AUC of spamCART?
library(ROCR)
predTestCART = prediction(predTestCART, test$spam)
perf <- performance(predTestCART, "tpr", "fpr")
as.numeric(performance(predTestCART, "auc")@y.values)

# What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
predictRF = predict(spamRF, newdata=test)
table(test$spam, predictRF)
(1291+385)/(1291+385+25+17)

# What is the testing set AUC of spamRF?
library(ROCR)
predTestRF = prediction(predTestRF, test$spam)
perf <- performance(predTestRF, "tpr", "fpr")
as.numeric(performance(predTestRF, "auc")@y.values)
