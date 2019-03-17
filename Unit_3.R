#Video 4
#Logistic Regression in R

#Read the data
quality = read.csv("quality.csv")
str(quality)

table(quality$PoorCare)

#Baseline model
98/133

#Install package caTools to split the data into training and testing datasets
install.packages("caTools")
library(caTools)

#Set our seed to make sure that we will get the same split
set.seed(88)

#Make sure that 75% in the training dataset are receiving the good care
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split

#Create training set
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
nrow(qualityTrain)

#Create the model; using the family = binomal to tell the glm function to build the logistic regression
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data =qualityTrain, family = binomial)

#Check the model, the minimum AIC shows that the model is good
summary(QualityLog)

#Make the predictions on the training set, using the type function to tell the predict function give us probabilities
predictTrain = predict(QualityLog, type = "response")
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)

#Build the model using different independent variables
QualityLog1 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data =qualityTrain, family = binomial)
summary(QualityLog1)

#Threshold Value
table(qualityTrain$PoorCare, predictTrain > 0.5)

#Generate the ROC curve in R
install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr","fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))

#Interpreting the model // How to assess the strength of the model
#Compute the test set predictions of the original model
predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

#Analytics to prevent Heart Disease
# Read in the dataset
framingham = read.csv("framingham.csv")

# Look at structure
str(framingham)

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets, around 50-80% for the training data
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

# Split up the data using subset
train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE)

# Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)

# Predictions on the test set
predictTest = predict(framinghamLog, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)

# Accuracy
(1069+11)/(1069+6+187+11)

# Baseline accuracy
(1069+6)/(1069+6+187+11) 

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Unit 3, Recitation
# Video 2
# Read in data
polling = read.csv("PollingData.csv")
str(polling)
View(polling)
table(polling$Year)
summary(polling)

# Install and load mice package to solve the missing data
install.packages("mice")
library(mice)

# Multiple imputation
simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling)

# Video 3
# Subset data into training set and test set
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)

# Smart Baseline
table(Train$Republican)
sign(20)
sign(-10)
sign(0)
table(sign(Train$Rasmussen))
table(Train$Republican, sign(Train$Rasmussen))

# Video 4
# Multicollinearity
cor(Train)
str(Train)
cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

# Logistic Regression Model
mod1 = glm(Republican~PropR, data=Train, family="binomial")
summary(mod1)

# Training set predictions
pred1 = predict(mod1, type="response")
table(Train$Republican, pred1 >= 0.5)

# Two-variable model, use two variables with the least correlation because we can work together to predict the outcome
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
pred2 = predict(mod2, type="response")
table(Train$Republican, pred2 >= 0.5)
summary(mod2)

# Video 5
# Smart baseline accuracy
table(Test$Republican, sign(Test$Rasmussen))

# Test set predictions
TestPrediction = predict(mod2, newdata=Test, type="response")
table(Test$Republican, TestPrediction >= 0.5)

# Analyze mistake
subset(Test, TestPrediction >= 0.5 & Republican == 0)

#Assignment_3
#Assignment_3.1 Popularity of music

#read the data
songs = read.csv("songs.csv")
View(songs)
str(songs)

#Check the years of songs
table(songs$year)

#Check the artist of songs
table(songs$artistname)
table(songs$artistname == "Michael Jackson")
MJ = subset(songs, artistname == "Michael Jackson")
MJ[c("songtitle", "Top10")]

#What are the values of this timesignature variable that occur in our dataset?
table(songs$timesignature)

#Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. Which one is it?
which.max(songs$tempo)
songs$songtitle[[6206]]

#Create the model
#Use the subset function to split the data
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)
summary(SongsTrain)

#How many observations?
str(SongsTrain)

#Remove variables which will not be used in the prediction model
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

#Create the logistic regression model
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

#Beware of Multicollinearity Issues!
cor(SongsTrain)
cor(SongsTrain$loudness, SongsTrain$energy)

#Create Model 2, which is Model 1 without the independent variable "loudness"
#We just subtracted the variable loudness. We couldn't do this with the variables "songtitle" and "artistname", because they are not numeric variables, and we might get different values in the test set that the training set has never seen. But this approach (subtracting the variable from the model formula) will always work when you want to remove numeric variables.
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

#create Model 3, which should be exactly like Model 1, but without the variable "energy"
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

#Make predictions on the test set using Model 3
TestPrediction = predict(SongsLog3, newdata=SongsTest, type="response")

#using a threshold of 0.45
table(SongsTest$Top10, TestPrediction >= 0.45)

#What is the accuracy of Model 3 on the test set
(309+19)/(309+5+40+19)

#What would the accuracy of the baseline model be on the test set?
table(SongsTest$Top10)
314/(314+59)

#Calculate the sensitivity and specifility
19/59
309/(309+5)

#Assignment_3.2 Predicting Parole Violators

# Load the data
parole = read.csv("parole.csv")

# Count the number of parolees
nrow(parole)

# Count the number of violators
table(parole$violator)

# Output the structure
str(parole)

# Convert to Factor
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

# Split the data
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# Logistic Regression
mod = glm(violator~., data=train, family="binomial")

# Output the summary
summary(mod)

# Calculate log odds
male=1 
race=1
age=50
state2=0
state3=0
state4=0
time.served=3
max.sentence=12
multiple.offenses=0
crime2=1
crime3=0
crime4=0
logodds =-4.2411574 + 0.3869904*male + 0.8867192*race - 0.0001756*age + 0.4433007*state2 + 0.8349797*state3 - 3.3967878*state4 - 0.1238867*time.served + 0.0802954*max.sentence + 1.6119919*multiple.offenses + 0.6837143*crime2 - 0.2781054*crime3 - 0.0117627*crime4
logodds
odds = exp(logodds)
odds

# Calculate Probability
1/(1 + exp(-logodds))

# Make Predictions
predictions = predict(mod, newdata=test, type="response")

# Output the summary
summary(predictions)

# Model Predictions with threshold of 0.5
table(test$violator, as.numeric(predictions >= 0.5))

# Tabulate the baseline
table(test$violator)

# Calculate AUC
library(ROCR)
pred = prediction(predictions, test$violator)
as.numeric(performance(pred, "auc")@y.values)

#Assignment_3.3 Predicting Loan Repayment

# Load the data
loans = read.csv("loans.csv")
str(loans)
summary(loans)

# Tabulate not fully paid
table(loans$not.fully.paid)

# Fill the missing data
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

# Split the data
library(caTools)
set.seed(144)
spl = sample.split(loans$not.fully.paid, 0.7)
train = subset(loans, spl == TRUE)
test = subset(loans, spl == FALSE)

# Logistic Regression
mod = glm(not.fully.paid~., data=train, family="binomial")
summary(mod)

# Make predictions
test$predicted.risk = predict(mod, newdata=test, type="response")
# Tabulate not fully with threshold
table(test$not.fully.paid, test$predicted.risk > 0.5)

# The accuracy of the baseline model
table(test$not.fully.paid)

# Calculate AUC
library(ROCR)
pred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)

# Logistic Regression
bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)
cor(train$int.rate,train$fico)

# Make predictions
pred.bivariate = predict(bivariate, newdata=test, type="response")
# Max Probability
summary(pred.bivariate)

# Calculate AUC
prediction.bivariate = prediction(pred.bivariate, test$not.fully.paid)
as.numeric(performance(prediction.bivariate, "auc")@y.values)

# Profit of an investment
c = 10
r = 0.06
t = 3
c*exp(r*t)

# Create a new variable
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

# Maximum profit
summary(test$profit)

# Subset the data
highInterest = subset(test, int.rate >= 0.15)

# Find the average
mean(highInterest$profit)

# Tabulate high interest loans not fully paid
table(highInterest$not.fully.paid)

# Implement cutoff
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

# Subset the data
selectedLoans = subset(highInterest, predicted.risk <= cutoff)

# Calculate the profit
sum(selectedLoans$profit)

# Tabulate how many selected loans were not paid back in full
table(selectedLoans$not.fully.paid)
