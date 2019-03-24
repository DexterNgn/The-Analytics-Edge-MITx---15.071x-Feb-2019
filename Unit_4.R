# Unit 4: Decision Tree Analysis - Classification

# The Supreme Court
# VIDEO 4
# Read in the data
stevens = read.csv("stevens.csv")
str(stevens)

# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)

# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# CART model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=100)

# Plot the model
prp(StevensTree)

# Make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71)

# ROC curve -> Evaluate the model
library(ROCR)
PredictROC = predict(StevensTree, newdata = Test)
PredictROC
pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

# Compute the AUC of the CART model
as.numeric(performance(pred, "auc")@y.values)

# VIDEO 5 - Random Forests
# Install randomForest package
install.packages("randomForest")
library(randomForest)

#Set the seed
set.seed(200)

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

# Try again
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(43+75)/(43+34+18+75)

# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp = 0.18)

# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)

# Plot the model
prp(StevensTreeCV)

# The story of D2HawKeye
# Read in the data
Claims = read.csv("ClaimsData.csv")
str(Claims)

# Percentage of patients in each cost bucket
table(Claims$bucket2009)/nrow(Claims)

# Split the data
library(caTools)
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain = subset(Claims, spl==TRUE)
ClaimsTest = subset(Claims, spl==FALSE)

#What proportion of people in the training set (ClaimsTrain) had at least one diagnosis code for diabetes?
summary(ClaimsTrain$age)
table(ClaimsTrain$diabetes)/nrow(ClaimsTrain)

# VIDEO 7
# Baseline method
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
(110138 + 10721 + 2774 + 1539 + 104)/nrow(ClaimsTest)

# Penalty Matrix
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)

PenaltyMatrix

# Penalty Error of Baseline Method
as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix

sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)

#To compute the accuracy, you can create a table of the variable ClaimsTest$bucket2009:
table(ClaimsTest$bucket2009)

#According to the table output, this baseline method would get 122978 observations correct, and all other observations wrong. So the accuracy of this baseline method is 122978/nrow(ClaimsTest) = 0.67127.
#For the penalty error, since this baseline method predicts 1 for all observations, it would have a penalty error of:
(0*122978 + 2*34840 + 4*16390 + 6*7937 + 8*1057)/nrow(ClaimsTest)

# VIDEO 8
# Load necessary libraries
library(rpart)
library(rpart.plot)

# CART model
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005)

prp(ClaimsTree)

# Make predictions
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest)

(114141 + 16102 + 118 + 201 + 0)/nrow(ClaimsTest)

# Penalty Error
as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix

sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)

# New CART model with loss matrix
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005, parms=list(loss=PenaltyMatrix))

# Redo predictions and penalty error
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest)

(94310 + 18942 + 4692 + 636 + 2)/nrow(ClaimsTest)

sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)

# Recitation 4
# Read in data
boston = read.csv("boston.csv")
# Output structure
str(boston)

plot(boston$LON, boston$LAT)

# Tracts alongside the Charles River
points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1], col="blue", pch=19)

# Plot MIT
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],col="red", pch=20)

# Plot polution
summary(boston$NOX)
points(boston$LON[boston$NOX>=0.55], boston$LAT[boston$NOX>=0.55], col="green", pch=20)

# Plot prices
plot(boston$LON, boston$LAT)
summary(boston$MEDV)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)

# Linear Regression using LAT and LON
plot(boston$LAT, boston$MEDV)

plot(boston$LON, boston$MEDV)

latlonlm = lm(MEDV ~ LAT + LON, data=boston)
summary(latlonlm)

# Visualize regression output
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)

latlonlm$fitted.values

# Load CART packages
library(rpart)
library(rpart.plot)

# CART model
latlontree = rpart(MEDV ~ LAT + LON, data=boston)
prp(latlontree)

# Visualize output
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)

fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues>21.2], boston$LAT[fittedvalues>=21.2], col="blue", pch="$")

# Simplify tree by increasing minbucket
latlontree = rpart(MEDV ~ LAT + LON, data=boston, minbucket=50)
plot(latlontree)
text(latlontree)

# Visualize Output
plot(boston$LON,boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)

# Split the data
library(caTools)
set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split==TRUE)
test = subset(boston, split==FALSE)

# Create linear regression
linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
summary(linreg)

# Make predictions
linreg.pred = predict(linreg, newdata=test)
linreg.sse = sum((linreg.pred - test$MEDV)^2)
linreg.sse

# Create a CART model
tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
prp(tree)
    
# Make predictions
tree.pred = predict(tree, newdata=test)
tree.sse = sum((tree.pred - test$MEDV)^2)
tree.sse
    
# Load libraries for cross-validation
library(caret)
library(e1071)

# Number of folds
tr.control = trainControl(method = "cv", number = 10)

# cp values
cp.grid = expand.grid( .cp = (0:10)*0.001)

# What did we just do?

# Cross-validation
tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

# Extract tree
best.tree = tr$finalModel
prp(best.tree)

# Make predictions
best.tree.pred = predict(best.tree, newdata=test)
best.tree.sse = sum((best.tree.pred - test$MEDV)^2)
best.tree.sse

#Assignment 4.1. Understand why people vote

gerber <- read.csv("gerber.csv")
str(gerber)

table(gerber$voting)

tapply(gerber$voting, gerber$civicduty, mean)

tapply(gerber$voting, gerber$hawthorne, mean)

tapply(gerber$voting, gerber$neighbors, mean)

tapply(gerber$voting, gerber$self, mean)

# Build a logistic regression model for voting using the four treatment group variables as the independent variables
logReg <- glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = binomial)
summary(logReg)

# Using a threshold of 0.3, what is the accuracy of the logistic regression model?
predict_logReg <- predict(logReg, type = "response")
table(gerber$voting, predict_logReg >= 0.3)
(134513+51966)/(134513+51966+100875+56730)

# Using a threshold of 0.5, what is the accuracy of the logistic regression model?
table(gerber$voting, predict_logReg >= 0.5)
235388/nrow(gerber)

# Compare your previous two answers to the percentage of people who did not vote (the baseline accuracy) and compute the AUC of the model. What is happening here?
library(ROCR)
ROCRpred = prediction(predictLog, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Build a Tree model
library(rpart)
library(rpart.plot)
CARTmodel <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)

# Plot the tree
prp(CARTmodel)

# introducing cp to force the complete tree to be built
CARTmodel2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, cp = 0.0)
# Plot the tree
prp(CARTmodel2)

# introducing a new variable sex
CARTmodel3 <- rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data = gerber, cp = 0.0)
# Plot the tree
prp(CARTmodel3)

# focus on the “Control” treatment group. Create a regression tree using just the “control” variable, then create another tree with the “control” and “sex” variables, both with cp=0.0.
controlReg <- rpart(voting ~ control, data = gerber, cp = 0.0)
controlSexReg <- rpart(voting ~ control + sex, data = gerber, cp = 0.0)
prp(controlReg, digits=6)
prp(controlSexReg, digits = 6)

# Absolute value
0.34 - 0.296638

# Going back to logistic regression now, create a model using “sex” and “control”
ControlSex_logReg <- glm(voting ~ sex + control, data = gerber, family = binomial)
summary(ControlSex_logReg)
# This means that women are less likely to vote, since women have a larger value in the sex variable, and a negative coefficient means that larger values are predictive of 0.

Possibilities <- data.frame(sex = c(0,0,1,1), control = c(0,1,0,1))
# Difference
0.2908065 - 0.290456
# The CART tree predicts 0.290456 for the (Woman, Control) case, and the logistic regression model predicts 0.2908065. So the absolute difference, to five decimal places, is 0.00035.

# evaluate your logistic regression using the predict function
predict(ControlSex_logReg, newdata = Possibilities, type = "response")

ControlSex_logReg2 <- glm(voting ~ sex + control + sex:control, data = gerber, family = binomial)
summary(ControlSex_logReg2)

# Run the same code as before to calculate the average for each group
predict(ControlSex_logReg2, newdata = Possibilities, type = "response")

# Diff
0.2904558 - 0.290456

# Assignment 4.2. Letter Recognization
#Lets load the dataset
letters<-read.csv("letters_ABPR.csv")
str(letters)

#Let's warm up by attempting to predict just whether a letter is B or not. To begin, load the file letters_ABPR.csv into R, and call it letters. Then, create a new variable isB in the dataframe, which takes the value "TRUE" if the observation corresponds to the letter B, and "FALSE" if it does not. You can do this by typing the following command into your R console:
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000) # to get the same split everytime
spl = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, spl==TRUE)
test= subset(letters, spl==FALSE)

table(train$isB) #The output of table(train$isB) tells us that "not B" is more common

#Before building models, let's consider a baseline method that always predicts the most frequent outcome, which is "not B". What is the accuracy of this baseline method on the test set?
table(test$isB)

1175/(1175+383)

#Now build a classification tree to predict whether a letter is a B or not, using the training set to build your model. Remember to remove the variable "letter" out of the model, as this is related to what we are trying to predict! To just remove one variable, you can either write out the other variables, or remember what we did in the Billboards problem in Week 3, and use the following notation:

CARTb = rpart(isB ~ . - letter, data=train, method="class")

#What is the accuracy of the CART model on the test set? (Use type="class" when making predictions on the test set.)

#The model can be be represented as a decision tree:
prp(CARTb) #plotting the classification tree

# Make predictions
predictions = predict(CARTb, newdata = test, type = "class") #We need to give type = "class" if we want the majority class predictions.This is like using a threshold of 0.5.

#Now lets assess the accuracy of the model through confusion matrix
cmat_CART<-table(test$isB, predictions)  #first arg is thr true outcomes and the second is the predicted outcomes
cmat_CART

#lets now compute the overall accuracy

accu_CART <- (cmat_CART[1,1] + cmat_CART[2,2])/sum(cmat_CART)
accu_CART  #(1118+340)/(1118+57+43+340)

#Now, build a random forest model to predict whether the letter is a B or not (the isB variable) using the training set. You should use all of the other variables as independent variables, except letter (since it helped us define what we are trying to predict!). Use the default settings for ntree and nodesize (don't include these arguments at all). Right before building the model, set the seed to 1000. (NOTE: You might get a slightly different answer on this problem, even if you set the random seed. This has to do with your operating system and the implementation of the random forest algorithm.)

library(randomForest)
set.seed(1000)

# Build random forest model
RFb = randomForest(isB ~ . - letter, data = train) 
#or RFb = randomForest(isB ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor, data=train)

#What is the accuracy of the model on the test set?
predictions = predict(RFb, newdata=test)
#Now lets assess the accuracy of the model through confusion matrix
cmat_forest<-table(test$isB, predictions)  #first arg is thr true outcomes and the second is the predicted outcomes
cmat_forest

#lets now compute the overall accuracy
accu_forest <- (cmat_forest[1,1] + cmat_forest[2,2])/sum(cmat_forest)
accu_forest  #or (1165+374)/(1165+10+9+374) 

#PROBLEM 2.1 - PREDICTING THE LETTERS A, B, P, R  
#Let us now move on to the problem that we were originally interested in, which is to predict whether or not a letter is one of the four letters A, B, P or R.

#As we saw in the D2Hawkeye lecture, building a multiclass classification CART model in R is no harder than building the models for binary classification problems. Fortunately, building a random forest model is just as easy.

#The variable in our data frame which we will be trying to predict is "letter". Start by converting letter in the original data set (letters) to a factor by running the following command in R:

letters$letter = as.factor( letters$letter )

#Now, generate new training and testing sets of the letters data frame using letters$letter as the first input to the sample.split function. Before splitting, set your seed to 2000. Again put 50% of the data in the training set. (Why do we need to split the data again? Remember that sample.split balances the outcome variable in the training and testing sets. With a new outcome variable, we want to re-generate our split.)
library(caTools)
set.seed(2000) # to get the same split everytime
spl = sample.split(letters$letter, SplitRatio = 0.5)
train2 = subset(letters, spl==TRUE)
test2= subset(letters, spl==FALSE)

#In a multiclass classification problem, a simple baseline model is to predict the most frequent class of all of the options.
table(train2$letter) #tells us that "P" has the most observations

#What is the baseline accuracy on the testing set?
library(randomForest)

# Build random forest model
RFb = randomForest(letter ~ ., data = train2) 
#or RFb = randomForest(isB ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor, data=train)

predictions = predict(RFb, newdata=test2)
#Now lets assess the accuracy of the model through confusion matrix
cmat_forest<-table(test2$letter, predictions)  #first arg is thr true outcomes and the second is the predicted outcomes
cmat_forest

401/nrow(test2)  #or table(test2$letter)

#PROBLEM 2.2 - PREDICTING THE LETTERS A, B, P, R  

#Now build a classification tree to predict "letter", using the training set to build your model. You should use all of the other variables as independent variables, except "isB", since it is related to what we are trying to predict! Just use the default parameters in your CART model. Add the argument method="class" since this is a classification problem. Even though we have multiple classes here, nothing changes in how we build the model from the binary case.
# Build random forest model
CARTletter= rpart(letter ~ .-isB, data = train2,method="class" ) 
prp(CARTletter)

#What is the test set accuracy of your CART model? Use the argument type="class" when making predictions.
#(HINT: When you are computing the test set accuracy using the confusion matrix, you want to add everything on the main diagonal and divide by the total number of observations in the test set, which can be computed with nrow(test), where test is the name of your test set).

# Make predictions
predictLetter= predict(CARTletter, newdata = test2, type = "class") #We need to give type = "class" if we want the majority class predictions.This is like using a threshold of 0.5.

#Now lets assess the accuracy of the model through confusion matrix
cmat_CART<-table(test2$letter, predictLetter)  #first arg is the true outcomes and the second is the predicted outcomes
cmat_CART

#lets now compute the overall accuracy
accu_CART <- (cmat_CART[1,1] + cmat_CART[2,2] + cmat_CART[3,3] + cmat_CART[4,4])/sum(cmat_CART) #(348+318+363+340)/nrow(test2)
accu_CART  

#Now build a random forest model on the training data, using the same independent variables as in the previous problem -- again, don't forget to remove the isB variable. Just use the default parameter values for ntree and nodesize (you don't need to include these arguments at all). Set the seed to 1000 right before building your model. (Remember that you might get a slightly different result even if you set the random seed.)

library(randomForest)
set.seed(1000)

# Build random forest model
RFletter= randomForest(letter ~ .-isB, data = train2) 

#What is the test set accuracy of your random forest model?
predictLetter= predict(RFletter, newdata=test2)

#Now lets assess the accuracy of the model through confusion matrix
cmat_forest<-table(test2$letter, predictLetter)  #first arg is thr true outcomes and the second is the predicted outcomes
cmat_forest

#lets now compute the overall accuracy
accu_forest <- (cmat_forest[1,1] + cmat_forest[2,2] + cmat_forest[3,3] + cmat_forest[4,4])/sum(cmat_forest)
accu_forest  #or (390+380 +393+364)/nrow(test2) = 0.9801027 

# Assignment 4.3. PREDICTING EARNINGS FROM CENSUS DATA

#PROBLEM 1.1 - A LOGISTIC REGRESSION MODEL

#Let's begin by building a logistic regression model to predict whether an individual's earnings are above $50,000 (the variable "over50k") using all of the other variables as independent variables. First, read the dataset census.csv into R.

#lets read in the dataset
Census<-read.csv("census.csv")
str(Census)

#Then, split the data randomly into a training set and a testing set, setting the seed to 2000 before creating the split. Split the data so that the training set contains 60% of the observations, while the testing set contains 40% of the observations.

library(caTools)

# Randomly split data into training and test sets
set.seed(2000)
spl<-sample.split(Census$over50k, SplitRatio=0.6)#the DV is split into 60% training set and 40% test set
train<-subset(Census,spl== TRUE)
test <-subset(Census, spl== FALSE)

#Next, build a logistic regression model to predict the dependent variable "over50k", using all of the other variables in the dataset as independent variables. Use the training set to build the model.

#Build logistic regression model using all other variables
censusglm<-glm(over50k ~ ., data=train, family=binomial)

summary(censusglm)

#PROBLEM 1.2 - A LOGISTIC REGRESSION MODEL  

#What is the accuracy of the model on the testing set? Use a threshold of 0.5. (You might see a warning message when you make predictions on the test set - you can safely ignore it.)

#Out-of-Sample predictions of the Logistic Regression model
predictTest<-predict(censusglm, newdata=test, type="response")

#Accuracy of model using threshold of 0.5
cmat_LR<- table(test$over50k, predictTest > 0.5)
cmat_LR

#Overall accuracy on the testing set
accu_LR <-(cmat_LR[1,1] + cmat_LR[2,2])/sum(cmat_LR)
accu_LR #If we divide the sum of the main diagonal by the sum of all of the entries in the matrix, we obtain the accuracy:(9051+1888)/(9051+662+1190+1888) = 0.8552107

#PROBLEM 1.3 - A LOGISTIC REGRESSION MODEL  

#What is the baseline accuracy for the testing set?

#We need to first determine the most frequent outcome in the training set. To do that, we table the dependent variable in the training set:
table(train$over50k)

#Our baseline model accuracy for the test set is the most frequent outcome as determined from above training set
table(test$over50k) 

9713/nrow(test) #9713/(9713+3078) = 0.7593621

#PROBLEM 1.4 - A LOGISTIC REGRESSION MODEL  

#What is the area-under-the-curve (AUC) for this model on the test set?

#Lets evaluate our model using the ROC curve
library(ROCR)

#Compute the test set predictions
ROCRpred<- prediction(predictTest ,test$over50k)
perf<- performance(ROCRpred, "tpr", "fpr")
plot(perf)

##Compute the AUC 
as.numeric(performance(ROCRpred, "auc")@y.values)

#PROBLEM 2.1 - A CART MODEL  

#We have just seen how the logistic regression model for this data achieves a high accuracy. Moreover, the significances of the variables give us a way to gauge which variables are relevant for this prediction task. However, it is not immediately clear which variables are more important than the others, especially due to the large number of factor variables in this problem.

#Let us now build a classification tree to predict "over50k". Use the training set to build the model, and all of the other variables as independent variables. Use the default parameters, so don't set a value for minbucket or cp. Remember to specify method="class" as an argument to rpart, since this is a classification problem. After you are done building the model, plot the resulting tree.

#Build CART model using all defaults
library(rpart)
library(rpart.plot)

censustree<-rpart(over50k ~ ., data=train, method="class") #method="class" arg tells us to make classification tree and not regression tree
prp(censustree)

#Make predictions
PredictCART = predict(censustree, newdata = test, type = "class") #We need to give type = "class" if we want the majority class predictions.This is like using a threshold of 0.5.

#Now lets assess the accuracy of the model through confusion matrix
cmat_CART<-table(test$over50k, PredictCART)  #first arg is the true outcomes and the second is the predicted outcomes
cmat_CART

#lets now compute the overall accuracy
accu_CART <- (cmat_CART[1,1] + cmat_CART[2,2])/sum(cmat_CART)
accu_CART  #(9243+1596)/(9243+470+1482+1596) = 0.8473927 #sum the diagonal entries and divide by the sum of all of the terms

#or 
sum(diag(cmat_CART)) /nrow(test)

#2nd method using the probabilities
PredictCART <- predict(censustree, newdata=test)[,2]
cmat_CART<- table(test$over50k, PredictCART > 0.5)
sum(diag(cmat_CART)) /nrow(test)

#PROBLEM 2.5 - A CART MODEL 

#Let us now consider the ROC curve and AUC for the CART model on the test set. You will need to get predicted probabilities for the observations in the test set to build the ROC curve and compute the AUC. Remember that you can do this by removing the type="class" argument when making predictions, and taking the second column of the resulting object.

#Lets evaluate our model using the ROC curve
library(ROCR)

#Generate the predictions for the tree. Note that unlike the previous question, when we call the predict function, we leave out the argument type = "class" from the function call. Without this extra part, we will get the raw probabilities of the dependent variable values for each observation, which we need in order to generate the AUC. We need to take the second column of the output:
PredictROC = predict(censustree, newdata = test)
head(PredictROC)

#First we use the prediction() function with first argument the second column of PredictROC, and second argument the true outcome values, Test$Reverse.
#We pass the output of prediction() to performance() to which we give also two arguments for what we want on the X and Y axes of our ROC curve, true positive rate and false positive rate.
pred = prediction(PredictROC[,2], test$over50k)
perf = performance(pred, "tpr", "fpr")

#and the plot
plot(perf)

#Compute the AUC of the CART model
as.numeric(performance(pred, "auc")@y.values)

#PROBLEM 2.6 - A CART MODEL  

#What is the AUC of the CART model on the test set?

#Compute the AUC of the CART model
as.numeric(performance(pred, "auc")@y.values)

#PROBLEM 3.1 - A RANDOM FOREST MODEL

#Before building a random forest model, we'll down-sample our training set. While some modern personal computers can build a random forest model on the entire training set, others might run out of memory when trying to train the model since random forests is much more computationally intensive than CART or Logistic Regression. For this reason, before continuing we will define a new training set to be used when building our random forest model, that contains 2000 randomly selected obervations from the original training set. Do this by running the following commands in your R console (assuming your training set is called "train"):

set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

#Let us now build a random forest model to predict "over50k", using the dataset "trainSmall" as the data used to build the model. Set the seed to 1 again right before building the model, and use all of the other variables in the dataset as independent variables. (If you get an error that random forest "can not handle categorical predictors with more than 32 categories", re-build the model without the nativecountry variable as one of the independent variables.)

#Build random forest model
library(randomForest)

set.seed(1)
CensusForest<-randomForest(over50k ~ ., data=trainSmall)

#Then, make predictions using this model on the entire test set. What is the accuracy of the model on the test set, using a threshold of 0.5? (Remember that you don't need a "type" argument when making predictions with a random forest model if you want to use a threshold of 0.5. Also, note that your accuracy might be different from the one reported here, since random forest models can still differ depending on your operating system, even when the random seed is set. )

#Make predictions
predictTest= predict(CensusForest, newdata = test)

#Now lets assess the accuracy of the model through confusion matrix
cmat_forest<-table(test$over50k, predictTest)  #first arg is thr true outcomes and the second is the predicted outcomes
cmat_forest

#lets now compute the overall accuracy
accu_forest <- (cmat_forest[1,1] + cmat_forest[2,2])/sum(cmat_forest)
accu_forest  #or (9584+1090)/nrow(test) = 0.8348839

#or
sum(diag(cmat_forest)) / nrow(test)

#PROBLEM 3.2 - A RANDOM FOREST MODEL  

#As we discussed in lecture, random forest models work by building a large collection of trees. As a result, we lose some of the interpretability that comes with CART in terms of seeing how predictions are made and which variables are important. However, we can still compute metrics that give us insight into which variables are important.

#One metric that we can look at is the number of times, aggregated over all of the trees in the random forest model, that a certain variable is selected for a split. To view this metric, run the following lines of R code (replace "MODEL" with the name of your random forest model):

#Find out the number of times, aggregated over all of the trees in random forest model, that a certain variable is selected for a split:

vu = varUsed(CensusForest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(CensusForest$forest$xlevels[vusorted$ix]))

#PROBLEM 3.3 - A RANDOM FOREST MODEL  

#A different metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf of the tree is. In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased. Therefore, one way to measure the importance of a variable is to average the reduction in impurity, taken over all the times that variable is selected for splitting in all of the trees in the forest. To compute this metric, run the following command in R (replace "MODEL" with the name of your random forest model):

#Impurity which measures how homogenous each bucket or leaf of the tree is.
varImpPlot(CensusForest)

#PROBLEM 4.1 - SELECTING CP BY CROSS-VALIDATION  

#We now conclude our study of this data set by looking at how CART behaves with different choices of its parameters.

#Let us select the cp parameter for our CART model using k-fold cross validation, with k = 10 folds. Do this by using the train function. Set the seed beforehand to 2. Test cp values from 0.002 to 0.1 in 0.002 increments, by using the following command:

cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

#Selecting cp parameter of our CART model using kfold cross validation, with k=10 folds and cp values from 0.002 to 0.1 in 0.002 increments.

library(caret)
library(e1071)

#Set the seed to 2:
set.seed(2)

#Define cross-validation experiment

#specify that we are going to use k-fold cross validation with 10 folds:
numFolds = trainControl( method = "cv", number = 10 )

#Specify the grid of cp values that we wish to evaluate:
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
#This will define our cp parameters to test as numbers from 0.002 to 0.1, in increments of 0.002.

#Perform the cross validation by running the train function and view the result:
save_CV<-train(over50k~.,data=train,method="rpart",trControl=numFolds,   tuneGrid=cartGrid)
save_CV

#Also, remember to use the entire training set "train" when building this model. The train function might take some time to run.

plot(save_CV) #or

plot(save_CV$results$cp, save_CV$results$Accuracy, type="l", xlab="cp", ylab="accuracy")

#Which value of cp does the train function recommend?
save_CV$bestTune

#PROBLEM 4.2 - SELECTING CP BY CROSS-VALIDATION 

#Fit a CART model to the training data using this value of cp. What is the prediction accuracy on the test set?

#Create a new CART model i.e. fit a CART model to the training data using this value of cp:
CensusCV <- rpart(over50k ~ ., data=train, method="class", cp=save_CV$bestTune) # using the cp value (save_CV$bestTune=0.02) got from Cross validation above

#Make predictions (Out-of-Sample predictions of the Cross Validated CART model)
PredictCV = predict(CensusCV, newdata = test, type = "class")
cmat_CART_CV<-table(test$over50k, PredictCV) #confusion matrix
cmat_CART_CV

#lets now compute the overall accuracy
accu_CART_CV <- (cmat_CART_CV[1,1] + cmat_CART_CV[2,2])/sum(cmat_CART_CV)
accu_CART_CV  #9178+1838)/(9178+535+1240+1838) = 0.8612306.

#or
sum(diag(cmat_CART_CV)) / nrow(test)

#PROBLEM 4.3 - SELECTING CP BY CROSS-VALIDATION  

#Compared to the original accuracy using the default value of cp, this new CART model is an improvement, and so we should clearly favor this new model over the old one -- or should we? Plot the CART tree for this model. How many splits are there?

#What does this decision tree look like?
prp(CensusCV)