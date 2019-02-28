wine = read.csv("wine.csv")
str(wine)
summary(wine)
model1 = lm(Price ~ AGST, data=wine)
summary(model1)
model1$residuals
SSE = sum(model1$residuals^2)
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)
SSE = sum(model2$residuals^2)
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)
SSE = sum(model3$residuals^2)

cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

predictTest = predict(model4, newdata = wineTest)
predictTest
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST

baseball = read.csv("baseball.csv")
str(baseball)
moneyball = subset(baseball, Year < 2002)
str(moneyball)
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)
plot(moneyball$RD, moneyball$W)
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)
RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg) 

lm.RA<-lm(RA~OOBP+OSLG, data=moneyball)
lm.RS<-lm(RS~OBP+SLG)
lm.RS<-lm(RS~OBP+SLG, data=moneyball)
summary(lm.RS)
test<-data.frame('player'=c('Chavez', 'Giambi', 'Menechino', 'Myers', 'Pena'), 'OBP'=c(0.338, 0.391,0.369,0.313,0.361), 'SLG'=c(0.540,0.450,0.374,0.447,0.500))
test
predict(lm.RS, test)

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2012)
cor(teamRank, wins2013)

#Recitation 2: Playing baseball in the NBA
#Read NBA file
NBA = read.csv("NBA_train.csv")
str(NBA)

#Check the Playoff number using table function
table(NBA$W, NBA$Playoffs)

#Calculate the Diff
NBA$PTSdiff = NBA$PTS - NBA$oppPTS

#Plot the diff with the Win
plot(NBA$PTSdiff, NBA$Win)

#Build a regression model
WinsReg = lm(W ~ PTSdiff, data = NBA)
summary(WinsReg)

#build a regression model of Points
PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data= NBA)
summary(PointsReg)

#Calculate the residuals, SSE, SST, SSR
PointsReg$residuals
SSE = sum(PointsReg$residuals^2)
SSE
RMSE = sqrt(SSE/nrow(NBA))
RMSE
mean(NBA$PTS)

#Improve the model by removing some insignificant independent variables
PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data= NBA)
summary(PointsReg2)
PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data= NBA)
summary(PointsReg3)
PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data= NBA)
summary(PointsReg4)

#Recheck residuals, SSE, SST, SSR
SSE_4 = sum(PointsReg4$residuals^2)
SSE_4
RMSE_4 = sqrt(SSE_4/nrow(NBA))
RMSE_4
RMSE

#Make prediction  by first reading test data
NBA_test = read.csv("NBA_test.csv")
summary(NBA_test)
str(NBA_test)

#Bulld prediction model
PointsPredictions = predict(PointsReg4, newdata=NBA_test)

#Measure the prediction goodness of fit, need to calculate the sample R-squared
SSE = sum((PointsPredictions - NBA_test$PTS)^2)
SSE
SST = sum((mean(NBA$PTS)- NBA_test$PTS)^2)
R2 = 1 - SSE/SST
R2
RMSE = sqrt(SSE/nrow(NBA_test))
RMSE

#Assignment 2 for Unit_2

#Assignment 2.1: Climate Change
#Read the data Climate_change
Climatechange = read.csv("climate_change.csv")
str(Climatechange)
summary(Climatechange)
View(Climatechange)

#Split the data into training and testing data
training = subset(Climatechange,Year < 2007)
str(training)
summary(training)
testing = subset(Climatechange, Year > 2006)

#Build a regression model using training data
Temperature = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=training)
summary(Temperature)

#Calculate the RMSE
SSE = sum(Temperature$residuals^2)
SSE
RMSE = sqrt(SSE/nrow(training))
RMSE
mean(training$Temp)

#Check the year with the highest level of MEI in the training set
sort(tapply(training$MEI, training$Year, mean))
training[which.max(training$MEI),]$Year

#Calculate the correlation among independent variables
cor(training$CH4, training$N2O)
cor(training)

#Build a new regression model after removing some independent variables
Temperature2 = lm(Temp ~ MEI + TSI + N2O + Aerosols, data=training)
summary(Temperature2)

#Create a new simplified model from the initial full-var model using the step function
Temperaturestep = step(Temperature)
summary(Temperaturestep)

#Create a prediction model using the initial model from training dataset
PredictTest = predict(Temperaturestep, newdata = testing)
SSE = sum((testing$Temp - PredictTest)^2)
SST = sum((testing$Temp - mean(training$Temp))^2)
1-(SSE/SST)

#Assignment 2.2: Reading Test Scores
#Read the training and testing data
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
str(pisaTrain)

#Calculate the average reading score based on sex of student
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

#Check which variables of training data are missing data
summary(pisaTrain)

#Define the variables as the unordered factor variables (>=3)
str(pisaTrain)

#Remove missing data for training and testing set and check the diff with the initial data
pisaTrain1 = na.omit(pisaTrain) 
pisaTest1 = na.omit(pisaTest)
str(pisaTrain1)
str(pisaTest1)

#Consider the variable "raceeth" in our problem, which has levels "American Indian/Alaska Native", "Asian", "Black", "Hispanic", "More than one race", "Native Hawaiian/Other Pacific Islander", and "White".
#Because it is the most common in our population, we will select White as the reference level.
#Which binary variables will be included in the regression model?
#Set the reference level of the factor by typing the following two lines in your R console:
pisaTrain1$raceeth = relevel(pisaTrain1$raceeth, "White")
pisaTest1$raceeth = relevel(pisaTest1$raceeth, "White")
str(pisaTrain1)

#Build a regression model using training set to predict readingScore using all remaining variables
lmScore = lm(readingScore ~ ., data=pisaTrain1)
summary(lmScore)

#Predict the score for the testing data using the model already built, do not remove any variables
PredTest = predict(lmScore, newdata = pisaTest1)
summary(PredTest)
#Calculate the range between max and min value of prediction
637.7 - 353.2

#Calculate SSE, RMSE
SSE = sum((pisaTest1$readingScore - PredTest)^2)
sqrt(mean((PredTest - pisaTest1$readingScore)^2))
SSE

#Calculate the predicted value of test score used in the baseline model
baseline = mean(pisaTrain1$readingScore)
baseline

#calculte the sum of squared errors of the baseline model on the testing set
SST = sum((pisaTest1$readingScore - mean(pisaTrain1$readingScore))^2)
SST

#Calculate R2
1-SSE/SST

#Assignment 2.3: Detecting Flu Epidemics via Search Engine Query Data
#Loading the FluTrain data
FluTrain = read.csv("FluTrain.csv")
str(FluTrain)

#Plot the histogram of the depedent var ILI
hist(FluTrain$ILI)

#Plot the natural logarithm of ILI versus Queries
plot(log(FluTrain$ILI), FluTrain$Queries)

#Build a regression model
FluTrend1 = lm(log(FluTrain$ILI) ~ FluTrain$Queries, data = FluTrain)
summary(FluTrend1)

#Testing the correlation between 2 variables and its relationship with R2 from the model
(cor(FluTrain$Queries, log(FluTrain$ILI)))^2

#Load the FluTest data
FluTest = read.csv("FluTest.csv")
str(FluTest)

#Obtain predictions of the ILI value by converting from predictions of log(ILI) to predictions of ILI via exponentiation, or the exp() function.
PredTest1 = predict(FluTrend1, newdata=FluTest)
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
summary(PredTest1)

#What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012?
which(FluTest$Week %in% "2012-03-11 - 2012-03-17")
PredTest1[11]
(FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]

#What should be the amount of time to lag the observations?
View(FluTrain)

#Install and load the zoo package
install.packages("zoo")
library(zoo)
#create the ILILag2 variable in the training set:
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)

#Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using the Queries variable as well as the log of the ILILag2 variable.
FluTrend2 = lm(log(FluTrain$ILI) ~ FluTrain$Queries + log(FluTrain$ILILag2), data = FluTrain)
summary(FluTrend2)

#add ILILag2 to the FluTest data frame
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest)

#plot the log of ILILag2 against the log of ILI
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

#What is the new value of the ILILag2 variable in the first row of FluTest?
#What is the new value of the ILILag2 variable in the second row of FluTest?
nrow(FluTrain)
FluTest$ILI.lag2[1:2]<- FluTrain$ILI[416:417]
FluTest$ILI.lag2[1]
FluTest$ILI.lag2[2]

#Obtain test set predictions of the ILI variable from the FluTrend2 model
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE<- sum((PredTest2 - FluTest$ILI)^2)
SSE
RMSE<- sqrt(SSE/nrow(flu.test))
RMSE
