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
