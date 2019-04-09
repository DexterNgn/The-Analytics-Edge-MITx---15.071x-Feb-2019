# Video 6

# After following the steps in the video, load the data into R
movies = read.table("movieLens.txt", header=FALSE, sep="|",quote="\"")
str(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

# Take a look at our data again:
str(movies)
table(movies$Comedy)
table(movies$Western)
table(movies$Romance, movies$Drama)

# Video 7
# Compute distances
distances = dist(movies[2:20], method = "euclidean")

# Hierarchical clustering
clusterMovies = hclust(distances, method = "ward") 

# Plot the dendrogram
plot(clusterMovies)

# Assign points to clusters
clusterGroups = cutree(clusterMovies, k = 2)

#Now let's figure out what the clusters are like.

# Let's use the tapply function to compute the percentage of movies in each genre and cluster
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)
tapply(movies$Adventure, clusterGroups, mean)
tapply(movies$Animation, clusterGroups, mean)
tapply(movies$Comedy, clusterGroups, mean)
tapply(movies$Crime, clusterGroups, mean)
tapply(movies$Drama, clusterGroups, mean)

# We can repeat this for each genre. If you do, you get the results in ClusterMeans.ods

# Find which cluster Men in Black is in.
subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]

# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]

# Assignment 6
# Assignment 6.1. Document Clustering with Daily Kos

# Hierarchical Clustering
# You can read in the data set, compute the distances, and build the hierarchical clustering model by using the following commands:
dailykos = read.csv("dailykos.csv")
kosDist = dist(dailykos, method="euclidean")
kosHierClust = hclust(kosDist, method="ward.D")

#The distance computation can take a long time if you have a lot of observations and/or if there are a lot of variables. As we saw in recitation, it might not even work if you have too many of either!

#You can plot the dendrogram with the command:
plot(kosHierClust)

# where "kosHierClust" is the name of your clustering model.

# You can split your data into clusters by first using the cutree function to compute the cluster numbers:
hierGroups = cutree(kosHierClust, k = 7)

# Then, you can use the subset function 7 times to split the data into the 7 clusters:
HierCluster1 = subset(dailykos, hierGroups == 1)
HierCluster2 = subset(dailykos, hierGroups == 2)
HierCluster3 = subset(dailykos, hierGroups == 3)
HierCluster4 = subset(dailykos, hierGroups == 4)
HierCluster5 = subset(dailykos, hierGroups == 5)
HierCluster6 = subset(dailykos, hierGroups == 6)
HierCluster7 = subset(dailykos, hierGroups == 7)

# If you use the nrow function on each of these new datasets, you can see that cluster 3 has 374 observations, cluster 1 has the most observations, and cluster 4 has the fewest number of observations.
# Alternatively, you could answer these questions by looking at the output of table(hierGroups).

# More Advanced Approach:
# There is a very useful function in R called the "split" function. Given a vector assigning groups like hierGroups, you could split dailykos into the clusters by typing:
HierCluster = split(dailykos, hierGroups)

# Then cluster 1 can be accessed by typing HierCluster[[1]], cluster 2 can be accessed by typing HierCluster[[2]], etc. If you have a variable in your current R session called "split", you will need to remove it with rm(split) before using the split function.

# Instead of looking at the average value in each variable individually, we'll just look at the top 6 words in each cluster. To do this for cluster 1, type the following in your R console (where "HierCluster1" should be replaced with the name of your first cluster subset):
tail(sort(colMeans(HierCluster1)))

# This computes the mean frequency values of each of the words in cluster 1, and then outputs the 6 words that occur the most frequently. The colMeans function computes the column (word) means, the sort function orders the words in increasing order of the mean values, and the tail function outputs the last 6 words listed, which are the ones with the largest column means.

# You can repeat the command on each of the clusters by typing the following:
tail(sort(colMeans(HierCluster2)))
tail(sort(colMeans(HierCluster3)))
tail(sort(colMeans(HierCluster4)))
tail(sort(colMeans(HierCluster5)))
tail(sort(colMeans(HierCluster6)))
tail(sort(colMeans(HierCluster7)))

# K-Means Clustering

# You can run k-means clustering by using the following commands:
  
set.seed(1000)
KmeansCluster = kmeans(dailykos, centers=7)

# Then, you can subset your data into the 7 clusters by using the following commands:
KmeansCluster1 = subset(dailykos, KmeansCluster$cluster == 1)
KmeansCluster2 = subset(dailykos, KmeansCluster$cluster == 2)
KmeansCluster3 = subset(dailykos, KmeansCluster$cluster == 3)
KmeansCluster4 = subset(dailykos, KmeansCluster$cluster == 4)
KmeansCluster5 = subset(dailykos, KmeansCluster$cluster == 5)
KmeansCluster6 = subset(dailykos, KmeansCluster$cluster == 6)
KmeansCluster7 = subset(dailykos, KmeansCluster$cluster == 7)

# Alternatively, you could answer these questions by looking at the output of 
table(KmeansCluster$cluster)

# More Advanced Approach:
  
# There is a very useful function in R called the "split" function. Given a vector assigning groups like KmeansCluster$cluster, you could split dailykos into the clusters by typing:
  
KmeansCluster = split(dailykos, KmeansCluster$cluster)

# Then cluster 1 can be accessed by typing KmeansCluster[[1]], cluster 2 can be accessed by typing KmeansCluster[[2]], etc. If you have a variable in your current R session called "split", you will need to remove it with rm(split) before using the split function.

# You can output the most frequent words in each of the k-means clusters by using the following commands:
  
tail(sort(colMeans(KmeansCluster1)))

tail(sort(colMeans(KmeansCluster2)))

tail(sort(colMeans(KmeansCluster3)))

tail(sort(colMeans(KmeansCluster4)))

tail(sort(colMeans(KmeansCluster5)))

tail(sort(colMeans(KmeansCluster6)))

tail(sort(colMeans(KmeansCluster7)))

# By looking at the output, you can see that the cluster best correponding to the Iraq War is cluster 3 (top words are iraq, war, and bush) and the cluster best corresponding to the democratic party is cluster 2 (top words dean, kerry, clark, and edward).

# From "table(hierGroups, KmeansCluster$cluster)", we read that 116 (80.6%) of the observations in K-Means Cluster 2 also fall in Hierarchical Cluster 7.

# From "table(hierGroups, KmeansCluster$cluster)", we read that 171 (61.7%) of the observations in K-Means Cluster 3 also fall in Hierarchical Cluster 5.

# From "table(hierGroups, KmeansCluster$cluster)", we read that no more than 123 (39.9%) of the observations in K-Means Cluster 7 fall in any hierarchical cluster.

# From "table(hierGroups, KmeansCluster$cluster)", we read that 320 (97.3%) of observations in K-Means Cluster 6 fall in Hierarchical Cluster 2.

# Assignment 6.2, Market Segmentation for Airlines

# Market segmentation is a strategy that divides a broad target market of customers into smaller, more similar groups, and then designs a marketing strategy specifically for each group. Clustering is a common technique for market segmentation since it automatically finds similar groups given a data set. 

# In this problem, we'll see how clustering can be used to find similar groups of customers who belong to an airline's frequent flyer program. The airline is trying to learn more about its customers so that it can target different customer segments with different types of mileage offers. 

# The file AirlinesCluster.csv contains information on 3,999 members of the frequent flyer program. This data comes from the textbook "Data Mining for Business Intelligence," by Galit Shmueli, Nitin R. Patel, and Peter C. Bruce. For more information, see the website for the book.

# There are seven different variables in the dataset, described below:
#-	Balance = number of miles eligible for award travel
#-	QualMiles = number of miles qualifying for TopFlight status
#-	BonusMiles = number of miles earned from non-flight bonus transactions in the past 12 months
#-	BonusTrans = number of non-flight bonus transactions in the past 12 months
#-	FlightMiles = number of flight miles in the past 12 months
#-	FlightTrans = number of flight transactions in the past 12 months
#-	DaysSinceEnroll = number of days since enrolled in the frequent flyer program

# Read the data
airlines = read.csv("AirlinesCluster.csv")
str(airlines)
View(airlines)
summary(airlines)

# Normalize the data
install.packages("caret")
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

# Compute the distance
airlinesDist = dist(airlinesNorm, method="euclidean")
airlinesHierClust = hclust(airlinesDist, method="ward.D")
plot(airlinesHierClust)

airlinesclusterGroups = cutree(airlinesHierClust, k = 5)
HierCluster1 = subset(airlinesNorm, airlineshierGroups == 1)
HierCluster2 = subset(airlinesNorm, airlineshierGroups == 2)
HierCluster3 = subset(airlinesNorm, airlineshierGroups == 3)
HierCluster4 = subset(airlinesNorm, airlineshierGroups == 4)
HierCluster5 = subset(airlinesNorm, airlineshierGroups == 5)

table(clusterGroups)

tapply(airlines$Balance, airlinesclusterGroups, mean)

tapply(airlines$QualMiles, airlinesclusterGroups, mean)

tapply(airlines$BonusMiles, airlinesclusterGroups, mean)

tapply(airlines$BonusTrans, airlinesclusterGroups, mean)

tapply(airlines$FlightMiles, airlinesclusterGroups, mean)

tapply(airlines$FlightTrans, airlinesclusterGroups, mean)

tapply(airlines$DaysSinceEnroll, airlinesclusterGroups, mean)

# Run K-Mean Clustering
set.seed(88)
KmeansCluster = kmeans(airlinesNorm, centers=5, iter.max = 1000)

table(KmeansCluster$cluster)

KmeansCluster1 = subset(airlinesNorm, KmeansCluster$cluster == 1)
KmeansCluster2 = subset(airlinesNorm, KmeansCluster$cluster == 2)
KmeansCluster3 = subset(airlinesNorm, KmeansCluster$cluster == 3)
KmeansCluster4 = subset(airlinesNorm, KmeansCluster$cluster == 4)
KmeansCluster5 = subset(airlinesNorm, KmeansCluster$cluster == 5)
table(airlineshierGroups, KmeansCluster$cluster)

# Assignment 6.3. Predicting Stock Returns with Cluster-Then-Predict
# Predicting Stock Returns with Cluster-Then-Predict
# In the second lecture sequence this week, we heard about cluster-then-predict, a methodology in which you first cluster observations and then build cluster-specific prediction models. In the lecture sequence, we saw how this methodology helped improve the prediction of heart attack risk. In this assignment, we'll use cluster-then-predict to predict future stock prices using historical stock data.
# When selecting which stocks to invest in, investors seek to obtain good future returns. In this problem, we will first use clustering to identify clusters of stocks that have similar returns over time. Then, we'll use logistic regression to predict whether or not the stocks will have positive future returns.

#For this problem, we'll use StocksCluster.csv, which contains monthly stock returns from the NASDAQ stock exchange. The NASDAQ is the second-largest stock exchange in the world, and it lists many technology companies. The stock price data used in this problem was obtained from infochimps, a website providing access to many datasets.
#Each observation in the dataset is the monthly returns of a particular company in a particular year. The years included are 2000-2009. The companies are limited to tickers that were listed on the exchange for the entire period 2000-2009, and whose stock price never fell below $1. So, for example, one observation is for Yahoo in 2000, and another observation is for Yahoo in 2001. Our goal will be to predict whether or not the stock return in December will be positive, using the stock returns for the first 11 months of the year.
#This dataset contains the following variables:
# ReturnJan = the return for the company's stock during January (in the year of the observation). 
# ReturnFeb = the return for the company's stock during February (in the year of the observation). 
# ReturnMar = the return for the company's stock during March (in the year of the observation). 
# ReturnApr = the return for the company's stock during April (in the year of the observation). 
# ReturnMay = the return for the company's stock during May (in the year of the observation). 
# ReturnJune = the return for the company's stock during June (in the year of the observation). 
# ReturnJuly = the return for the company's stock during July (in the year of the observation). 
# ReturnAug = the return for the company's stock during August (in the year of the observation). 
# ReturnSep = the return for the company's stock during September (in the year of the observation). 
# ReturnOct = the return for the company's stock during October (in the year of the observation). 
# ReturnNov = the return for the company's stock during November (in the year of the observation). 
# PositiveDec = whether or not the company's stock had a positive return in December (in the year of the observation). This variable takes value 1 if the return was positive, and value 0 if the return was not positive.
# For the first 11 variables, the value stored is a proportional change in stock value during that month. For instance, a value of 0.05 means the stock increased in value 5% during the month, while a value of -0.02 means the stock decreased in value 2% during the month.

# In cluster-then-predict, our final goal is to predict the dependent variable, which is unknown to us at the time of prediction. Therefore, if we need to know the outcome value to perform the clustering, the methodology is no longer useful for prediction of an unknown outcome value.

# This is an important point that is sometimes mistakenly overlooked. If you use the outcome value to cluster, you might conclude your method strongly outperforms a non-clustering alternative. However, this is because it is using the outcome to determine the clusters, which is not valid.

# From mean(stocksTrain$ReturnJan) and mean(stocksTest$ReturnJan), we see that the average return in January is slightly higher in the training set than in the testing set. Since normTest was constructed by subtracting by the mean ReturnJan value from the training set, this explains why the mean value of ReturnJan is slightly negative in normTest.

# Read the data
stocks = read.csv("StocksCluster.csv")
View(stocks)
str(stocks)

table(stocks$PositiveDec)
6324/(6324+5256)

# Check correlation
cor(stocks)
summary(stocks)

set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec ~ ., family = binomial, 
data = stocksTrain)
predictTrain = predict(StocksModel, type="response")
table(stocksTrain$PositiveDec, predictTrain > 0.5)

predictTest = predict(StocksModel, type="response", newdata=stocksTest)
table(stocksTest$PositiveDec, predictTest > 0.5)

table(stocksTest$PositiveDec)

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

set.seed(144)
km = kmeans(normTrain, centers=3, iter.max = 1000)
table(km$cluster)
KmCluster1 = subset(normTrain, km$cluster == 1)
KmCluster2 = subset(normTrain, km$cluster == 2)
KmCluster3 = subset(normTrain, km$cluster == 3)
table(km$cluster)

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)
mean(stocksTrain1$PositiveDec)

StocksModel1 = glm(PositiveDec ~ ., family = binomial, data = stocksTrain1)
StocksModel2 = glm(PositiveDec ~ ., family = binomial, data = stocksTrain2)
StocksModel3 = glm(PositiveDec ~ ., family = binomial, data = stocksTrain3)
predictTest1 = predict(StocksModel1, type="response", newdata=stocksTest1)
table(stocksTest1$PositiveDec, predictTest1 > 0.5)

predictTest2 = predict(StocksModel2, type="response", newdata=stocksTest2)
table(stocksTest2$PositiveDec, predictTest2 > 0.5)

predictTest3 = predict(StocksModel3, type="response", newdata=stocksTest3)
table(stocksTest3$PositiveDec, predictTest3 > 0.5)

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
> cmatoverall<-table(AllOutcomes, AllPredictions > 0.5)
> cmatoverall
