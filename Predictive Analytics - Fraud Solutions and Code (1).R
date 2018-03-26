
##############################################################
## Predictive Analytics Day 2
##############################################################

setwd("Documents/Data/RData")
read.csv("Sales.csv")
## Outlier detection

## Use variate distribution
set.seed(3147) 
par(mfrow=c(1,2))

x <- rnorm(100) 
plot(x, col = "darkgreen", main = "x")
summary(x)
# outliers 
boxplot(x, main = "x",col = "darkgreen")
# x outliers using box.plotstats
x.out <- boxplot.stats(x)$out
# find the index of outliers from x using %in% operator
outlier.x <- which(x %in% x.out)
x[outlier.x]
# [1] -3.315391  2.685922 -3.055717  2.571203

y <- rnorm(100) 
plot(y, col = "darkred", main = "y")
boxplot(y, main = "y",col = "darkred")
y.out <- boxplot.stats(y)$out
outlier.y <- which(y %in% y.out)
y[outlier.y]
# [1]  2.497527 -3.720281 -2.519041 -2.657271  2.403223

par(mfrow = c(1, 2))
boxplot(x, main = "x", col = "darkgreen")
boxplot(y, main = "y", col = "darkred")


#### Plot x and y outliers separately
par(mfrow=c(1, 2))
x <- data.frame(1:100, x)
plot(x, main = "x outlier", col = "darkgreen")
points(x[outlier.x, ], col = "red", pch = "x", cex = 1.5)

y <- data.frame(1:100, y)
plot(y, main = "y outlier", col = "darkred")
points(y[outlier.y, ], col = "green", pch = "+", cex = 1.5)




## Combine paired x-y data frame
df <- data.frame(x[, 2], y[, 2]) 
par(mfrow=c(1, 2))
head(df)

# outliers in both x and y 
outlier.list1 <- intersect(outlier.x, outlier.y)

plot(df, col=c("purple"), main = "x-y - outliers in both")
points(df[outlier.list1, ], col = "red", pch = "+", cex = 2.5)

# outliers in either x or y 
outlier.list2 <- union(outlier.x, outlier.y)
plot(df, col = c("blue"), main = "x-y - outliers in either") 
points(df[outlier.list2, ], col = "red", pch = "+", cex = 2.5)

## By LOF 
## Details see reference

## By clustering method 
# remove species from the data to cluster 
iris2 <- iris[, 1:4]
summary(iris2)

# Run clustering for three clusters
kmeans.result <- kmeans(iris2, centers = 3) 

# cluster centers 
kmeans.result$centers
# cluster IDs 
kmeans.result$cluster
  
# Get the centers
centers <- kmeans.result$centers[kmeans.result$cluster, ] 

# Calculate distances between objects and cluster centers 
distances <- sqrt(rowSums((iris2 - centers)^2)) 

# Pick top 5 largest distances 
outliers <- order(distances, decreasing = T)[1:5] 
# Print the outliers 
print(outliers)
  
print(iris2[outliers, ])

par(mfrow=c(1, 1))
# plot clusters 
plot(iris2[, c("Sepal.Length", "Sepal.Width")], pch = "o", col = kmeans.result$cluster, cex = 0.8, main = "Iris clusters and outliers") 
# plot cluster centers 
points(kmeans.result$centers[, c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 10, cex = 2.5) 
# plot outliers 
points(iris2[outliers, c("Sepal.Length", "Sepal.Width")], pch = "+", col = 4, cex = 1.5)


par(mfrow=c(1, 2))
plot(iris2[, c("Sepal.Length", "Sepal.Width")], pch = "o", col = kmeans.result$cluster, cex = 0.8, main = "Iris clusters and outliers") 
points(kmeans.result$centers[, c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 10, cex = 2.5) 
points(iris2[outliers, c("Sepal.Length", "Sepal.Width")], pch = "+", col = 4, cex = 1.5)
plot(iris2[, c("Petal.Length", "Petal.Width")], pch = "o", col = kmeans.result$cluster, cex = 0.8, main = "Iris clusters and outliers") 
points(kmeans.result$centers[, c("Petal.Length", "Petal.Width")], col = 1:3, pch = 10, cex = 2.5) 
points(iris2[outliers, c("Petal.Length", "Petal.Width")], pch = "+", col = 4, cex = 1.5)

# for two centers
kmeans.result2 <- kmeans(iris2, centers = 2) 
# Get the centers
centers2 <- kmeans.result2$centers[kmeans.result2$cluster, ] 

# Calculate distances between objects and cluster centers 
distances2 <- sqrt(rowSums((iris2 - centers2)^2)) 

# Pick top 5 largest distances 
outliers2 <- order(distances2, decreasing = T)[1:5] 

par(mfrow=c(1, 2))
plot(iris2[, c("Sepal.Length", "Sepal.Width")], pch = "o", col = kmeans.result2$cluster, cex = 0.8, main = "Iris clusters and outliers") 
points(kmeans.result2$centers[, c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 10, cex = 2.5) 
points(iris2[outliers2, c("Sepal.Length", "Sepal.Width")], pch = "+", col = 4, cex = 1.5)
plot(iris2[, c("Petal.Length", "Petal.Width")], pch = "o", col = kmeans.result2$cluster, cex = 0.8, main = "Iris clusters and outliers") 
points(kmeans.result2$centers[, c("Petal.Length", "Petal.Width")], col = 1:3, pch = 10, cex = 2.5) 
points(iris2[outliers2, c("Petal.Length", "Petal.Width")], pch = "+", col = 4, cex = 1.5)



####################################################################################################
### Loading the FRAUD CASE STUDY data into R
### Data dictionary
# "ID":     a factor for the salesman ID
# "Prod":   a factor for the product ID
# "Quant":  sales units
# "Val":    sales dollar amount
# "Fraud":  label for fraud inspection status (on the reported sales dollar amount)
# Code adapted from "Data Mining with R: Learning with Case Studies", 
# by L.Torgo
####################################################################################################

install.packages("DMwR")
library(DMwR)
# Read in Data
sales <- read.csv("Sales.csv",header=TRUE)
head(sales)

###################################################
### Exploring the data set
###################################################
summary(sales)
str(sales)
nlevels(sales$ID)
nlevels(sales$Prod)

## Check on the missing values
length(which(is.na(sales$Quant) & is.na(sales$Val)))

## To avoid using length() and which, re-code the line above in a more efficient way 
## (Note: T=0 and F=0 for the return of is.na
sum(is.na(sales$Quant) & is.na(sales$Val))

## Fraud freq distribution
fraud <- table(sales$Insp)
salesNum <- nrow(sales)
fraud.percent <- fraud/salesNum*100
fraud.percent


par(mfrow=c(1, 2))
## Number of transactions by salesman
totS <- table(sales$ID)
## Coefficient of variation
totS.cv <- sd(totS)/mean(totS)
totS.spl <- totS[1:50]

#plot(totS,main='Transactions per salespeople',names.arg='',xlab='Salespeople',ylab='Amount')
barplot(totS.spl, col = "darkgreen", main = 'Transactions per salespeople', names.arg = '', xlab = 'Salespeople', ylab = 'Amount')

## Number of transactions by products
totP <- table(sales$Prod)
## Coefficient of variation
totP.cv <- sd(totP)/mean(totP)
totP.spl <- totP[1:50]

barplot(totP.spl, col = "darkblue", main = 'Transactions per product', names.arg = '', xlab = 'Products', ylab = 'Amount')


par(mfrow=c(1, 1))

## Add a new column for price and check out the prices
sales$Uprice <- sales$Val/sales$Quant

summary(sales$Uprice)

attach(sales)
## Check out the median price by product
upp <- aggregate(Uprice, list(Prod), median, na.rm=T)

## Question: what's the best way to plot the price distribution?
hist(upp[, 2], col = "darkgreen", main =" Price Distribution")

## Explore the five most expensive and the five least expensive items
topP <- sapply(c(T, F), function(x) upp[order(upp[, 2], decreasing = x)[1:5], 1])
colnames(topP) <- c('Expensive', 'Cheap')
topP




## Question: How to do this using "by"?
upp.1 <- by(Uprice, Prod, median)


## Question: Plot the distribution with outliers excluded (filter price for less than 100 only)
hist(upp[which(upp$x < 100),]$x, col = "darkgreen", main = "Price Distribution")

topP[1, ]





## Find out how the most expensive and the least expensive items compare in terms of price distribution
## topP[1,] are the most expensive and the cheapest items
tops <- sales[Prod %in% topP[1, ] , c('Prod', 'Uprice')]
tops$Prod <- factor(tops$Prod)

par(mfrow=c(1,1))

#Use a log scale to avoid the cheapest item becoming indistinguishable
boxplot(Uprice ~ Prod, data = tops, ylab = 'Uprice', col = "darkred", log = "y", xlab = paste("Most Expensive","     ","Least Expensive"), main = "Price Distributions") 

## Explore the most performing the least performing salesmen (by summing up sales) 
vs <- aggregate(Val, list(ID), sum, na.rm = T)
scoresSs <- sapply(c(T, F), function(o) vs[order(vs$x, decreasing = o)[1:5], 1])
colnames(scoresSs) <- c('Most', 'Least')
scoresSs
unique(scoresSs[1, ]) #The most and the least performing salesmen

## %sales top 100 salesman generate
## Question: Remove as.double and see what happens
sum(as.double(vs[order(vs$x,decreasing=T)[1:100],2]))/sum(as.double(Val),na.rm=T)*100
## %sales bottom 2000 salesman generate
sum(as.double(vs[order(vs$x,decreasing=F)[1:2000],2]))/sum(as.double(Val),na.rm=T)*100
## Question: can you plot the sales distribution density function?

## Explore the best sales items the least sales items (by summing up quantity) 
qs <- aggregate(Quant, list(Prod), sum, na.rm = T)
scoresPs <- sapply(c(T, F), function(o) 
  qs[order(qs$x, decreasing = o)[1:5], 1])
colnames(scoresPs) <- c('Most', 'Least')
scoresPs

## %sales top 100 items generate
sum(as.double(qs[order(qs$x,decreasing=T)[1:100],2]))/
  sum(as.double(Quant),na.rm=T)*100
## %sales bottom 4000 items generate
sum(as.double(qs[order(qs$x,decreasing=F)[1:4000],2]))/
  sum(as.double(Quant),na.rm=T)*100




##################################################
#
# Detect product outliers using price distribution (Normal)
# Use the Box-plot outlier method
# OUtliers fall below Q1-1.5IQR or above Q3+1.5IQR
# IQR = Q3-Q1
# 
# The out component of function boxplot.stats() returns
# the outlier observations
###################################################

## Locate the outliers
outlier <- tapply(Uprice, list(Prod = Prod),
                  function(x) length(boxplot.stats(x)$out))

## Order the items by the # outliers
top10.out.items <- outlier[order(outlier, decreasing = T)[1:10]]

# To test above
# length(boxplot.stats(sales[which(sales$Prod=="p1125"),]$Uprice)$out)

# Transactions from the outlier observations
sum(outlier)
# %transactions from the outlier observations
sum(outlier)/nrow(sales)*100


###################################################
### Data problems
### Recall both Quant and Val have missing values 
### from earlier data exploration
###################################################
attach(sales)

## Calc. transactions by salesman
totS <- table(ID)
## Calc. transactions by product
totP <- table(Prod)

nas <- sales[which(is.na(Quant) & is.na(Val)), c('ID', 'Prod')]

# %sales transactions with missing values by salesman
propS <- 100*table(nas$ID)/totS
# Top 10 salesmen with missing values (by %missing values)
propS[order(propS, decreasing=T)[1:10]]

# %sales transactions with missing values by product
propP <- 100*table(nas$Prod)/totP
# Top 10 products with missing values (by %missing values)
propP[order(propP, decreasing=T)[1:10]]

# Remove the records with both Quant and Val missing
detach(sales)
sales.clean <- sales[-which(is.na(sales$Quant) & is.na(sales$Val)),]

# Total number of transactions with missing Quant by Product
nnasQp <- tapply(sales.clean$Quant,list(sales.clean$Prod),
                 function(x) sum(is.na(x)))

# Ratio of missing Quant by product
propNAsQp <- 100*nnasQp/table(sales.clean$Prod)
# Top 10 products with the highest missing Quant 
propNAsQp[order(propNAsQp,decreasing=T)[1:10]]

# Remove the two products with 100% missing Quant
sales.clean1 <- sales.clean[!sales.clean$Prod %in% c('p2442','p2443'),]

# #Products in the changed sales data
# nlevels(sales$Prod)
# sales$Prod <- factor(sales$Prod)
# nlevels(sales$Prod)

# Similarly, explore the missing Quant by sales
nnasQs <- tapply(sales.clean1$Quant, list(sales.clean1$ID), function(x) sum(is.na(x)))
propNAsQs <- 100*nnasQs/table(sales.clean1$ID)
propNAsQs[order(propNAsQs, decreasing = T)[1:10]]

# # Explore the missing Vale by product
# nnasVp <- tapply(sales.clean1$Val, list(sales.clean1$Prod),
#                  function(x) sum(is.na(x)))
# propNAsVp <- 100*nnasVp/table(sales.clean1$Prod)
# propNAsVp[order(propNAsVp, decreasing = T)[1:10]]
# 
# # Explore the missing Vale by sales.clean1man
# nnasVs <- tapply(sales.clean1$Val,list(sales.clean1$ID),function(x) sum(is.na(x)))
# propNAsVs <- nnasVs/table(sales.clean1$ID)
# propNAsVs[order(propNAsVs,decreasing=T)[1:10]]





#######################################################################
# Preparing for missing value impute

# Retrive non-fraud median price by product
tPrice <- tapply(sales.clean1[sales.clean1$Insp!='fraud','Uprice'],list(sales.clean1[sales.clean1$Insp!='fraud','Prod']),median,na.rm=T)

# Tag transactions with missing Quant values
noQuant <- which(is.na(sales.clean1$Quant))

# Tag transactions with missing Val values
noVal <- which(is.na(sales.clean1$Val))

# Impute the missing Quant values using Val and median price 
sales.clean1[noQuant,'Quant'] <- ceiling(sales.clean1[noQuant,'Val'] /
                                           tPrice[sales.clean1[noQuant,'Prod']])

# Impute the missing Val values using Quant and median price 
sales.clean1[noVal,'Val'] <- sales.clean1[noVal,'Quant'] *
  tPrice[sales.clean1[noVal,'Prod']]

# Derive the new price 
sales.clean1$Uprice <- sales.clean1$Val/sales.clean1$Quant




# ##############################################################
# ## LINEAR REGRESSION MODEL FITTING
# ##############################################################
# 
# # Linear regression model fitting
# install.packages("isdals")
# library(isdals)
# data(fev)
# summary(fev)
# 
# ## Plot the gender distribution
# hist(fev$Gender)
# 
# ##FEV - Forced Expiratory Volume
# # Convert variables to factors and get meaningful labels
# fev$Gender <- factor(fev$Gender, labels=c("Female", "Male"))
# fev$Smoke <- factor(fev$Smoke, labels=c("No", "Yes"))
# summary(fev)
# 
# # Fit the initial model. Interactions like Smoke*Age 
# # inherently include the main effects of Smoke and Age
# model <- lm(FEV ~ Ht + I(Ht^2) + Smoke*Gender + Smoke*Age, 
#             data=fev)
# summary(model)
# 
# # Start model fitting iterations by dropping less significant terms
# drop1(model, test="F")
# model2 <- lm(FEV ~ Ht + I(Ht^2) + Gender + Smoke*Age,
#              data=fev)
# summary(model2)
# drop1(model2, test="F")
# 
# # Remove the insignificant interaction and refit
# model3 <- lm(FEV ~ Ht + I(Ht^2) + Gender + Smoke + Age,
#              data=fev)
# drop1(model3, test="F")
# summary(model3)
