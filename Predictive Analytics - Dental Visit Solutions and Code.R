## 
## Predictive Analytics - Day 1
## 

setwd("Documents/Data/RData")

# Explore the data
train = read.csv("DentalVisit-Train.csv")
str(train)
summary(train)

str(train$bmi)
min.bmi <- max(na.omit(train$bmi))
min.bmi
max.bmi <- max(na.omit(train$bmi))
max.bmi

# Without missing value filtered
hist(train$bmi, col = "darkblue", main = "History of bmi (no NA filter)", cex.axis = 1.5, cex.lab = 1.5)




summary(train$bm)
tmp <- as.data.frame(na.omit(train$bmi))
colnames(tmp) <- "bmi"
bmi.omit <- tmp$bmi[tmp$bmi < max.bmi]

length(train$bmi)
length(bmi.omit)
max(train$bmi)
max(bmi.omit)


par(mfrow=c(1,2))
# Without missing value filtered
hist(train$bmi, col="darkblue",main="History of bmi (no NA filter)", cex.axis = 1.5, cex.lab = 1.5)

# With missing value filtered
hist(bmi.omit, col="darkgreen",main="History of bmi (NA filtered)", cex.axis = 1.5, cex.lab = 1.5)


# train$log.bmi <- log(train$bmi)

table(train$agegrp, mfrow=c(1,1))
par(mar=c(6,3,1,1)+0.2) 

barplot(table(train$agegrp), col="darkgreen", main = "Age group distribution", las = 2)

table(train$race)
barplot(table(train$race),col = "darkblue", main = "Race group distribution", las = 2)

# Create racial percentage table
race.tbl <- table(train$race)
race.dt <- as.data.frame(race.tbl)
colnames(race.dt)[1] <- "race"
tot.nbr <- sum(race.dt[2])
race.dt.per <- race.dt
race.dt.per[2] <- 100 * race.dt[2] / tot.nbr
race.dt.per.matrix <- as.matrix(t(race.dt.per)[2, ])
rownames(race.dt.per.matrix) <- t(race.dt.per)[1, ] 
barplot(t(race.dt.per.matrix), col = "darkblue", main = "Race group distribution", ylab = "% Race", las = 2)

plot(density(bmi.omit), col = "darkgreen", main = "Density plot of BMI")


par(mfrow=c(1,1))

# "bmi" and "children" scatter plot
plot(jitter(train[which(train$children > 0 & train$bmi < 55), ]$children, amount = 1), jitter(train[which(train$children > 0 & train$bmi < 55), ]$bmi, amount = 1), ylab = "BMI", xlab = "Number of children", main = "BMI vs # Children", col="darkblue", type = "p")

#bmi vs. race boxplots
par(mfrow=c(1,1))
boxplot(train$bmi[which(train$bmi < 60)] ~ train$race[which(train$bmi < 60)], xlab = "Race", ylab = "BMI", col = "darkred")

table(train$agegrp, train$race)

library(dplyr)
# Remove the NAs in 'bmi' and 'race'
data.tmp <- train[(!is.na(train$bmi) & !is.na(train$race)), ]
# Group by 'race'
agg.race <- group_by(data.tmp, race)
# Summary BMI stats based on the group by
summary.bmi <- summarise(agg.race, minBMI = min(bmi), maxBMI = max(bmi), medianBMI = median(bmi))
summary.bmi <- as.data.frame(summary.bmi)

par(mfrow=c(1,2))
# Original code using jitter
plot(jitter(train[which(train$children > 0 & train$bmi < 55), ]$children, amount = 1), jitter(train[which(train$children > 0 & train$bmi < 55), ]$bmi, amount = 1), ylab = "BMI", xlab = "Number of children", main = "BMI vs # Children", col = "darkblue", type = "p")
# y-axis values
y.axis = train[which(train$children > 0 & train$bmi < 55), ]$bmi
# x-axis values
x.axis = train[which(train$children > 0 & train$bmi < 55), ]$children
plot(y.axis ~ x.axis, ylab = "BMI", xlab = "Number of children", main = "BMI vs # Children", col = "darkgreen", type = "p")


# print number of lines with missing values
df <- data.frame(train)
T <- length(train$sex)
cat <- length(names(train))
d <- 0
for (i in 1:T ){
  a <- sum((1:cat)[is.na(df[i, ])])
  if(a == 0){
    d <- d + 1
  }
}
d

# Imputing meds using logistic regression

# Number of missing values for "meds" (37) 
nbr.misg <- sum(is.na(train$meds)) 
# library(MASS) 
MedsReg <- glm(meds ~ ., family = binomial, data = na.omit(train)) 
# Extract the temporary data for logistic regression modeling 
mod.train <- subset(train, select = c("meds", "sex", "insured")) 
# Model training 
MedsReg <- glm(meds ~ sex + insured, family = binomial, data = na.omit(mod.train)) 
# Modeling results summary 
summary(MedsReg) 


# Estimate meds value using the model 
z <- predict(MedsReg, train, type = 'response') 
# Fill in the missing values of 'meds' with estimated values from the model 
newMeds <- NULL
for (ele in 1:length(train$meds)) { 
  if (is.na(train$meds[ele])) { 
    newMeds[ele] <- -z[ele] 
  } else { 
    newMeds[ele] <- train$meds[ele] 
  } 
} 
# Number of missing values after imputation dropped to 0 
nbr.misg.new <- sum(is.na(newMeds))
nbr.misg.new

# Make histogram of the number of children
par(mfrow=c(1,1))
hist(train$children, 15, col = "darkgreen", xlab="number of children", main="Histogram of children")

# Imputing the number of children using ZeroInflPoisson

# Number of missing values for "children" (201)
chld.nbr.mis <- sum(is.na(train$children))

library(pscl)

# To estimate "children" values using zero-inflated count model using other variables
# Zero-inflated count models are two-component mixture models combining a point mass
# at zero with a proper count distribution.

ZeroInfChildren <- zeroinfl(formula = children ~ agegrp + marital.stat + employ + race + insured + employ.ins + sex + health, data = na.omit(train[c("children", "agegrp", "marital.stat", "employ","race", "health","insured", "employ.ins", "sex")]))

# To fill in the missing values for "children" using the model estimate
newchildren <- train$children
z <- predict(ZeroInfChildren, train)
for (ele in 1:length(train$children)){
  if (is.na(train$children[ele])) {
    newchildren[ele] <- z[ele]
  } 
}
train$newchildren <- as.integer(newchildren)

# Number of missing values for "children" after imputation dropped to 51
chld.nbr.mis.new <- sum(is.na(train$newchildren))
chld.nbr.mis.new


# Make histogram of the number of children 
par(mfrow=c(1,2)) 
hist(train$children, 5, xlab = "number of children", main = "Histogram of children (before imputation)", col = "dark green") 
hist(train$newchildren, 5, xlab = "number of children", main = "Histogram of children (after imputation)", col = "dark blue")



tbl.old = table(train$children) 
tbl.new = table(train$newchildren) 
diff.tbl = tbl.old - tbl.new
diff.tbl

# LRT on interactions (on clean data set)

train <- read.csv("DentalVisit-Clean.csv")
# Setting NAs as factor
install.packages("gam")
library(gam)
train <- na.gam.replace(train)
# 
# 
m1 <- glm(dental.visit ~ meds+emergency,data=train,family=binomial)
m2 <- glm(dental.visit~meds*emergency,data=train,family=binomial)
anova(m1,m2,test='Chisq')

# Model selection

train$log.bmi=log(train$bmi)
base1 <- glm(dental.visit ~ phone + sex + agegrp + race + employ.ins + insured + employ + marital.stat + postponed.care + emergency + specialist + meds + log.bmi + children + confident + educ + health + log.bmi + phone:confident +employ.ins:specialist + sex:marital.stat + race:employ, data=train, family = "binomial")

stepsDental <- stepAIC(base1, direction="both")

summary(stepsDental)

z <- predict(base1, type = 'response') 
y <- vector()
for(i in 1:3417) {
  if(z[i] < 0.5) {
    y[i] <- "No"
  } else {
    y[i] <- "Yes"
  }
}
y <- as.factor(y)
library(caret)
confusionMatrix(table(data.frame(train$dental.visit, y)))
