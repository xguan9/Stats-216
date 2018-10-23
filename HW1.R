## (a) Read in college.csv 
college = read.csv("College.csv", header = TRUE)

## (b) Turn the first column into row names
rownames(college) = college[,1]
fix(college)
## (b) Eliminate the column containing the row names
college = college[,-1]
fix(college)

##(c) Summary of all variables in the data frame
summary(college)
## Diagonal Plot of the first 10 variables in data frame 'college'
png("diagonal.png")
pairs(college[,1:10])
dev.off()
## Box Plot: Out-of-State Tuition at Public vs. Private Schools
png("Boxplot1.png")
plot(college$Private,college$Outstate, xlab = "Private School", ylab = "Out of State Tuition ($)", 
     main = "Out-of-State Tuition at Public vs. Private Schools")
dev.off()
##Create a new variable 'Elite' based on variable 'Top10perc'
Elite = rep("No",nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college,Elite)
summary(college)
## Plot Out-of-State Tuition at Elite vs. Non-Elite Schools
png("Boxplot2.png")
plot(college$Elite,college$Outstate, xlab = "Elite School", ylab = "Out of State Tuition ($)", 
     main = "Out-of-State Tuition at Public vs. Private Schools")
dev.off()

png("histogram.png")
par(mfrow = c(2,2))
hist(college$F.Undergrad, breaks = 20, xlab = "# Full Time Undergrad", ylab = "Frequency", main = "FT Undergrad")
hist((college$Accept/college$Apps)*100, breaks = 20, xlab = "Acceptance", ylab = "Frequency", main = "Acceptance Rate")
hist(subset(college, college$Elite == "Yes")[["Expend"]], breaks = 20, xlab = "Expenditure per Student ", ylab = "Frequency", main = "Expenditure in Elite Schools")
hist(subset(college, college$Elite == "No")[["Expend"]], breaks = 20, xlab = "Expenditure per Student ", ylab = "Frequency", main = "Expenditure in Non-Elite Schools")
dev.off()


##### Problem 4 #####
## (a) Read in college.csv 
college = read.csv("College.csv", header = TRUE)

## (b) Turn the first column into row names
rownames(college) = college[,1]
fix(college)
## (b) Eliminate the column containing the row names
college = college[,-1]
fix(college)

##(c) Summary of all variables in the data frame
summary(college)
## Diagonal Plot of the first 10 variables in data frame 'college'
png("diagonal.png")
pairs(college[,1:10])
dev.off()
## Box Plot: Out-of-State Tuition at Public vs. Private Schools
png("Boxplot1.png")
plot(college$Private,college$Outstate, xlab = "Private School", ylab = "Out of State Tuition ($)", 
     main = "Out-of-State Tuition at Public vs. Private Schools")
dev.off()
##Create a new variable 'Elite' based on variable 'Top10perc'
Elite = rep("No",nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college,Elite)
summary(college)
## Plot Out-of-State Tuition at Elite vs. Non-Elite Schools
png("Boxplot2.png")
plot(college$Elite,college$Outstate, xlab = "Elite School", ylab = "Out of State Tuition ($)", 
     main = "Out-of-State Tuition at Public vs. Private Schools")
dev.off()

png("histogram.png")
par(mfrow = c(2,2))
hist(college$F.Undergrad, breaks = 20, xlab = "# Full Time Undergrad", ylab = "Frequency", main = "FT Undergrad")
hist((college$Accept/college$Apps)*100, breaks = 20, xlab = "Acceptance", ylab = "Frequency", main = "Acceptance Rate")
hist(subset(college, college$Elite == "Yes")[["Expend"]], breaks = 20, xlab = "Expenditure per Student ", ylab = "Frequency", main = "Expenditure in Elite Schools")
hist(subset(college, college$Elite == "No")[["Expend"]], breaks = 20, xlab = "Expenditure per Student ", ylab = "Frequency", main = "Expenditure in Non-Elite Schools")
dev.off()

##### Problem 5 #####
library(ISLR)
college = read.csv("College.csv", header = TRUE)
rownames(college) = college[,1]
college = college[,-1]
Elite = rep("No",nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college,Elite)

train = c(1:length(1:floor(dim(college)[1]/2)))## Use the first half of the data as training
test = c(ceiling(dim(college)[1]/2):dim(college)[1])## Use the first half of the data as training
lm.train = lm(Apps ~.-Enroll-Accept-Elite, data = college, subset = train)
summary(lm.train)
lm.test = predict(lm.train, newdata = college[test,])
summary(lm.test)
mean(lm.train$residuals^2) ## 1623952
mean((lm.test - College$Apps[test])^2) ## 5995879

## (c)Try select the test and training data differently
set.seed(100)
ran = sample(1:nrow(college), size = floor(dim(college)[1]/2))
train2 = ran
test2 = -ran
lm.train2 = lm(Apps ~.-Enroll-Accept-Elite, data = college, subset = train2)
summary(lm.train2)
lm.test2 = predict(lm.train2, newdata = college[test2,])
summary(lm.test2)
mean(lm.train2$residuals^2)
mean((lm.test2 - College$Apps[test2])^2) 

## Get the summary and plot of college applications and found an outlier
summary(college$Apps)
plot(college$Apps)

##### Problem 6 #####
college = read.csv("College.csv", header = TRUE)
rownames(college) = college[,1]
college = college[,-1]
Elite = rep("No",nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college,Elite)

Y = rep(0, nrow(college))
Y[College$Apps > median(College$Apps)] = 1
college_catApp = data.frame(subset(college, select = -Apps),Y) ##replace the Apps column with Y (0 or 1)

train = college[1:floor(dim(college)[1]/2),] ## Use the first half of the data as training
test = college[ceiling(dim(college)[1]/2):nrow(college),] ## Use the second half of the data as testing

train4 = college_catApp[1:floor(dim(college_catApp)[1]/2),] 
test4 = college_catApp[ceiling(dim(college_catApp)[1]/2):nrow(college_catApp),] 
lg.train = glm(Y~.-Enroll-Accept-Elite, data = train4, family=binomial)
summary(lg.train)
train.probs=predict(lg.train, newdata=train4,type="response")
test.probs=predict(lg.train, newdata=test4,type="response") 
train.pred = ifelse(train.probs > 0.5, 1, 0)
mean(train.pred != train4$Y)
test.pred = ifelse(test.probs > 0.5, 1, 0)
mean(test.pred != test4$Y)

lm.train = lm(Apps ~.-Enroll-Accept-Elite, data = train)
lm.test.probs=predict(lm.train, newdata=test,type="response") 
lm.test.pred = ifelse(lm.test.probs > median(college$Apps), 1, 0)
table(lm.test.pred,test.pred)

