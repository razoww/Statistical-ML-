adv <- "str"

#linear regression
# Import Advertising data
adver <- read.csv('C:/Users/avandelaer/Documents/Statistical & machine learning/SML_Section1_v2.1/data/Advertising.csv', row.names=1)
head(adver)

options(repr.plot.width=8, repr.plot.height=4)  #size
par(mfrow=c(1, 3))  # regions
plot(adver$TV, adver$sales, ylab="Sales", xlab="TV")
abline(lm(sales ~ TV, data=adver), col='red')
plot(adver$radio, adver$sales, ylab="Sales", xlab="Radio")
abline(lm(sales ~ radio, data=adver), col='red')
plot(adver$newspaper, adver$sales, ylab="Sales", xlab="Newspaper")
abline(lm(sales ~ newspaper, data=adver), col='red')

mtext("Sales and Advertising Budget Data", side=3, line=-2.5, outer=TRUE)

# Sales ~ TV
options(repr.plot.width=6, repr.plot.height=4.5)  # size

plot(adver$TV, adver$sales, pch=21, cex=0.8, bg='red',
     xlab='TV', ylab='Sales')
abline(lm(sales ~ TV, data=adver), col='blue')

#Exercise 1

adver$num <- mapply(function (x,y) (x - mean(adver$TV)) * (y - mean(adver$sales)), adver$TV, adver$sales)
adver$denom <- mapply(function (x) (x - mean(adver$TV))**2, adver$TV)
B1 <- sum(adver$num) / sum(adver$denom)

B0 <- mean(adver$sales) - B1*mean(adver$TV)

print(paste("B0=",B0, "B1=", B1))

# Check B0, B1 by lm function
md <- lm(sales ~ TV, data=adver)
summary(md)

#Exercise 2

adver$num <- mapply(function (x,y) (x - mean(adver$radio)) * (y - mean(adver$sales)), adver$radio, adver$sales)
adver$denom <- mapply(function (x) (x - mean(adver$radio))**2, adver$radio)
B1 <- sum(adver$num) / sum(adver$denom)

B0 <- mean(adver$sales) - B1*mean(adver$radio)

print(paste("B0=",B0, "B1=", B1))

# Check B0, B1 by lm function
summary(lm(sales ~ radio, data=adver))

#Exercise 3

adver$num    <- mapply(function (x,y) (x - mean(adver$TV)) * (y - mean(adver$sales)), adver$TV, adver$sales)
adver$denom  <- mapply(function (x) (x - mean(adver$TV))**2, adver$TV)
B1         <- sum(adver$num) / sum(adver$denom)           
B0         <- mean(adver$sales) - B1*mean(adver$TV)

adver$rss    <- mapply(function(x,y) (y - B0 - B1*x)**2, adver$TV, adver$sales) 
RSS        <- sum(adver$rss)
RSE        <- (RSS/(nrow(adver)-2))**0.5

SE_B0      <- ((RSE**2)*(1/nrow(adver) + (mean(adver$TV))**2 / sum(adver$denom)))**0.5
SE_B1      <- (RSE**2/sum(adver$denom))**0.5 

B0_ubound  <- B0 + 2 * SE_B0
B0_lbound  <- B0 - 2 * SE_B0

B1_ubound  <- B1 + 2 * SE_B1
B1_lbound  <- B1 - 2 * SE_B1

#print(SE_B0)
#print(SE_B1)

print(paste("95% confidence interval B0:", B0_lbound, B0_ubound))
print(paste("95% confidence interval B1:", B1_lbound, B1_ubound))

# Check
SE_B0 <- 0.457843
SE_B1 <- 0.002691

B0_ubound  <- B0 + 2 * SE_B0
B0_lbound  <- B0 - 2 * SE_B0

B1_ubound  <- B1 + 2 * SE_B1
B1_lbound  <- B1 - 2 * SE_B1

print(paste("95% confidence interval B0:", B0_lbound, B0_ubound))
print(paste("95% confidence interval B1:", B1_lbound, B1_ubound))

# Double check with the lm function
md <- lm(sales ~ TV, data=adv)
summary(md)

#Exercise 4
adver$num    <- mapply(function (x,y) (x - mean(adver$radio)) * (y - mean(adver$sales)), adver$radio, adver$sales)
adver$denom  <- mapply(function (x) (x - mean(adver$radio))**2, adver$radio)
B1         <- sum(adver$num) / sum(adver$denom)           
B0         <- mean(adver$sales) - B1*mean(adver$radio)

adver$rss    <- mapply(function(x,y) (y - B0 - B1*x)**2, adver$radio, adver$sales) 
RSS        <- sum(adver$rss)
RSE        <- (RSS/(nrow(adver)-2))**0.5

SE_B0      <- ((RSE**2)*(1/nrow(adver) + (mean(adver$radio))**2 / sum(adver$denom)))**0.5
SE_B1      <- (RSE**2/sum(adver$denom))**0.5 

t          <- (B1 - 0) / SE_B1
p          <- 2*pt(t, nrow(adver)-1, lower=FALSE)

print(paste("T-Statistic: ", t))

# Show the model of Sales ~ f(Radio)
md <- lm(sales ~ radio, data=adver)
summary(md)

adver$num    <- mapply(function (x,y) (x - mean(adver$newspaper)) * (y - mean(adver$sales)), adver$newspaper, adver$sales)
adver$denom  <- mapply(function (x) (x - mean(adver$newspaper))**2, adver$newspaper)
B1         <- sum(adver$num) / sum(adver$denom)           
B0         <- mean(adver$sales) - B1*mean(adver$newspaper)

adver$rss    <- mapply(function(x,y) (y - B0 - B1*x)**2, adver$newspaper, adver$sales) 
RSS        <- sum(adver$rss)
RSE        <- (RSS/(nrow(adver)-2))**0.5

SE_B0      <- ((RSE**2)*(1/nrow(adver) + (mean(adver$newspaper))**2 / sum(adver$denom)))**0.5
SE_B1      <- (RSE**2/sum(adver$denom))**0.5 

t          <- (B1 - 0) / SE_B1
p          <- 2*pt(t, nrow(adver)-1, lower=FALSE)

print(paste("T-Statistic: ", t))

# Show the model of Sales ~ f(Newspaper)
md <- lm(sales ~ newspaper, data=adver)
summary(md)

#Exercise 5
adver$num    <- mapply(function (x,y) (x - mean(adver$radio)) * (y - mean(adver$sales)), adver$radio, adver$sales)
adver$denom  <- mapply(function (x) (x - mean(adver$radio))**2, adver$radio)
B1         <- sum(adver$num) / sum(adver$denom)           
B0         <- mean(adver$sales) - B1*mean(adver$radio)

adver$rss    <- mapply(function(x,y) (y - B0 - B1*x)**2, adver$radio, adver$sales) 
RSS        <- sum(adver$rss)
RSE        <- (RSS/(nrow(adver)-2))**0.5

adver$y2     <- mapply(function (x) (x - mean(adver$sales))**2, adver$sales)
Cor_XY     <- sum(adver$num) / ((sum(adver$denom)**0.5) * (sum(adver$y2)**0.5))
R2         <- Cor_XY**2

print(paste("RSE:", RSE, " R2", R2))

# Show the model of Sales ~ f(Radio)
md <- lm(sales ~ radio, data=adver)
summary(md)

adver$num    <- mapply(function (x,y) (x - mean(adver$newspaper)) * (y - mean(adver$sales)), adver$newspaper, adver$sales)
adver$denom  <- mapply(function (x) (x - mean(adver$newspaper))**2, adver$newspaper)
B1         <- sum(adver$num) / sum(adver$denom)           
B0         <- mean(adver$sales) - B1*mean(adver$newspaper)

adver$rss    <- mapply(function(x,y) (y - B0 - B1*x)**2, adver$newspaper, adver$sales) 
RSS        <- sum(adver$rss)
RSE        <- (RSS/(nrow(adver)-2))**0.5

adver$y2     <- mapply(function (x) (x - mean(adver$sales))**2, adver$sales)
Cor_XY     <- sum(adver$num) / ((sum(adver$denom)**0.5) * (sum(adver$y2)**0.5))
R2         <- Cor_XY**2

print(paste("RSE:", RSE, " R2", R2))

# Show the model of Sales ~ f(Newspaper)
md <- lm(sales ~ newspaper, data=adv)
summary(md)

#Exercise 6
# ?? = ((X'X)**-1)X'y
X <- as.matrix(cbind(1, adver$TV, adver$radio, adver$newspaper))
y <- as.matrix(adver$sales)

B <- solve(t(X) %*% X) %*% t(X) %*% y

for (coef in 1:nrow(B)){print(paste(coef, "Coefficient:", B[coef]))}

# Compare with the function of R
md <- lm(sales ~ ., data=adver)
summary(md)

##Exercises from the book

####Chapter 2####
#################

####Exercise 2#### 
#	regression
#	inference
#	n = 500 observations
#	p = 3 variables (profit, number of employees, industry)

####Exercise 10#### 

require(ISLR)
library(MASS)


# Question a)
Boston
str(Boston)

#data set includes 506 rows and 14 columns
#rows represent observations for each town
#columns represent features

# Question b)
pairs(Boston)

#Question c)
library(ggplot2)
library(reshape2)

bosmelt <- melt(Boston, id="crim")
ggplot(bosmelt, aes(x=value, y=crim)) +
  facet_wrap(~variable, scales="free") + 
  geom_point()

#Question d)
g <- ggplot(Boston, aes(x=1:nrow(Boston), y=crim))
g + geom_point()

g <- ggplot(Boston, aes(x=1:nrow(Boston), y=tax))
g + geom_point()

g <- ggplot(Boston, aes(x=1:nrow(Boston), y=ptratio))
g + geom_point()

#definitely outliers for crim and tax
#no clear outlier for ptratio

#Question e)
table(Boston$chas)

# 0    1
# 471  35

#Question f)
median(Boston$ptratio)
#19.05

#Question g)
(seltown <- Boston[Boston$medv == min(Boston$medv),])
#Two towns with lowest medv value of 5

#Question h)
# count of towns
nrow(Boston[Boston$rm > 7,])  
# 64 suburbs

nrow(Boston[Boston$rm > 8,])  
# 13 suburbs

rbind(sapply(Boston[Boston$rm > 8,], mean), sapply(Boston, median))

####Exercise 3 (Chap.3)####

#Question A
#salary = B0 + B1*gpa + B2*iq + B3*gender_f + B4*(gpa*iq) + B5*(gpa*gender_f)
B0 <- 50
B1 <- 20
B2 <- 0.07
B3 <- 35
B4 <- 0.01
B5 <- -10

iq <- 110
gpa <- 4

gender_f <- 1
salary <- B0 + B1*gpa + B2*iq + B3*gender_f + B4*(gpa*iq) + B5*(gpa*gender_f)
print(paste("F",salary))

gender_f <- 0
salary <- B0 + B1*gpa + B2*iq + B3*gender_f + B4*(gpa*iq) + B5*(gpa*gender_f)
print(paste("M",salary))
#Answer iii -> For a fixed value of IQ and GPA, males earn more on average than females provided that the GPA is high enough (>3.5)

#Question B
#Answer: 137.1

#Question C
#Answer: FALSE, The impact of the interaction term is measured in the unit of the coefficients.
#If the amount of gpa or iq is high, the interaction effect (1% increase) results in a bigger impact as well

#Exercise 10 (Chap.3)####
library(ISLR)
head(Carseats, 2)
#?Carseats

#Question A
lm.fit <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm.fit)

# Question B
#B0 y intercept -> with all the other coefficients being 0, the sales are 13.04
#B1 (price) -> significant (p-value < 0.05), negative: the higher the price, the lower the sales
#B2 (UrbanYes) -> not significant (p-value > 0.05) negative impact on sales
#B3 (USYes) -> significant (p-value < 0.05) positive: if store is in the US, sales are higher 

# Question C
sales <- Price <- UrbanYes <- USYes <- 0
lm.fit$coeff
sales <- B0 + B1 * Price + B2 * UrbanYes + B3 * USYes

#Question D
#Urban -> p-value = 0.936 > 0.05 so Urban is on a 95% confidence level not statistically relevant

#Question E
lm.fit1 <- lm(Sales ~ Price + US, data = Carseats)
summary(lm.fit1)

#Question F
#R2 for Model 1 = 0.2335 ~ R2 Model 2 = 0.2354
# both models only explain 23% of the total variance for sales in the dataset

#Question G
confint(lm.fit1, level = 0.95)

#Question H
summary(Carseats[,c("Sales", "Price", "US")])
#there is no evidence for outliers or high leverage observation points


#Exercise 15 (Chap.3)####
#?Boston

#Question A
sum <- NULL
for (coeff in names(Boston)){
  lm.fit <- lm(Boston$crim ~ Boston[,coeff])
  t <- summary(lm.fit)$coefficients[2,]
  sum <- cbind(sum, t)
  colnames(sum)[ncol(sum)] <- coeff
}
sum

sum[, sum[4,] < 0.05]
#All coefficients are significant on an individual level

#Question C
library(tidyr)
par1 <- t(sum1)
par1 <- par1[,2:ncol(par1)]
par <- sum[,2:ncol(sum)]

par
par1

plot(par[1,], par1[1,], col=factor(colnames(par)))

#Question D
#Yes, The plots from do not show a simple linear relationship between response and predictor variables
# predictor variables "rad" and "black"
