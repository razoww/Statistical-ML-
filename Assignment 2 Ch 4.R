### Exercise 6 

#a)

z <- exp(-6+(.05*40)+(1*3.5)) 
p <- z/(1+z)
p

## If the students dedicates 40 hours of study, there will be -5% chance of getting an A in the class. 

#b)

z1 <- exp(-6+(.05*50)+(1*3.5)) 
p1 <- z1/(1+z1)
p1

## The students needs 50 hours of study in order to have a 50% chance of getting an A in the class.

##########################################################################################################

### Exercise 8
## We should use logistic regression since training error is zeto when k=1 in KNN method. So the real test error is 36% and not 18%. 
## KNN test error (38%) is greater than logistic regression test error (30%) 

##########################################################################################################

### Exercise 9

#a)

.37/(1+.37)

#b)
.16/(1-.16)

##########################################################################################################

### Exercise 10
install.packages("ISLR")
library(ISLR)
summary(Weekly)

# a)

cor(Weekly[, -9])

attach(Weekly)
plot(Volume)

# b)

fit.glm <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(fit.glm)

# c) 

probs <- predict(fit.glm, type = "response")
pred.glm <- rep("Down", length(probs))
pred.glm[probs > 0.5] <- "Up"
table(pred.glm, Direction)

# d)

train <- (Year < 2009)
Weekly.20092010 <- Weekly[!train, ]
Direction.20092010 <- Direction[!train]
fit.glm2 <- glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summary(fit.glm2)

probs2 <- predict(fit.glm2, Weekly.20092010, type = "response")
pred.glm2 <- rep("Down", length(probs2))
pred.glm2[probs2 > 0.5] <- "Up"
table(pred.glm2, Direction.20092010)

# In this case, we may conclude that the percentage of correct predictions on the test data is (9+56)/104 wich is equal to 62.5%. 
# In other words 37.5% is the test error rate. We could also say that for weeks when the market goes up, the model is right 91.8032787% of 
# the time (56/(56+5)). For weeks when the market goes down, the model is right only 20.9302326% of the time (9/(9+34)).

# e) 

library(MASS)
fit.lda <- lda(Direction ~ Lag2, data = Weekly, subset = train)
fit.lda

pred.lda <- predict(fit.lda, Weekly.20092010)
table(pred.lda$class, Direction.20092010)

# We may conclude that the percentage of correct predictions on the test data is 62.5%. In other words 37.5% is the test error rate. 
# We could also say that for weeks when the market goes up, the model is right 91.8032787% of the time. For weeks when the market goes down, 
# the model is right only 20.9302326% of the time. These results are very close to those obtained with the logistic regression model which is 
# not surpising.

# f)

fit.qda <- qda(Direction ~ Lag2, data = Weekly, subset = train)
fit.qda

pred.qda <- predict(fit.qda, Weekly.20092010)
table(pred.qda$class, Direction.20092010)

# In this case, we may conclude that the percentage of correct predictions on the test data is 58.6538462%. In other words 41.3461538% is the
# test error rate. We could also say that for weeks when the market goes up, the model is right 100% of the time. For weeks when the market goes
# down, the model is right only 0% of the time. We may note, that QDA achieves a correctness of 58.6538462% even though the model chooses "Up" 
# the whole time !

# g)

library(class)
train.X <- as.matrix(Lag2[train])
test.X <- as.matrix(Lag2[!train])
train.Direction <- Direction[train]
set.seed(1)
pred.knn <- knn(train.X, test.X, train.Direction, k = 1)
table(pred.knn, Direction.20092010)

# we may conclude that the percentage of correct predictions on the test data is 50%. In other words 50% is the test error rate. We could 
# also say that for weeks when the market goes up, the model is right 50.8196721% of the time. For weeks when the market goes down, the model 
# is right only 48.8372093% of the time.

# h)

# If we compare the test error rates, we see that logistic regression and LDA have the minimum error rates, followed by QDA and KNN.

# I)

# Logistic regression with Lag2:Lag1
fit.glm3 <- glm(Direction ~ Lag2:Lag1, data = Weekly, family = binomial, subset = train)
probs3 <- predict(fit.glm3, Weekly.20092010, type = "response")
pred.glm3 <- rep("Down", length(probs3))
pred.glm3[probs3 > 0.5] = "Up"
table(pred.glm3, Direction.20092010)

mean(pred.glm3 == Direction.20092010)

# LDA with Lag2 interaction with Lag1
fit.lda2 <- lda(Direction ~ Lag2:Lag1, data = Weekly, subset = train)
pred.lda2 <- predict(fit.lda2, Weekly.20092010)
mean(pred.lda2$class == Direction.20092010)

# QDA with sqrt(abs(Lag2))
fit.qda2 <- qda(Direction ~ Lag2 + sqrt(abs(Lag2)), data = Weekly, subset = train)
pred.qda2 <- predict(fit.qda2, Weekly.20092010)
table(pred.qda2$class, Direction.20092010)

mean(pred.qda2$class == Direction.20092010)

# KNN k =10
pred.knn2 <- knn(train.X, test.X, train.Direction, k = 10)
table(pred.knn2, Direction.20092010)

mean(pred.knn2 == Direction.20092010)

# KNN k = 100
pred.knn3 <- knn(train.X, test.X, train.Direction, k = 100)
table(pred.knn3, Direction.20092010)

mean(pred.knn3 == Direction.20092010)

# Out of these combinations, the original logistic regression and LDA have the best performance in terms of test error rates.