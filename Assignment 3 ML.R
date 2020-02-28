# Homework 3 #

#### Exercise 3 (p.198, Chap.5) ####

# a)

# The k-fold cross validation is implemented by taking the n observations and randomly splitting 
# it into k non-overlapping groups of length of (approximately) n/k. These groups acts as a validation set, and 
# the remainder (of length n-n/k) acts as a training set. The test error is then estimated by averaging the k 
# resulting MSE estimates.

# b)

# i. Advantages and disadvantages of validation set approach

# The validation set approach has two cons compared to k-fold cross-validation. First, the validation estimate of 
# the test error rate can be highly variable (depending on precisely which observations are included in the training set and 
# which observations are included in the validation set). Second, only a subset of the observations are used to fit the model. 
# Since statistical methods tend to perform worse when trained on fewer observations, this suggests that the validation set error
# rate may tend to overestimate the test error rate for the model fit on the entire data set.

# ii. # i. Advantages and disadvantages of LOOCV

# The LOOCV cross-validation approach is a special case of k-fold cross-validation in which k=n. This approach has two 
# drawbacks compared to k-fold cross-validation. First, it requires fitting the potentially computationally expensive model
# n times compared to k-fold cross-validation which requires the model to be fitted only k times. Second, the LOOCV 
# cross-validation approach may give approximately unbiased estimates of the test error, since each training set contains
# n-1 observations; however, this approach has higher variance than k-fold cross-validation (since we are averaging the 
# outputs of n fitted models trained on an almost identical set of observations, these outputs are highly correlated, and the 
# mean of highly correlated quantities has higher variance than less correlated ones). So, there is a bias-variance trade-off
# associated with the choice of k in k-fold cross-validation; typically using k=5 or k=10 yield test error rate estimates that 
# suffer neither from excessively high bias nor from very high variance.

#### Exercise 8 ((p.200, Chap.5) ####

# a)

set.seed(1)
y <- rnorm(100)
x <- rnorm(100)

y <- x - 2 * x^2 + rnorm(100)

# n = 100  and p = 2
# the model used is Y = X - 2 X2 + E

# b)

plot(x,y)
# In the plot we can see there is a curved relationship.

# c)

# i.

install.packages("boot")
library(boot)
set.seed(1)
Data <- data.frame(x, y)
fit.glm.1 <- glm(y ~ x)
cv.glm(Data, fit.glm.1)$delta[1]

# ii.

fit.glm.2 <- glm(y ~ poly(x, 2))
cv.glm(Data, fit.glm.2)$delta[1]

# iii.

fit.glm.3 <- glm(y ~ poly(x, 3))
cv.glm(Data, fit.glm.3)$delta[1]

# iv.

fit.glm.4 <- glm(y ~ poly(x, 4))
cv.glm(Data, fit.glm.4)$delta[1]

# d)

set.seed(10)
fit.glm.1 <- glm(y ~ x)
cv.glm(Data, fit.glm.1)$delta[1]

fit.glm.2 <- glm(y ~ poly(x, 2))
cv.glm(Data, fit.glm.2)$delta[1]

fit.glm.3 <- glm(y ~ poly(x, 3))
cv.glm(Data, fit.glm.3)$delta[1]

fit.glm.4 <- glm(y ~ poly(x, 4))
cv.glm(Data, fit.glm.4)$delta[1]

# The results above are the same than the ones obtained in c).

# e)

# The second model has the smallest SME (1.086596). This is due to the quadratic relationship between x and y in the graph of b).

# f)

summary(fit.glm.4)

# The p-values show that the linear and quadratic terms are statistically significants and that the cubic and 4th degree terms are not 
# statistically significants. This agrees strongly with cross-validation results.

#### Exercise 1 (p.259, Chap.6) ####

# a) The model with k predictors, which has the smallest training RSS is best subset selection.

# b) Best subset selection have the smallest test RSS.

# c) 

# i.   True
# ii.  True
# iii. False
# iv.  False
# v.   False

#### Exercise 9 (p.263, Chap.6) ####

# a)

install.packages("ISLR")
library(ISLR)
data(College)
set.seed(11)
train = sample(1:dim(College)[1], dim(College)[1] / 2)
test <- -train
College.train <- College[train, ]
College.test <- College[test, ]

# b) 

fit.lm <- lm(Apps ~ ., data = College.train)
pred.lm <- predict(fit.lm, College.test)
mean((pred.lm - College.test$Apps)^2)
# 1026096

# c) 

install.packages("glmnet")
library(glmnet)

train.mat <- model.matrix(Apps ~ ., data = College.train)
test.mat <- model.matrix(Apps ~ ., data = College.test)
grid <- 10 ^ seq(4, -2, length = 100)
fit.ridge <- glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
cv.ridge <- cv.glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge

pred.ridge <- predict(fit.ridge, s = bestlam.ridge, newx = test.mat)
mean((pred.ridge - College.test$Apps)^2)
# 1026069

# d)

fit.lasso <- glmnet(train.mat, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
cv.lasso <- cv.glmnet(train.mat, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)
mean((pred.lasso - College.test$Apps)^2)

predict(fit.lasso, s = bestlam.lasso, type = "coefficients")

# e)

install.packages("pls")
library(pls)

fit.pcr <- pcr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
validationplot(fit.pcr, val.type = "MSEP")

pred.pcr <- predict(fit.pcr, College.test, ncomp = 10)
mean((pred.pcr - College.test$Apps)^2)

# f)

fit.pls <- plsr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
validationplot(fit.pls, val.type = "MSEP")

pred.pls <- predict(fit.pls, College.test, ncomp = 10)
mean((pred.pls - College.test$Apps)^2)

# g)

test.avg <- mean(College.test$Apps)
lm.r2 <- 1 - mean((pred.lm - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
ridge.r2 <- 1 - mean((pred.ridge - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
lasso.r2 <- 1 - mean((pred.lasso - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
pcr.r2 <- 1 - mean((pred.pcr - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
pls.r2 <- 1 - mean((pred.pls - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)

# Test R2 for least squares is 0.9044281, test R2 for ridge is 0.9000536, test R2 for lasso is 0.8984123, test R2 for 
# pcr is 0.8127319 and test R2 for pls is 0.9062579. All models have good accuracy, except PCR.

#### Exercise 9 (p.299, Chap.7) ####

# a)


library(MASS)
fit <- lm(nox ~ poly(dis, 3), data = Boston)
summary(fit)

dis.grid <- seq(min(Boston$dis), max(Boston$dis), by = 0.1)
preds <- predict(fit, list(dis = dis.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2* preds$se.fit, preds$fit - 2 * preds$se.fit)
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, preds$fit, lwd = 2, col = "red")

matlines(dis.grid, se.bands, lwd = 1, col = "red", lty = 3)

# b) 

rss <- rep(NA, 10)
for (i in 1:10) {
  fit <- lm(nox ~ poly(dis, i), data = Boston)
  rss[i] <- sum(fit$residuals ^ 2)
}
plot(1:10, rss, type = 'l', xlab = "Degree", ylab = "RSS")

# c)

testMSE <- rep(NA, 10)
for (i in 1:10) {
  fit <- glm(nox ~ poly(dis, i), data = Boston)
  testMSE[i] <- cv.glm(Boston, fit, K = 10)$delta[1]
}
plot(1:10, testMSE, type = 'l', xlab = "Degree", ylab = "Test MSE")
points(which.min(testMSE), testMSE[which.min(testMSE)], col = 'red', pch = 19)

# d)

install.packages("splines")
library(splines)

dof <- 4
fit <- lm(nox ~ bs(dis, df = dof), data = Boston)
attr(bs(Boston$dis, df = dof), "knots")

preds <- predict(fit, list(dis = dis.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2* preds$se.fit, preds$fit - 2 * preds$se.fit)
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, preds$fit, lwd = 2, col = "red")

matlines(dis.grid, se.bands, lwd = 1, col = "red", lty = 3)

# e)

res <- c()
df.range <- 3:16
for (dof in df.range) {
  fit <- lm(nox ~ bs(dis, df = dof), data = Boston)
  res <- c(res, sum(fit$residuals ^ 2))
}
plot(df.range, res, type = 'l', xlab = 'degree of freedom', ylab = 'RSS')

# From the results, df = 10 is good enough.

# f)

res <- c()
for (dof in df.range) {
  fit <- glm(nox ~ bs(dis, df = dof), data = Boston)
  testMSE <- cv.glm(Boston, fit, K = 10)$delta[1]
  res <- c(res, testMSE)
}

plot(df.range, res, type = 'l', xlab = 'degree of freedom', ylab = 'Test MSE')
points(which.min(res) + 2, res[which.min(res)], col = 'red', pch = 19)

#### Exercise 10 (p.300, Chap.7) ####

# a)

library(ISLR)

install.packages("leaps")
library(leaps)

train <- sample(1: nrow(College), nrow(College)/2)
test <- -train
fit <- regsubsets(Outstate ~ ., data = College, subset = train, method = 'forward')
fit.summary <- summary(fit)
fit.summary

coef(fit, id = 6)

# b)

install.packages("gam")
library(gam)

gam.mod <- gam(Outstate ~ Private + s(Room.Board, 5) + s(Terminal, 5) + s(perc.alumni, 5) + s(Expend, 5) + s(Grad.Rate, 5), data = College, subset = train)
par(mfrow = c(2,3))
plot(gam.mod, se = TRUE, col = 'blue')

preds <- predict(gam.mod, College[test, ])
RSS <- sum((College[test, ]$Outstate - preds)^2) # based on equation (3.16)
TSS <- sum((College[test, ]$Outstate - mean(College[test, ]$Outstate)) ^ 2)
1 - (RSS / TSS)   # based on equation


# d) 

summary(gam.mod)






































