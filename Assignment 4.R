
install.packages("kernlab")
library(kernlab)

####  Exercise 1 (p.368, Chap.9) ####
#####################################

x1 <- -10:10
x2 <- 1 + 3 * x1
plot(x1, x2, type = "l", col = "green")
text(c(0), c(-20), "Greater than 0", col = "green")
text(c(0), c(20), "Less than 0", col = "green")
lines(x1, 1 - x1/2, col = "red")
text(c(0), c(-15), "Less than 0", col = "red")
text(c(0), c(15), "Greater than 0", col = "red")


####  Exercise 2 (p.368, Chap.9) ####
#####################################

### a)

plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)

### b)

plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
text(c(-1), c(2), "< 4")
text(c(-4), c(2), "> 4")

### c) 

plot(c(0, -1, 2, 3), c(0, 1, 2, 8), col = c("blue", "red", "blue", "blue"), 
     type = "p", asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)

####  Exercise 4 (p.368, Chap.9) ####
#####################################

# Non-linear separation between 2 classes

install.packages("e1071")
library(e1071)

set.seed(1)
x <- rnorm(100)
y <- 4 * x^2 + 1 + rnorm(100)
class <- sample(100, 50)
y[class] <- y[class] + 3
y[-class] <- y[-class] - 3
plot(x[class], y[class], col = "red", xlab = "X", ylab = "Y", ylim = c(-6, 30))
points(x[-class], y[-class], col = "blue")

# Support vector classifier on training data

z <- rep(-1, 100)
z[class] <- 1
data <- data.frame(x = x, y = y, z = as.factor(z))
train <- sample(100, 50)
training <- data[train, ]
testing <- data[-train, ]
svm.linear <- svm(z ~ ., data = training, kernel = "linear", cost = 10)
plot(svm.linear, training)
table(predict = predict(svm.linear, training), truth = training$z)

# Support vector machine with a polynomial kernel

svm.poly <- svm(z ~ ., data = training, kernel = "polynomial", cost = 10)
plot(svm.poly, training)
table(predict = predict(svm.poly, training), truth = training$z)

# Vector machine with a radial kernel and gamma of 1

svm.radial <- svm(z ~ ., data = training, kernel = "radial", gamma = 1, cost = 10)
plot(svm.radial, training)
table(predict = predict(svm.radial, training), truth = training$z)

# Application of models on test data

# SVM linear

plot(svm.linear, testing)
table(predict = predict(svm.linear, testing), truth = testing$z)

# SVM polinomial

plot(svm.poly, testing)
table(predict = predict(svm.poly, testing), truth = testing$z)

# SVM radial

plot(svm.radial, testing)
table(predict = predict(svm.radial, testing), truth = testing$z)

####  Exercise 7 (p.371, Chap.9) ####
#####################################

## a)

install.packages("ISLR")
library(ISLR)
var <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
Auto$mpglevel <- as.factor(var)

## b)

set.seed(1)
tune.out <- tune(svm, mpglevel ~ ., data = Auto, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)
# The best parameter is "cost 1"

## c)

# Radial

set.seed(1)
tune.out <- tune(svm, mpglevel ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 150), gamma = c(0.01, 0.1, 1, 5, 10, 150)))
summary(tune.out)
# Best parameter are "cost 150" and "gamma 0.01"

# Polynomial

set.seed(1)
tune.out <- tune(svm, mpglevel ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 150), degree = c(2, 3, 4)))
summary(tune.out)
# Best parameter are "cost 100" and "degree 2"

## d)

svm.linear <- svm(mpglevel ~ ., data = Auto, kernel = "linear", cost = 1)
svm.poly <- svm(mpglevel ~ ., data = Auto, kernel = "polynomial", cost = 150, degree = 2)
svm.radial <- svm(mpglevel ~ ., data = Auto, kernel = "radial", cost = 150, gamma = 0.01)
plotpairs = function(fit) {
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
    plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
  }
}
plotpairs(svm.linear)


























