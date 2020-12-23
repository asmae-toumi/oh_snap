library(BART)
library(MASS)



x <- Boston[, c(6, 13)]
y <- Boston$medv
head(cbind(x, y))

set.seed(99)
nd <- 200
burn <- 50
post <- wbart(x, y, nskip = burn, ndpost = nd)


n <- length(y)
set.seed(14)
i <- sample(1:n, floor(0.75 * n))
xtrain <- x[i, ]; ytrain = y[i]
xtest <- x[-i, ]; ytest = y[-i]
cat("training sample size = ", length(ytrain), "\n")
cat("testing sample size = ", length(ytest), "\n")


set.seed(99)


x.train <- as.matrix(Boston[i, -14])