## Forward stepwise regression via successive orthogonalization
## Spencer Braun
## 20200212


library(tidyverse)
library(reshape2)
library(ggplot2)
library(here)




colSelect <- function(X, index) {
  #' Helper func to filter columns of X matrix

  if (is.null(index)) {
    return(matrix(0, nrow(X), 1))
  } else {
    return(as.matrix(X[, index]))
  }
}


orthogonalize <- function(X, Z, k) {
  #' Orthogonalize predictors relative to predictors already in X.S
  fit <- lm(X ~ Z[,1:k] + 0)
  return(list("residuals" = as.matrix(fit$residuals),
              "coefficients" = fit$coefficients)
  )
}


betaK <- function(y, rs) {
  fit <- lm(y ~ rs + 0)
  rss <- sum(fit$residuals^2)

  return(c(fit$coefficients, rss))
}


runModel <- function(X, y, S, newPredictor) {
  if (is.null(S)) {
    fit <- lm(y ~ X[,newPredictor])
  } else {
    fit <- lm(y ~ colSelect(X, S) + X[,newPredictor])
  }
  return(fit$coefficients)
}


findNextBestPredictor  <- function(X, y, S, Sc, Z, R, k) {
  #' Find next best predictor after adjusting for predictors already selected
  #' in X.S. Criteria is minimizing RSS

  X.Sc <- colSelect(X, Sc)
  X.S <- colSelect(X, S)
  adjustedX <- orthogonalize(X.Sc, Z, k)

  compareFits <- apply(adjustedX$residuals, 2, partial(betaK, y=y))
  rssMinimizer <- which.min(compareFits[2,])
  bestPredictor <- Sc[rssMinimizer]
  rCoefficients <- as.matrix(adjustedX$coefficients)[,rssMinimizer]

  return(list("predictor" = bestPredictor,
              "residuals" = adjustedX$residuals[,rssMinimizer],
              "coefficient" = compareFits[1, rssMinimizer],
              "rcoefficients" = rCoefficients)
  )
}


forwardStepwise <- function(X, y) {
  #' Wrapper function to find p+1 models via forward subset selection
  X <- as.matrix(X)
  y <- as.matrix(y)
  beta <- matrix(0, ncol(X)+1, ncol(X)+1)
  beta[1,] <- mean(y)

  stepCoefs <- matrix(0, ncol(X)+1, ncol(X)+1)
  stepCoefs[1,1] <- mean(y)

  S <- c()
  Sc <- seq(1, ncol(X))

  Z <- matrix(NA, nrow(X), ncol(X) + 1)
  Z[,1] <- rep(1, nrow(X))

  R <- matrix(0, ncol(X)+1, ncol(X)+1)
  R[1, 1] <- 1

  predictorOrder <- rep(0, ncol(X))
  k <- 1
  for (k in 1: ncol(X)) {
    nextPred <- findNextBestPredictor(X, y, S, Sc, Z, R, k)
    Z[,k+1] <- nextPred$residuals

    R[k + 1, 1:(k + 1)] <- c(-1*nextPred$rcoefficients, 1)

    beta[k+1, (k+1):ncol(beta)] <- nextPred$coefficient

    fitCoefs <- runModel(X, y, S, nextPred$predictor)
    stepCoefs[seq(1, k+1), k+1] <- as.vector(fitCoefs)

    predictorOrder[k] <- nextPred$predictor
    S <- append(S, nextPred$predictor)
    Sc <- setdiff(Sc, S)

  }

  #realCoefficients <- t(R) %*% beta

  return(list("coefs"=stepCoefs,
              "order"=predictorOrder,
              "R"=R,
              "orthogonalCoefs"=beta))
}


kPredictor <- function(fit.object, X0, k) {
  #' Make prediction on test data X0 using forward selected model for k
  #' predictors.

  coefs <- fit.object$coefs
  porder <- fit.object$order
  k_coefs <- coefs[2:ncol(coefs), k + 1][order(porder)]
  prediction <- as.matrix(X0) %*% k_coefs + coefs[1, k + 1]

  return(prediction)
}


forwardPredictor <- function(fit.object, X0) {
  #' Make prediction on test data X0 over all size models from forward selection
  k_vector <- seq(0,ncol(X0))
  sapply(k_vector, partial(kPredictor, fit.object=fit.object, X0=X0))
}




#### Run on Spam dataset ###

set.seed(123)
spam <- read.csv("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data",
                 sep= ' ', header=FALSE)

test_ind <- ifelse(as.vector(
  read.csv("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.traintest",
           header=FALSE)) == 1, TRUE, FALSE)

spamy.train <- as.matrix(spam[!test_ind ,ncol(spam)])
spamy.test <- as.matrix(spam[test_ind ,ncol(spam)])

spam_x.raw <- spam[,1:(ncol(spam) - 1)]
spam_x <- cbind(
  ifelse(spam_x.raw[,1:(ncol(spam_x.raw) - 3)] == 0, 0 , 1),
  log(spam_x.raw[,(ncol(spam_x.raw) - 2):ncol(spam_x.raw)])
)
spamx.test <- spam_x[test_ind, ]
spamx.train <- spam_x[!test_ind,]



# Fit forward selection models on spam training data
fit.spam <- forwardStepwise(spamx.train, spamy.train)

# Predict for spam test data for all forward selected models of sizes 1 - p+1
predict.spam <- forwardPredictor(fit.spam, spamx.test)

rss <- apply((predict.spam - as.vector(spamy.test))^2, 2, sum)

data.frame(Step = seq(0,length(rss)-1), rss) %>%
  ggplot(aes(x=Step, y = rss)) +
  geom_point() + geom_line() +
  ggtitle("Number of Predictors vs. Test RSS") +
  ylab("RSS") + theme_bw()


misclass.rate <- function(prediction, ytest) {
  pred.binary <- ifelse(prediction > 0.5, 1, 0)
  apply(pred.binary != as.vector(ytest), 2, mean)
}

missclassByStep <- data.frame(
  Step = seq(0,length(rss)-1),
  misclass = misclass.rate(predict.spam, spamy.test)
)

missclassByStep %>% ggplot(aes(x=Step, y = misclass)) +
  geom_point() + geom_line() +
  ggtitle("Number of Predictors vs. Test Misclassification Rate") +
  ylab("Misclassification Rate") + theme_bw()



set.seed(123)
folds <- sample(1:10, nrow(spamx.train), replace=TRUE)
cv.errors=matrix(NA, 10, ncol(spamx.test)+1)

for(j in 1:10){
  best.spam <- forwardStepwise(spamx.train[folds!=j,], spamy.train[folds!=j])
  pred <- forwardPredictor(best.spam, spamx.train[folds == j,])
  cv.errors[j,] <- misclass.rate(pred, spamy.train[folds == j])
}

cv_means <- apply(cv.errors, 2, mean)
best_step <- as.character(which.min(cv_means))
print(best_step)


cv_select <- dplyr::filter(missclassByStep, Step==best_step)

missclassByStep %>% ggplot(aes(x=Step, y = misclass)) +
  geom_point() + geom_line() +
  geom_point(data=cv_select, aes(x=Step, y = misclass), color="red", size=1) +
  geom_vline(xintercept=as.numeric(best_step), linetype="dashed", color = "red") +
  ggtitle("Step vs. Misclassification Rate, CV Selected Step in Red") +
  ylab("Misclassification Rate") + theme_bw()


print(str_interp("Coefficient Order"))
for (i in 0:5) {
  print(fit.spam$order[(i*10):((i+1)*10-1)])
}
matplot(
  t(fit.spam$coefs), type='l',
  main = "Coefficient Values by Step Number", xlab="Step",
  ylab="Coefficient Value", col=fit.spam$order)

rsqMatrix <- function(fit.object, X, y) {
  Xmat <- as.matrix(X)
  y_hat <- forwardPredictor(fit.object, Xmat)

  ymat <- matrix(rep(y, ncol(y_hat)), ncol=ncol(y_hat))
  rss <- apply((ymat - (y_hat))^2, 2, sum)
  tss <- sum((y - mean(y))^2)
  rsq <- 1- (rss/tss)

  return(cbind(rsq, t(fit.object$coefs)))
}



r_test <- rsqMatrix(fit.spam, spamx.train, spamy.train)
rdf <- as.data.frame(r_test)
names(rdf) <- c("R2", "B0", sapply(fit.spam$order, function(x) str_interp("B${x}")))
tidy_rdf <- melt(rdf, id="R2", variable.name="Coefficient")

tidy_rdf %>% ggplot(aes(x=R2, y=value, colour=Coefficient)) +
  geom_line() +
  ggtitle("Coefficient Values by R2") + ylab("Coefficient Value") +
  theme_bw() + theme(legend.position = "none")

print("First 10 coefficients:")
print(fit.spam$order[1:10])
print((fit.spam$coefs[1:10,10]))
```