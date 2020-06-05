## Mini-batch stochastic gradient descent implementation for squared error loss
## Spencer Braun
## 20200605

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse)
pacman::p_load(here)
pacman::p_load(docstring)


mini_batches <- function(y, X, B, seed=123) {
  #' Mini-batch Generator
  #'
  #' Produces minibatches of size B in a list
  #' @param y Vector of response value in dataset
  #' @param X Matrix of predictor values in dataset
  #' @param B Mini-batch size, eg. 64
  #' @param seed Random seed for reproducibility

  set.seed(seed)

  N <- nrow(X)
  rows <- sample(N)
  X.shuf <- X[rows, ,drop=FALSE]
  y.shuf <- y[rows, ,drop=FALSE]

  mb_mod <- ifelse(N %% B > 0, 1, 0)
  complete_mbs <- floor(N / B)

  mbs <- vector("list", complete_mbs + mb_mod)

  for (k in 0:(complete_mbs-1)) {
    X.mb <- X.shuf[(k*B+1):((k+1)*B),,drop=FALSE]
    y.mb <- y.shuf[(k*B+1):((k+1)*B),,drop=FALSE]
    mbs[[k+1]] <- cbind(y.mb, X.mb)
  }
  if (mb_mod > 0) {
    X.mb <- X.shuf[(N-(N - B * complete_mbs) + 1):N,,drop=FALSE]
    y.mb <- y.shuf[(N-(N - B * complete_mbs) + 1):N,,drop=FALSE]
    mbs[[length(mbs)]] <- cbind(y.mb, X.mb)
  }
  mbs
}


mb_grad_descent <- function(y, X, B, epochs=100, lr=0.01, verbose=TRUE) {
  #' Mini-batch Gradient Descent
  #'
  #' Example function to executre minibatch gradient descent using
  #' squared error loss on a linear model
  #' @param y Vector of response value in dataset
  #' @param X Matrix of predictor values in dataset
  #' @param B Mini-batch size, eg. 64
  #' @param epochs Number of epochs to run descent
  #' @param lr Learning rate
  #' @param verbose Print cost value for every epoch

  seed <- 123
  N <- nrow(X)
  n <- ncol(X)

  mb_mod <- ifelse(N %% B > 0, 1, 0)
  complete_mbs <- floor(N / B)
  costs <- matrix(NA, complete_mbs + mb_mod, epochs)

  set.seed(seed)
  w <- matrix(rnorm(n, 0,1), n, 1)

  for (i in 1:epochs) {
    seed <- seed + 1
    mbs <- mini_batches(y, X, B, seed)
    epoch_cost <- 0

    j <- 0
    for (batch in mbs) {

      j <- j + 1
      y.mb <- batch[,1, drop=FALSE]
      X.mb <- batch[,2:ncol(batch),drop=FALSE]

      y_hat <- X.mb %*% w

      loss <- (1/nrow(X.mb))*sum((y.mb - X.mb %*% w)^2)
      costs[j, i] <- loss
      epoch_cost <- epoch_cost + loss

      dw <- (-2/nrow(X.mb))*(t(X.mb) %*% (y.mb - y_hat))
      w <- w - lr * dw
    }
    if (verbose) {
      print(str_interp("Cost at epoch ${i}: ${round(epoch_cost / N,3)}"))
    }
  }
  list('costs' = costs, 'weights' = w)
}



##### Example Usage #####

set.seed(123)
N <- 100
n <- 10
sigma <- 0.01
epochs <- 10
lr <- 0.01

X <- matrix(rnorm(N*n, 0,1), N, n)
a <- matrix(rnorm(n, 0,1), n, 1)
d <- matrix(rnorm(N^2, 0, sigma^2), N, 1)

y <- X %*% a + d

run_1 <- mb_grad_descent(y, X, 1, epochs=epochs, lr = lr, verbose=FALSE)
run_5 <- mb_grad_descent(y, X, 5, epochs=epochs, lr = lr, verbose=FALSE)
run_10 <- mb_grad_descent(y, X, 10, epochs=epochs, lr = lr, verbose=FALSE)

cost_df <- data.frame(
  "epoch" = 1:ncol(run_1$costs),
  "B.1" = apply(run_1$costs, 2, mean),
  "B.5" = apply(run_5$costs, 2, mean),
  "B.10" = apply(run_10$costs, 2, mean)) %>%
  pivot_longer(-epoch, names_to = "Batch_Size", values_to = "Loss")

cost_df %>%
  ggplot() +
  geom_line(aes(x=epoch, y=Loss, colour = factor(Batch_Size))) +
  labs(x="Epoch", y="Loss", title="Loss by Epoch and Batch Size",
       colour = "Batch Size") +
  theme_minimal()
