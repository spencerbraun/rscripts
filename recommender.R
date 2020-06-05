## Functions useful for recommendation systems
## SVD Clustering, User Clustering, Recommenderlab


library(docstring)
library(tidyverse)
library(recommenderlab)


# Clustering by SVD
# Define SVD and SVD-cluster Functions
set.seed(123)

select_k <- function(matrix, k_range) {
  #' Selecting Best K
  #'
  #' Attempts k-means clustering for a range of k values
  #' @param matrix utility matrix
  #' @param k_range vectorof k values

  wss <- rep(NA, length(k_range))

  for (i in 1:length(k_range)) {
    km.test <- kmeans(matrix, k_range[i], nstart = 20, iter.max = 30)
    wss[i] <- km.test$tot.withinss
  }

  plot(x=k_range, y=wss,
       main="Within-Cluster Sum of Squared Differences vs K")
}


dim_reduce <- function(utility_matrix, K, plot=TRUE) {
  #' Best Rank-K Matrix Approximation
  #'
  #' Uses singular value thresholding to approximate utility matrix with
  #' matrix of specified K rank. Returns US and SV' thresholded matrices
  #' @param utility_matrix utility matrix
  #' @param K max rank of thresholded matrix
  #' @param plot plot singular values in order with cutoff line at K

  utility_decomp <- svd(utility_matrix)
  sigma <- utility_decomp$d
  U <- utility_decomp$u
  V <- utility_decomp$v

  if (plot) {
    plot(sigma)
    abline(v=K)
  }

  sing.values <- sqrt(sigma[1:K])
  US.k <- U[,1:K] %*% diag(sing.values)
  SV.k <- diag(sing.values) %*% t(V[,1:K])

  list(US=US.k, SV = SV.k)
}


svd_clustered_ratings <- function(US, SV, K) {
  #' Cluster Principal Components
  #'
  #' Runs K-means on US matrix, recombines with SV
  #' @param US US portion of US^2V' SVD
  #' @param SV SV' portion of US^2V' SVD
  #' @param K number of clusters for k-means

  set.seed(123)

  km.test <- kmeans(US.k, K, nstart = 20, iter.max = 30)
  cluster_ratings <- km.test$centers %*% SV.k

  user_index <- data.frame(cluster=km.test$cluster)
  cluster_df <- as.data.frame(cluster_ratings)
  names(cluster_df) <- sapply(1:ncol(cluster_df), as.character)
  cluster_df['cluster'] <- rownames(cluster_df) %>% as.integer(.)

  user_index %>% left_join(cluster_df, by='cluster')
}


test_all_but_k <- function(k, clustered_matrix, utility_matrix_true, unit="user_id") {
  #' k-fold CV on SVD Clustered Recommendations
  #'
  #'
  #' @param k number of folds to use in CV
  #' @param clustered_matrix clustered utility matrix
  #' @param utility_matrix_true true utility matrix from observed data
  #' @param unit analysis unit, eg. users

  set.seed(123)

  precision_vector <- vector()
  recall_vector <- vector()
  rmse_vector<-vector()


  top_k_df <- clustered_matrix %>%
    pivot_longer(-c(get(unit), cluster), values_to = "rating", names_to = "item") %>%
    group_by(get(unit)) %>%
    mutate(ranking = rank(desc(rating), ties.method = 'first')) %>%
    filter(ranking <= k) %>%
    arrange(get(unit), ranking)

  ratings_matrix <- clustered_matrix %>%
    select(-c(get(unit), cluster)) %>%
    as.matrix
  userid_lookup <- clustered_matrix %>%
    pull(get(unit))

  # loop through users
  for (userindex in 1:nrow(ratings_matrix)) {
    userid <- userid_lookup[userindex]
    user <- utility_matrix_true[userindex, ]
    index_of_known <- which(user != 0)
    if (length(index_of_known) > k) {
      takeout <- sample(1:length(index_of_known), size = k)
      items_removed <- index_of_known[takeout]
      orig <- user[items_removed]
      user[items_removed] <- 0

      recommended <- top_k_df %>% filter(get(unit) == userid) %>% pull(item)
      matched <- length(intersect(items_removed, recommended))
      precision_vector <- append(precision_vector, matched / k)
      recall_vector <- append(recall_vector, (matched / (length(index_of_known))))

      user[items_removed] <- orig
      itemscores <- ratings_matrix[userindex, ]
      rmse <- sqrt(mean((user-itemscores)^2))
      rmse_vector <- append(rmse_vector,rmse)
    }
  }
  return(c("Precision" = mean(precision_vector),
           "Recall" = mean(recall_vector),
           "RMSE"=mean(rmse_vector)))
}



# Clustering by User Features

clustered_utility <- function(user_features, utility_matrix, user_ids, K, unit="user_id") {
  #' Characteristic Clustered Utility Matrix
  #'
  #' Clusters users and outputs an aggregated utility matrix by user cluster
  #' @param user_features data.frame of user features to cluster over by analysis unit (unit)
  #' @param utility_matrix utility matrix to aggregate
  #' @param user_ids user_id's in order of utility matrix index
  #' @param K number of clusters to use in K-means
  #' @param unit analysis unit, eg. users

  set.seed(123)

  if (class(utility_matrix) == "matrix") {
    utility_matrix <- data.frame(cbind(unit=user_ids, utility_matrix)) %>%
      rename_all(funs(str_replace(., "X", "")))
  }

  features.matrix <- user_features %>% select(-get(unit)) %>% as.matrix(.)
  user.cluster <- kmeans(features.matrix, K, nstart = 20, iter.max = 30)
  user_features['cluster'] <- user.cluster$cluster

  ut.units <- length(unique(utility_matrix[unit]))
  df.units <- length(unique(user_features[unit]))
  if (ut.units != df.units) {
    print(str_interp(
      "Warning: utility matrix has ${ut.users} units but clusters assigned for ${df.users}"))
  }

  cluster_utility <- utility_matrix %>%
    left_join(select(user_features, get(unit), cluster), by=unit) %>%
    filter(!is.na(cluster)) %>%
    select(-c(get(unit))) %>%
    group_by(cluster) %>%
    summarise_all(sum) %>%
    arrange(cluster) %>%
    select(-cluster) %>%
    as.matrix(.)

  list('matrix'=cluster_utility, 'clusters'=user.cluster$cluster)
}




### Grid Search

hyper_grid <- rbind(expand.grid(
  normalize = c("NULL", "center", "Z-score"),
  method = c("Cosine", "Euclidean", "Pearson"),
  algo = c("IBCF", "UBCF"),
  stringsAsFactors=FALSE
),
expand.grid(
  normalize = c("NULL", "center", "Z-score"),
  method = c("NULL"),
  algo = "POPULAR",
  stringsAsFactors=FALSE
))

hyper_grid[hyper_grid == "NULL"] <- NA


# Function to format numbers for writing to file
format_numeric <- function(x) {
  numeric_cols <- vapply(x, is.numeric, logical(1))
  x[numeric_cols] <- sapply(x[numeric_cols], as.character)
  x
}

reclab_grid_search <- function(utility_matrix, hyper_grid, given = -1, goodRating = 0.1) {
  #' Grid Search over recommenderlab Hyperparameters
  #'
  #'Given a data frame of parameter permutations, runs recommendation algorithms with
  #' parameters and collects statistics on run
  #' @param utility_matrix utility matrix to use in recommendation algorithms.
  #' Type "realRatingMatrix" or "Matrix"
  #' @param hyper_grid permutation data frame of parameters to test
  #' @param given number of observations to hold out for test set
  #' @param goodRating entry in real valued utility matrix considered "good"


  set.seed(123)

  # Converts to realRatingMatrix if needed
  if (class(utility_matrix) == "matrix") {
    utility_matrix <- utility_matrix %>%
      as("realRatingMatrix") %>%
      .[rowSums(.) > 0, ]
  }

  scheme <-
    evaluationScheme(
      utility_matrix,
      method = "split",
      train = 0.7,
      given = given,
      goodRating = goodRating
    )

  label_vec <- rep(NA, nrow(hyper_grid))
  algo_results <- tibble()
  pb <- txtProgressBar(min = 0, max = nrow(hyper_grid), style = 3)

  for (i in 1:nrow(hyper_grid)) {

    label_vec[i] <- hyper_grid %>%
      mutate(label = paste0(algo,"_",method,"_",normalize)) %>%
      pull(label) %>%
      .[i]
    algo <- hyper_grid %>% pull(algo) %>% .[i]
    if (algo == "POPULAR") {
      params <- list("normalize" = hyper_grid[i, "normalize"])
    } else {
      params <- as.list(hyper_grid[i,c(1,2)])
    }
    run_recs <- Recommender(getData(scheme, "train"), algo, param=params)

    model.pred <- predict(run_recs, getData(scheme, "known"), type = "ratings")
    error <- calcPredictionAccuracy(model.pred, getData(scheme, "unknown"))
    eval <- recommenderlab::evaluate(scheme,
                                     method=algo,
                                     type = "topNList",
                                     n=c(5),
                                     progress = FALSE)
    confusion <- getConfusionMatrix(eval)[[1]]

    model.pred_mat <- predict(run_recs, getData(scheme, "known"), type = "ratingMatrix") %>%
      as("matrix") %>% as_tibble()
    algo_results <- bind_rows(algo_results,
                              compile_results(model.pred_mat, eval, algo, error, scheme))

    rm(run_recs)

    setTxtProgressBar(pb, i)
  }

  cbind(label_vec, algo_results)
}

