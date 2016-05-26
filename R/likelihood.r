# Compute likelihood of trees according to generative model and parameters
# author: Alberto Lumbreras
###########################################################################

library(dplyr)
library(data.table)
library(igraph)
# Compute degrees only once for all trees (3-dimensional array: node, time, tree)
#all.degrees <- list()
#for (i in 1:length(trees)){
#  parents <- get.edgelist(trees[[i]])[,2]
#  size <- length(parents)+1
#  all.degrees[[i]] <- t(sapply(2:size, function(t) tabulate(parents[1:t], nbins=size)))
#}

likelihood.Gomez2013 <- function(trees, alpha=1, beta = 1, tau = 0.75){
  # Compute the likelihood of the whole set of trees
  # Arguments:
  #   trees: observed list of trees
  #   alpha.root, alpha.c, beta.root: parameters of the model
  like <- 0
  for (i in 1:length(trees)){
    g <- trees[[i]]
    parents <- get.edgelist(g)[,2] # parents vector
    
    # skip root and first post
    for(t in 2:length(parents)){
      b <- rep(0,t)
      b[1] <- beta
      lags <- t:1
      popularities <- 1 + tabulate(parents[1:(t-1)], nbins=t)
      popularities[1] <- popularities[1] - 1 # root has no parent 
      probs <- alpha*popularities + b + tau^lags
      probs <- probs/sum(probs)
      like <- like + log(probs[parents[t]])
    }
  }
  like
}

# Likelihood computation using the dataframe
likelihood.post <- function(row, alpha, beta, tau){
  log(alpha * row['popularity'] + beta*(row['parent']==1) + tau^row['lag']) - 
  log(2*alpha*(row['t']-1)   + beta + tau*(tau^row['t']-1)/(tau-1))
}

likelihood.Gomez2013.df <- function(df.trees, alpha = 1, beta = 1, tau = 0.75){
  # Likelihood using dataframe instead of list of trees
  sum(apply(df.trees, 1, likelihood.post, alpha, beta, tau))
}

likelihood.Lumbreras2016.users <- function(df.trees, users, alpha, beta, tau){
  # Compute the likelihood of the user posts given the parameters alpha, beta and tau
  mask <- df.trees$user %in% users
  sum(apply(df.trees[mask,], 1, likelihood.post, alpha, beta, tau))
}  


if(FALSE){
  
  alpha <- 1 # 1
  beta <- 0.68 # 0.68
  tau <- 0.75 # 0.75
  
  # Classic 
  like <- likelihood.Gomez2013(trees, alpha = alpha, beta = beta, tau = tau)
  cat('\n alpha: ', alpha)
  cat('\n beta: ', beta)
  cat('\n tau:', tau)
  cat('\n Total likelihood: ', like)
  
  # Using preprocessed dataframes
  df.trees_ <- filter(df.trees, t>1)
  like <- likelihood.Gomez2013.df(df.trees_, alpha = alpha, beta = beta, tau = tau)
  cat('\n alpha: ', alpha)
  cat('\n beta: ', beta)
  cat('\n tau:', tau)
  cat('\n Total likelihood: ', like)
  
  # Using preprocessed dataframes + cluster by cluster
  alphas <- c(1,2)
  betas <- c(0.68,0.75) 
  taus <- c(0.75, 0.75)
  users <- unique(df.trees$user)
  z <- c(1,1)
  
  # this is parallelizable
  like <- 0
  z.nonempty <- unique(z)
  for (i in 1:length(z.nonempty)){
    like <- like + likelihood.Lumbreras2016.users(df.trees_, 
                                                  which(z==z.nonempty[i]), 
                                                  alphas[z.nonempty[i]], 
                                                  betas[z.nonempty[i]], 
                                                  taus[z.nonempty[i]])
  }
  cat('\n Total likelihood: ', like)
  
  library(rbenchmark)
  benchmark(likelihood.Gomez2013(trees, alpha = alpha, beta = beta, tau = tau),
            likelihood.Gomez2013.df(df.trees_, alpha = alpha, beta = beta, tau = tau),
            likelihood.Lumbreras2016.users(df.trees_, 
                                           which(z==z.nonempty[1]), 
                                           alphas[z.nonempty[1]], 
                                           betas[z.nonempty[1]], 
                                           taus[z.nonempty[1]]))
  
}

