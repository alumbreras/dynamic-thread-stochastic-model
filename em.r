# Expectation-Maximizaton to fins clusters and alpha, beta, tau parameters 
# of each cluster
#
# author: Alberto Lumbreras
library(neldermead)
library(data.table)
library(neldermead)

cost.function <- function(alpha, beta, tau){
}


update_responsabilities <- function(df.trees, u, alphas, betas, taus, pis){
  # Compute E(z_uk) over the posterior distribution p(z_uk | X, theta)
  
  K <- length(alphas) # number of clusters
  Xu <- filter(df.trees, user==u) # all posts from user
  
  logfactors <- rep(0,K)
  for (i in 1:K){
    logfactors[i] <- log(pis[i]) + sum(apply(Xu, 1, likelihood.post, alphas[i], betas[i], taus[i]))
  }
  logfactors <- logfactors - min(logfactors)
  denominator <- sum(exp(logfactors))
  responsabilities_u <- exp(logfactors)/denominator
  responsabilities_u
}

update_pis <- function(responsabilities){
  # Compute pi_k
  pis <- rep(0,ncol(responsabilities))
  for(k in 1:ncol(responsabilities)){
    pis[k] <- sum(responsabilities[,k])/nrow(responsabilities)
  }
  pis
}

# Likelihood computation using the dataframe
likelihood.post <- function(row, alpha, beta, tau){
  log(alpha * row['popularity'] + beta*(row['parent']==1) + tau^row['lag']) - 
    log(2*alpha*(row['t']-1)   + beta + tau*(tau^row['t']-1)/(tau-1))
}

Qopt <- function(df.trees, responsabilities_k, pi_k, alpha, beta, tau){
  # sum of E[lnp(X,Z|\theta)] likelihoods for all clusters and all users
  # given the current responsabilities
  # Note that the optimizations can be done separatedly
  total <- 0
  for(i in 1:nrow(df.trees)){
    row <- df.trees[i,]
    u <- row$user
    #like.post <- log(alpha * row['popularity'] + beta*(row['parent']==1) + tau^row['lag']) - 
    #              log(2*alpha*(row['t']-1)   + beta + tau*(tau^row['t']-1)/(tau-1))
    total <- total + c(as.matrix(responsabilities_k[u]*(likelihood.post(row, alpha, beta, tau) + log(pi_k))))
  }
  total
}

# Wrapper for the optimization function
# so that the only parameters are the ones to optimize
cost.function <- function(params){
  responsabilities_k <- responsabilities[,kk]
  pi_k <- pis[kk]
  -Qopt(df.trees, responsabilities_k, pi_k, alpha=params[1], beta=params[2], tau=params[3])
}


em <-function(df.trees, alphas, betas, taus){
  # Expectation-Maximizationo to find groups of users with different
  # alpha, beta, tau parameters
  # Arguments:
  #   trees: a list of igraph objects representing discussion threads
  #   alphas, betas, taus: initial assignment of users to clusters
  nclusters <- length(alphas)
  users <- unique(df.trees$user)
  U <- length(users)
  responsabilities <- matrix(nrow = U, ncol = nclusters)
  
  pis <- rep(1/ncol(responsabilities), ncol(responsabilities))

  # EXPECTATION
  # Given the parameters of each cluster, find the best cluster for 
  # each user (hard assignments, a la k-means, for now)
  #################################################################
  #z <- sample(nclusters, length(users), replace=TRUE)
  z <- c(1,2)
  for (u in 1:U){
    responsabilities[u,] <- update_responsabilities(df.trees, u, alphas, betas, taus, pis)  
  }
  
  pis <- update_pis(responsabilities)
  
  # MAXIMIZATION
  # Given the current responsabilities and pis, find the best parameters for each cluster
  ################################################################
  for(k in 1:nclusters){
    kk <- k
    sol <- fminsearch(fun = cost.function, x0 = c(alphas[kk],betas[kk],taus[kk]), verbose=TRUE)
  }
  
  
  ###############################################################
  # EVALUATION OF FULL LIKELIHOOD p(X, Z | \theta)
  ###############################################################
}

if(TRUE){
  load('trees.Rda')

  alphas <- c(1, 1.5, 2)
  betas <- c(0, 1, 2)
  taus <- c(0.25, 0.5, 0.75)
  
  df.trees <- filter(df.trees, t>2)
  # Init the parameters rather than the z 
  em(df.trees, alphas, betas, taus)
}