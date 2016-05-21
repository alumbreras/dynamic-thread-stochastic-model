# Expectation-Maximizaton to fins clusters and alpha, beta, tau parameters 
# of each cluster
#
# author: Alberto Lumbreras
library(neldermead)
library(data.table)
library(neldermead)

cost.function <- function(alpha, beta, tau){
}


update_responsabilities <- function(df.trees, u, pis, alphas, betas, taus){
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


# Likelihood computation using the dataframe
likelihood.post <- function(row, alpha, beta, tau){
  c(as.matrix(log(alpha * row['popularity'] + beta*(row['parent']==1) + tau^row['lag']) - 
    log(2*alpha*(row['t']-1)   + beta + tau*(tau^row['t']-1)/(tau-1))))
}

Qopt <- function(df.trees, responsabilities_k, pi_k, alpha, beta, tau){
  # sum of E[lnp(X,Z|\theta)] likelihoods for all clusters and all users
  # given the current responsabilities
  # Note that the optimizations can be done separatedly
  a <- responsabilities_k[df.trees$user]
  b <- apply(df.trees, 1, function(x) likelihood.post(x, alpha, beta, tau) + log(pi_k))
  sum(a*b)
}

# Wrapper for the optimization function
# so that the only parameters are the ones to optimize
cost.function <- function(params){
  -1*Qopt(df.trees,  responsabilities[,k], pis[k], alpha=params[1], beta=params[2], tau=params[3])
}


EM <-function(df.trees, alphas, betas, taus){
  # Expectation-Maximizationo to find groups of users with different
  # alpha, beta, tau parameters
  # Arguments:
  #   trees: a list of igraph objects representing discussion threads
  #   alphas, betas, taus: initial assignment of users to clusters
  K <- length(alphas)
  users <- unique(df.trees$user)
  U <- length(users)
  
  responsabilities <- matrix(nrow = U, ncol = K)
  pis <- rep(1/ncol(responsabilities), ncol(responsabilities))

  # EXPECTATION
  # Given the parameters of each cluster, find the responsability of each user in each cluster 
  #################################################################
  # (this can be parallelizable)
  for (u in 1:U){
    responsabilities[u,] <- update_responsabilities(df.trees, u, pis, alphas, betas, taus)  
  }

  # MAXIMIZATION
  # Given the current responsabilities and pis, find the best parameters for each cluster
  ################################################################
  #(this can be parallelizable)
  for(k in 1:K){
    sol <- fminsearch(fun = cost.function, x0 = c(alphas[k],betas[k],taus[k]), verbose=TRUE)
    #alphas[k] <-
    #betas[k] <-
    #taus[k] <-
  }
  
  # Update pis
  pis <- colSums(responsabilities)/nrow(responsabilities)
  
  ###############################################################
  # EVALUATION OF FULL LIKELIHOOD p(X, Z | \theta)
  ###############################################################
}

if(TRUE){
  load('trees.Rda')

  # Initialize parameters
  alphas <- c(1, 1.5, 2)
  betas <- c(0, 1, 2)
  taus <- c(0.25, 0.5, 0.75)

  # The first two posts of every thread are trivial since they have no choice
  df.trees <- filter(df.trees, t>2)
  
  # Expectation-Maximization
  EM(df.trees, alphas, betas, taus)
}