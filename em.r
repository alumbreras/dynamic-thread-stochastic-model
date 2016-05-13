# Expectation-Maximizaton to fins clusters and alpha, beta, tau parameters 
# of each cluster
#
# author: Alberto Lumbreras
library(neldermead)
library(data.table)

cost.function <- function(x){
  # users in this cluster
  users <- which(z==1)
  neglike <- 0 
  for (i in 1:length(users)){ 
    user <- users[i]
    neglike <- neglike - likelihood.Lumbreras2016.user(user, trees, alpha=x[1], beta=x[2], tau=x[3])
  }
  neglike
}

em <-function(trees, alphas, betas, taus){
  # Expectation-Maximizationo to find groups of users with different
  # alpha, beta, tau parameters
  # Arguments:
  #   trees: a list of igraph objects representing discussion threads
  #   alphas, betas, taus: initial assignment of users to clusters
  nclusters <- length(alphas)
  users <- unique(unlist(lapply(trees, function(g) V(g)$user)))
  
  # EXPECTATION
  # Given the parameters of each cluster, find the best cluster for 
  # each user (hard assignments, a la k-means, for now)
  #################################################################
  #z <- sample(nclusters, length(users), replace=TRUE)
  z <- c(1,1,2,2,3,3)
  
  # MAXIMIZATION
  # Given the current z, find the best parameters for each cluster
  ################################################################
  # maximize cluster by cluster
  for (i in 1:nclusters){
    cat('\n Maximizing cluster ', i)
    sol <- fminsearch(fun = cost.function, x0 = c(0.5,0.5,0.5), verbose=FALSE)
  }
}

if(TRUE){
  load('trees.Rda')

  alphas <- c(1, 1.5, 2)
  betas <- c(0, 1, 2)
  taus <- c(0.25, 0.5, 0.75)
  
  # Init the parameters rather than the z 
  em(trees, alphas, betas, taus)
}