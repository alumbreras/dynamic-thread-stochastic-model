# Compute likelihood of trees according to generative model and parameters
# author: Alberto Lumbreras
###########################################################################

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
      probs <- alpha*popularities + b + tau^lags
      probs <- probs/sum(probs)
      like <- like + log(probs[parents[t]])
    }
  }
  like/length(trees)
}

likelihood.Lumbreras2016 <- function(trees, z=c(1,2,3), alphas = c(1,2,3), betas = c(1,2,3), taus = c(0.25, 0.5, 0.75)){
  # Compute the likelihood, from the whole set of trees, of posts writen
  # by users who belong to cluster k (z_i==k)
  # (each vertex must have an attribute user so that we can check, in z,
  # whether the user has been assigned to z)
  like <- 0
  for (i in 1:length(trees)){
    g <- trees[[i]]
    parents <- get.edgelist(g)[,2] # parents vector
    authors <- V(g)$user[-1] # remove first user to be aligned with parents vector
    
    # skip root and first post
    for(t in 2:length(parents)){
      
      # Recover parameters of user's cluster
      cluster <- z[authors[t]]
      alpha <- alphas[cluster]
      beta <- betas[cluster]
      tau <- taus[cluster]
      
      b <- rep(0,t)
      b[1] <- beta
      lags <- t:1
      popularities <- 1 + tabulate(parents[1:(t-1)], nbins=t)
      probs <- alpha*popularities + b + tau^lags
      probs <- probs/sum(probs)
      like <- like + log(probs[parents[t]])
    }
  }
  like/length(trees)
}

likelihood.Lumbreras2016.user <- function(user, trees, alpha, beta, tau){
  # Compute the likelihood of the user posts given the parameters alpha, beta and tau
  like <- 0
  for (i in 1:length(trees)){
    g <- trees[[i]]
    parents <- get.edgelist(g)[,2] # parents vector
    authors <- V(g)$user[-1] # remove first user (root) to be aligned with parents vector
    posts <- which(authors==user) 
    posts <- posts[posts!=1]   # ignore second post as usual
    if(length(posts)==0) next
    
    # skip root and first post
    for(i in 1:length(posts)){
      t <- posts[i]
      
      b <- rep(0,t)
      b[1] <- beta
      lags <- t:1
      popularities <- 1 + tabulate(parents[1:(t-1)], nbins=t)
      probs <- alpha*popularities + b + tau^lags
      probs <- probs/sum(probs)
      like <- like + log(probs[parents[t]])
    }
  }
  like/length(trees)
}  


if(FALSE){
  load('trees.Rda')
  alpha <- 1 # 1
  beta <- 0.68 # 0.68
  tau <- 0.75 # 0.75
  
  like <- likelihood.Gomez2013(trees, alpha = alpha, beta = beta, tau = tau)
  cat('\n alpha: ', alpha)
  cat('\n beta: ', beta)
  cat('\n tau:', tau)
  cat('\n Total likelihood: ', like)
}

if(FALSE){  

  set.seed(1)
  alphas <- c(0.1,2)
  betas <- c(0.1,2)
  taus <- c(0.2, 0.8)
  z <- c(2,1,2,1,2,1)

  alphas <- c(0.1,2)
  betas <- c(0.1,2)
  taus <- c(0.2, 0.8)
  z <- c(1,2,1,2,1,2)
  
  
  like <- likelihood.Lumbreras2016(trees, z = z, alphas = alphas, betas = betas, taus = taus)
  cat('\n Total likelihood: ', like)
}

if(TRUE){  
  set.seed(1)

  alpha <- 0.9
  beta <- 10
  tau <- 0.9
  like <- likelihood.Lumbreras2016.user(2, trees, alpha, beta, tau)
  cat('\n Total likelihood: ', like)
}


if(FALSE){
  library(neldermead)
  
  cost.function <- function(x){
    -likelihood.Gomez2013(trees, alpha=x[1], beta=x[2], tau=x[3]) 
  }
   
  load('trees.Rda')
  sol <- fminsearch(fun = cost.function, x0 = c(0.5,0.5,0.5), verbose=FALSE)
}


