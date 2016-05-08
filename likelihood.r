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

if(TRUE){
  load('trees.Rda')
  alpha <- 1 # 1
  beta <- 0.68 # 0.68
  tau <- 0.75 # 0.75
  
  like <- likelihood.Gomez2013(trees, alpha=alpha, beta = beta, tau = tau)
  cat('\n alpha: ', alpha)
  cat('\n beta: ', beta)
  cat('\n tau:', tau)
  cat('\n Total likelihood: ', like)
}