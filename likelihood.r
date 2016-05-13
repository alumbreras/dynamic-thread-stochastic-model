# Compute likelihood of trees according to generative model and parameters
# author: Alberto Lumbreras
###########################################################################

library(dplyr)

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
      probs <- alpha*popularities #+ b + tau^lags
      probs <- probs/sum(probs)
      like <- like + log(probs[parents[t]])
      cat('\n post t:', t, ' like: ', log(probs[parents[t]]))
    }
  }
  like/length(trees)
}

likelihood.Gomez2013.df <- function(df.trees, alpha = 1, beta = 1, tau = 0.75){
  # Likelihood using dataframe instead of list of trees
  like <- 0
  for (i in 1:nrow(df.trees)){
    values <- df.trees[i,] 
    num <- alpha * values$popularity #+ ifelse(values$parent==1, beta, 0) + values$lag
    
    t <- values$t
    den <- 2*alpha*(t-1)  # + beta + tau*(tau^t-1)/(tau-1)
    like <- like + log(num) - log(den)
    cat('\n post df t:', t, ' like: ', log(num) - log(den))
  }
  like
}

likelihood.Lumbreras2016.deprecated <- function(trees, z=c(1,2,3), alphas = c(1,2,3), betas = c(1,2,3), taus = c(0.25, 0.5, 0.75)){
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


tree.to.data <- function(g, thread=0){
  # Creates a dataframe with a row per post
  # and columns "degree of parent", "is_parent_root", "lag to parent", and "t"
  # With the model parameters, the three first columns are used to compute the numerator the likelihood
  # and 't' to compute the denominator of the likelihood
  parents <- get.edgelist(g)[,2] # parents vector without the first two posts
  authors <- V(g)$user[-1] # remove first post
  popularities <- c(1,sapply(2:length(parents), function(t) 1 + sum(parents[1:(t-1)]==parents[t])))
  popularities[parents==1] <- popularities[parents==1]-1 # the root node has no parent
  posts <- 2:(length(parents)+1)
  data <- data.frame(thread = rep(thread, length(posts)),
                     user = authors,
                     post = posts,
                     t = 1:(length(parents)),
                     parent = parents,
                     popularity = popularities)
          
  data <- mutate(data, lag=t-parent+1)
  data
}

if(FALSE){
  load('trees.Rda')
  alpha <- 1 # 1
  beta <- 0.68 # 0.68
  tau <- 0.75 # 0.75
  
  # TODO: these two should give exactly the same likelihood
  like <- likelihood.Gomez2013(trees, alpha = alpha, beta = beta, tau = tau)
  cat('\n alpha: ', alpha)
  cat('\n beta: ', beta)
  cat('\n tau:', tau)
  cat('\n Total likelihood: ', like)
  
  df.trees_ <- filter(df.trees, t>1)
  like <- likelihood.Gomez2013.df(df.trees_, alpha = alpha, beta = beta, tau = tau)
  cat('\n alpha: ', alpha)
  cat('\n beta: ', beta)
  cat('\n tau:', tau)
  cat('\n Total likelihood: ', like)
  
}

if(TRUE){  
  #data <- apply(trees[[1]], tree.to.data)
  #data <- lapply(trees[[1]], tree.to.data)
  
  # Prepare all the information needed into a nice dataframe
  library(parallel)
  ncores <- detectCores() - 2
  cl<-makeCluster(ncores, outfile="", port=11439)
  clusterEvalQ(cl, {library(igraph); library(dplyr)})
  data.parlapply <- parLapply(cl, trees, tree.to.data)
  stopCluster(cl)
  
  # join results adding thread id
  for (i in 1:length(data.parlapply)){
    data.parlapply[[i]]$thread <- i
  }
  data.parlapply <- rbindlist(data.parlapply)
  df.trees <- data.frame(thread = data.parlapply$thread, 
                         user = data.parlapply$user,
                         post = data.parlapply$post,
                         t = data.parlapply$t,
                         parent = data.parlapply$parent,
                         popularity = data.parlapply$popularity,
                         lag = data.parlapply$lag)
  
  save(df.trees, file='df.trees.Rda')
  
  like <- likelihood.Lumbreras2016.deprecated(trees, z = z, alphas = alphas, betas = betas, taus = taus)
  cat('\n Total likelihood: ', like)
  
  like <- likelihood.Lumbreras2016.deprecated(df.trees, z = z, alphas = alphas, betas = betas, taus = taus)
  cat('\n Total likelihood: ', like)
}

if(FALSE){  
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


