# Generates artificial threads with models of Gomez 2010 and Gomez 2013
# author: Alberto Lumbreras
################################
library(igraph)
source('R/plotting.r')

gen.thread.Gomez2011 <- function(n=100, alpha.root=1, alpha.c = 1, beta.root = 1){
  # Generate synthetic thread according to Gomez 2011
  # Args:
  #    n: number of posts
  #    alpha:  preferential attachement exponent
  #    beta: root bias
  g <- graph.empty(n=1)
  
  # First post has no choice
  g <- add_vertices(g, 1)
  g <- add_edges(g, c(2,1))
  
  for (i in 3:n){
    alphas <- c(alpha.root, rep(alpha.c, i-2))
    betas <- c(beta.root, rep(1, i-2))
    popularities <- 1 + degree(g, mode="in")
    
    # Probability of choosing every node (only one is chosen)
    probs <- (betas*popularities)^alphas
    probs <- probs/sum(probs)
    j <- sample(1:length(probs), 1, prob=probs)
    
    # Add new vertex attached to the chosen node
    g <- add_vertices(g, 1)
    g <- add_edges(g, c(i,j))
  } 
  g
}


gen.thread.Gomez2013 <- function(n=100, alpha=1, beta = 1, tau=0.75){
  # Generate synthetic thread according to Gomez 2011
  # Args:
  #    n: number of posts
  #    alpha:  preferential attachement exponent
  #    beta: root bias
  g <- graph.empty(n=1)
  
  # First post has no choice
  g <- add_vertices(g, 1)
  g <- add_edges(g, c(2,1))
  
  for (i in 3:n){
    betas <- c(beta, rep(0, i-2))
    lags <- (i-1):1
    popularities <- 1 + degree(g, mode="in")
    
    # Probability of choosing every node (only one is chosen)
    probs <- alpha*popularities + betas + tau^lags
    probs <- probs/sum(probs)
    j <- sample(1:length(probs), 1, prob=probs)
    
    # Add new vertex attached to the chosen node
    g <- add_vertices(g, 1)
    g <- add_edges(g, c(i,j))
  } 
  g
}

gen.thread.Lumbreras2016 <- function(n=100, z=c(1,2,3), alphas = c(1,2,3), betas = c(1,2,3), taus = c(0.25, 0.5, 0.75)){
  # Generate threads like in Gomez 2013 but from a mixture model where users
  # in one component have different parameters (alpha, beta, tau)  than users in the
  # other component
  # TODO: need to pass a z vector with user assignments?
  # then for each post chose a random user and then pick the parameters of its group
  # Args:
  #    n: number of posts
  #    alpha:  preferential attachement exponent
  #    beta: root bias
  nusers <- length(z)
  g <- graph.empty(n=1)
  
  # First post has no choice
  g <- add_vertices(g, 1)
  g <- add_edges(g, c(2,1))
  
  # the user of the two first posts is irrelevant for us
  V(g)$user <- 1 
  
  for (i in 3:n){
    # choose random user
    u <- sample(nusers, 1)
    cluster <- z[u]
    alpha <- alphas[cluster]
    beta <- betas[cluster]
    tau <- taus[cluster]
    
    bs <- c(beta, rep(0, i-2))
    lags <- (i-1):1
    popularities <- 1 + degree(g, mode="in")
    
    # Probability of choosing every node (only one is chosen)
    probs <- alpha*popularities + bs + tau^lags
    probs <- probs/sum(probs)
    
    j <- sample(1:length(probs), 1, prob=probs)
    
    # Add new vertex attached to the chosen node
    g <- add_vertices(g, 1, attr=list('user'= u))
    g <- add_edges(g, c(i,j))
  } 
  g
  # for each new vertex, choose uniformly betwmeen on the three clusters
  
}


# Generate a synthetic dataset of trees
#######################################
if(FALSE){
  n <- 100
  
  alpha.root <- 0.734
  alpha.c <- 0.683
  beta.root <- 1.302
  trees <- replicate(n,  gen.thread.Gomez2011(n=20, alpha.root=alpha.root, alpha.c=alpha.c, beta.root=beta.root), simplify = FALSE)
  
  alpha <- 1
  beta <- 0.68
  tau <- 0.75
  trees <- replicate(1000, gen.thread.Gomez2013(n=100, alpha=alpha, beta=beta, tau=tau), simplify = FALSE)

  set.seed(1)
  alphas <- c(1,2,3)
  betas <- c(1,2,3)
  taus <- c(0.25, 0.5, 0.75)
  z <- c(1,2,3,1,2,3,1,2,3,1,2,3)
  trees <- replicate(1000,  gen.thread.Lumbreras2016(n=100, z=z, alphas=alphas, betas=betas, taus=taus), simplify = FALSE)
  
  set.seed(1)
  alphas <- c(0.1,2)
  betas <- c(0.1,2)
  taus <- c(0.2, 0.8)
  z <- c(1,2,1,2,1,2)
  trees <- replicate(1000,  gen.thread.Lumbreras2016(n=100, z=z, alphas=alphas, betas=betas, taus=taus), simplify = FALSE)
    
  save(trees, file='trees.Rda')
}

# Generate and plot some synthetic tree
########################################
TEST <- FALSE
if(TEST){
  # Values reported in Gomze 2010 (Table 2)
  # Slashdot: 
  alpha.root <- 0.734
  alpha.c <- 0.683
  beta.root <- 1.302
  
  # Barrapunto
  alpha.root <- 0.665
  alpha.c <- -0.116
  beta.root <- 0.781
  
  # Meneame
  alpha.root <- 0.856
  alpha.c <- 0.196
  beta.root <- 1.588
  
  # Wikipedia
  alpha.root <- 0.884
  alpha.c <- -1.684
  beta.root <- 0.794
  
  trees <- replicate(4,  gen.thread.Gomez2011(n=100, alpha.root=alpha.root, alpha.c=alpha.c, beta.root=beta.root), simplify = FALSE)
  
  
  alpha <- 1
  beta <- 0.68
  tau <- 0.75
  trees <- replicate(4,  gen.thread.Gomez2013(n=100, alpha=alpha, beta=beta, tau=tau), simplify = FALSE)
  
  trees <- replicate(4,  gen.thread.Lumbreras2016(n=100), simplify = FALSE)
  
  par(mfrow=c(2,2))
  for (i in 1:length(trees)){
    g <- trees[[i]]
    root = which(degree(g, mode='out')==0)
    V(g)$color <- 'black'
    V(g)$color[root] <- 'red'
    V(g)$size <- 1
    V(g)$size[root] <- 3
    g.un <- as.undirected(g)
    la = layout_with_fr(g.un)
    plot(g.un, layout = la, vertex.label = NA, edge.arrow.size=0.6)
  }
}