# Generates artificial threads with models of Gomez 2010 and Gomez 2013
# author: Alberto Lumbreras
################################

library(igraph)
source('R/plotting.r')

gen.thread <- function(n=100, alpha.root=1, alpha.c = 1, beta.root = 1){
  # Generate synthetic thread according to Gomez 2010
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

if(TRUE){
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
  
  par(mfrow=c(2,2))
  for (i in 1:4){
    g <- gen.thread(n=100, alpha.root=alpha.root, alpha.c=alpha.c, beta.root=beta.root)
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