# Generates artificial threads with models of Gomez 2010 and Gomez 2013
# author: Alberto Lumbreras
################################

library(igraph)

gen.thread <- function(n=100, alpha=0, beta=0, lambda=0){
  # Generate artifitial thread
  # Combines attractiveness to degree (preferential attachment), depth and recency.
  # Args:
  #    alpha:  preferential attachement exponent
  #    beta: chaining exponent
  #    lambda: aging exponent
  #
  # See also: sample_pa_age() in igraph 1.0. http://igraph.org/r/doc/igraph.pdf
  g <- graph.empty(n=1)
  V(g)$time[1] <- 0
  
  for (i in 2:n){
    
    # Probability pf choosing every node (only one is chosen)
    taus <- i - V(g)$time
    indegrees <- degree(g, mode="in")
    depths <- as.numeric(distances(g, v=V(g), to=1, mode="out"))
    probs <- (indegrees+10e-3)^alpha * (depths+10e-3)^beta * taus^(-lambda) 
    #probs <- (indegrees+10e-3)^alpha * (depths+10e-3)^beta * lambda^(taus) # too much cascades
    probs <- probs/sum(probs)
    j <- sample(1:length(probs), 1, prob=probs)
    
    # Add new vertex attached to the chosen node
    g <- add_vertices(g, 1, time=i)
    g <- add_edges(g, c(i,j))
  } 
  g
}

if(TRUE){
  par(mfrow=c(1,1))
  for (i in seq(0,1,length=12)){
    alpha <- runif(1)
    beta <- runif(1)
    lambda <- runif(1)
    alpha <- 0.75
    beta <- 0.1
    lambda <- 0.1
    g <- gen.thread(n=100, alpha=alpha, beta=beta, lambda=lambda)
    # Plot
    la <- layout_with_fr(g)
    plot(as.undirected(g),
         layout = la, 
         vertex.label = "",
         vertex.color = "black",
         vertex.size = 1.5 + 1.5 * log( graph.strength(g), 3 ), 
         edge.width = 1.5, 
         asp=9/16,
         margin=-0.15)
    #title(bquote(alpha ~ '=' ~ .(round(alpha, digits=2)) ~ ';' ~
    #             beta ~ '=' ~ .(round(beta, digits=2)) ~ ';' ~ 
    #             lambda ~ '=' ~ .(round(lambda, digits=2))
    #            )
    #      )
  }
}
