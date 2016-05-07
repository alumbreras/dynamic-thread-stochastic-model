# Generates artificial threads
# Note: it make no sense to generate threads like that and it is normal that the generated threads
# do not look like real ones. 
# Update 22/11/2015: Are you sure?
###############################

library(igraph)

gen.thread.snapshot <- function(params=c(1000,3,3), timespan=3600*24){
  # Generate artifitial thread
  # the thread grows every hour
  G <- graph.empty(n=1)
  V(G)$date[1] <- 0
  
  theta <- params[1]
  lambda_l <- params[2]
  lambda_d <- params[3]
  k <- 1.5 # fixed parameter
  for (t in seq(1,timespan, by=3600)){
    for(i in 1:vcount(G)){
     
      # Node properties
      l <- length(unlist(ego(G, 10000, nodes=i, mode="out"))) - 1
      d <- length(unlist(ego(G, 1, nodes=i, mode="in"))) - 1 # how many replies
      tau <- t - V(G)$date[i] # how old is the post
      
      # Probability of getting a response  
      like <-  dgamma(tau, shape= k, scale = theta)
      like <- like * dpois(l, lambda_l)
      like <- like * dpois(d, lambda_d)
      
      # root has special atraction
      if(i==1){
        like <- 0.5
      }

      # Generate response (or not)
      cat("\n prob link in", i, like)
      if(runif(1)<like){
        G <- add_vertices(G, 1, date=t)
        j <- vcount(G)
        G <- add_edges(G, c(j,i))
      }
    }
  }
  G
}

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
