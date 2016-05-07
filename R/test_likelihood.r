# Generates graphs with some parameters
# Find the MLE of the graphs
# Re-generate graphs with those MLE to see whether they look like the originals.
#
# author: Alberto Lumbreras

library(igraph)
source("thread_generator.r")
source("likelihood.r")

#### Test likelihood
#########################
# Clear examples:
#alpha <- 1.2
#beta <- 50
#lambda <- 0

par(mfrow=c(3,3))
for (i in seq(0,1,length=9)){
  alpha <- 1.2
  beta <- 50
  lambda <- 0
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
  title(bquote(alpha ~ '=' ~ .(round(alpha, digits=2)) ~ ';' ~
                 beta ~ '=' ~ .(round(beta, digits=2)) ~ ';' ~ 
                 lambda ~ '=' ~ .(round(lambda, digits=2))
              ), outer=TRUE, line=-1)
}

# Find MLE by grid search
alpha_ <- seq(0.5,5, by=0.1)
beta_ <- seq(30,200, by=3)
xy <- merge(alpha_, beta_)
like <- rep(NA, nrow(xy)) 
for(i in 1:nrow(xy)){
  like[i] <- likelihood(c(xy[i,1], xy[i,2]), g)
}


# plot grid
par(mfrow=c(1,1))
plot(xy[,1], xy[,2], cex=exp(like-max(like)), xlab=expression(alpha), ylab=expression(beta))
title("Grid search of MLE")

# plot grid
par(mfrow=c(1,1))
plot(xy[,1], xy[,2], cex=exp((like-max(like))/sd(abs(like))), xlab=expression(alpha), ylab=expression(beta))
title("Grid search of MLE")

# Generate graphs with MLE parameters
par(mfrow=c(3,3))

alpha_mle <- xy[which.max(like),1]
beta_mle <- xy[which.max(like),2]

for (i in seq(0,1,length=9)){
  lambda <- 0
  g <- gen.thread(n=100, alpha=alpha_mle, beta=beta_mle, lambda=0)
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
  title(bquote(alpha ~ '=' ~ .(round(alpha_mle, digits=2)) ~ ';' ~
                 beta ~ '=' ~ .(round(beta_mle, digits=2)) ~ ';' ~ 
                 lambda ~ '=' ~ .(round(lambda, digits=2))
    )
  )
  title("Graphs with MLE parameters", outer=T, line=-1)
}
