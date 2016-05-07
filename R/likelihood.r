# Computes the likelihood of a graph according to our model
# author: Alberto Lumbreras
#############################
# TODO: make this as fast as possible.

parents.vector <- function(g){
  # Build a vector representation of tree graph g
  # where position i contains parent of i-th node
  # See:
  # Gomez, V., Kappen, H. J., Litvak, N., & Kaltenbrunner, A. (2012). 
  # A likelihood-based framework for the analysis of discussion threads. World Wide Web (Vol. 16). 
  # http://doi.org/10.1007/s11280-012-0162-8
  nodes <- order(V(g)$date)[-1] # exclude root
  parents <- get.edgelist(g, names=FALSE)[,2][nodes-1]
  #parents <- unlist(adjacent_vertices(g, nodes, mode = c("out"))) #bottleneck
  parents
}

degrees.3 <- function(parents){
  # Computes the degrees of every node using the parents vector
  # (slower in long parent vectors)
  degrees <- rep(0, length(parents)+1)
  for(i in 1:length(degrees)){
    degrees[parents[i]] <- degrees[parents[i]] + 1
  }
  degrees
}

degrees <- function(parents){
  # Computes the degrees of every node using the parents vector
  degrees <- hist(parents, plot=F, breaks=c(seq(0,length(parents)+1)))$counts
  degrees
}

depths <- function(parents){
  # Computes the depths of every node using the parents vector
  # depth: depth = parentdepth + 1
  depths <- rep(0, length(parents)+1)
  for(i in 1:length(parents)){
    depths[i+1] <- depths[parents[i]] + 1 # bottleneck
  }
  depths
}

likelihood <- function(params, trees) {
  # Computes total likelihood of a graph
  # given the parameters
  #
  # Args:
  #   params: vector with alpha, beta and lambda parameters
  #   g: observed graph
  #
  # Returns:
  #   loglikelihood of the graph
  alpha <- params[1]
  beta <- params[2]
  lambda <- params[3]
  
  # if only one tree, wrap it in a list
  # so that next 'for' loop works fine
  if(class(trees) == "igraph"){
    trees <- list(g)
  }
  
  total.like <- 0
  for(i in 1:length(trees)){
    g <- trees[[i]]
    parents <- parents.vector(g)
    
    # likelihood of every parent choice
    like <- 0
    for (t in 2:length(parents)){
      h <- depths(parents[1:(t-1)])
      d <- degrees.3(parents[1:(t-1)])
      #tau <- t - as.numeric(V(G)$date[i]) # how old is the post
      
      probs <- (d+10e-5)^alpha * (h+10e-5)^beta #* taus^(-lambda) 
      probs <- probs/sum(probs)
      like <- like + log(probs[parents[t]])
    }
    total.like <- total.like + like
  }
  total.like
}

########################################
# BENCHMARKS
#######################################"
if(TRUE){
  library(utils)
  Rprof(line.profiling=TRUE)
  replicate(n=100, likelihood(c(1,1,1), g))
  Rprof(NULL)
  #summaryRprof(lines="both")
  summaryRprof(lines="show")
}


####################"
library(microbenchmark)

#t1 <- replicate(n=1000, system.time(degrees.1(pparents))[3])
#t2 <- replicate(n=1000, system.time(degrees.2(pparents))[3])


#microbenchmark(depths.1(parents), depths.2(parents))
