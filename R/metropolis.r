# Metropolis-Hasting algorithms to infer the model parameters
# author: Alberto Lumbreras

source('likelihood.r')
library(mvtnorm)

############################"
# PRobably prepare parents vector, depths, so that we dont have to recompute it 
###########################
##########################
# Metropolis-Hastings
##########################
prior <- function(params, params_0){
  params.prior <- dmvnorm(params, mean = params_0, sigma=diag(c(1000,1000,1000)), log=TRUE)
  return(params.prior)
}

posterior <- function(params, trees){
  return(likelihood(params, trees) + prior(params, params_0))
}

proposal.function <- function(params){
  stopifnot(length(params)==length(steps))
  return(rmvnorm(1, mean=params, sigma=diag(steps))) # steps are sd or variance?
}

metropolis <- function(nsamples, trees){
  chain <- matrix(0, nsamples, 3)
  chain[1,] <- params_0 # initial state
  map <- matrix(0, nsamples, 4)
  
  for(i in 2:nsamples){
    proposal <- proposal.function(chain[i-1,])
    like.new <- posterior(proposal, trees)
    like.old <- posterior(chain[i-1,], trees)
    ratio <- exp(like.new - like.old)
    
    map[i,] <- c(proposal, like.new)
    cat("\nsample:", i, nsamples)
    cat("\n chain:", chain[i-1,], like.old)
    cat("\n proposal:", proposal, like.new)
    cat("\n acceptance prob:", ratio)
    
    if (runif(1) < ratio){
      chain[i,] <- proposal
      cat("\n********")
    }
    else{
      chain[i,] <- chain[i-1,]
    }
    
  }  
  colnames(chain) <- c("alpha","beta","lambda")
  colnames(map) <- c("alpha","beta","lambda", "likelihood")
  res <- list()
  res$chain <- chain
  res$map <- map[-1,]
  return(res)
  
}

nsamples <- 1000
steps <- c(0.001, 0.001, 0.1)
params_0 <- c(1,50,0)

res <- metropolis(nsamples, trees)
chain <- res$chain
burn <- 10
acceptance.rate <- 1 - mean(duplicated(chain[-(1:burn),]))
cat("\n Acceptance rate:", acceptance.rate)

map <- res$map

# Plot trace
par(mfrow=c(2,1))
plot(chain[,1], chain[,2], xlab=expression(alpha), ylab=expression(beta))
title("Accepted samples")

colors <- (map[,4]-max(map[,4]))/max(abs(map[,4]))+1
plot(map[,1], map[,2], cex=colors, xlab=expression(alpha), ylab=expression(beta))
title("Explored samples")