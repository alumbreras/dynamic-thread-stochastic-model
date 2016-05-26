# Generates artificial threads
# store them in igraph objects 
# and in dataframe representations useful to compute the likelihood
# author: Alberto Lumbreras
################################
library(igraph)
library(parallel)
library(data.table)
source('R/plotting.r')
source('R/thread_generators.r')
models <- c("Gomez2011", "Gomez2013", "Lumbreras2016unicluster", "Lumbreras2016")

ntrees <- 10000
ncores <- detectCores() - 2
model <- models[3]

##########################################
# Chose one of the models below
##########################################
# Gomez 2011
if(model == "Gomez2011"){
  alpha.root <- 0.734
  alpha.c <- 0.683
  beta.root <- 1.302
  trees <- replicate(ntrees,  gen.thread.Gomez2011(n=25, alpha.root=alpha.root, alpha.c=alpha.c, beta.root=beta.root), simplify = FALSE)
}

# Gomez 2013
if(model == "Gomez2013"){
  alpha <- 1
  beta <- 0.68
  tau <- 0.75
  trees <- replicate(ntrees, gen.thread.Gomez2013(n=25, alpha=alpha, beta=beta, tau=tau), simplify = FALSE)
}

# Lumbreras 2016 one cluster, one user
if(model == "Lumbreras2016unicluster"){
  users.by.cluster <- 50 
  alphas <- 1
  betas <- 0.68
  taus <- 0.75
  z <- do.call(c, lapply(1:length(alphas), function(x) rep(x, users.by.cluster)))
  
  # parallel
  cl <- makeCluster(ncores)  
  clusterEvalQ(cl, {library(igraph); source('R/thread_generators.r')})
  clusterExport(cl, c("alphas", "betas", "taus", "z"))
  trees <- parLapply(cl, 1:ntrees, function(i) gen.thread.Lumbreras2016(n=25, z=z, alphas=alphas, betas=betas, taus=taus) )
  stopCluster(cl)
}

# Lumbreras 2016
if(model == "Lumbreras2016"){
  users.by.cluster <- 1000 
  alphas <- c(0.1, 0.5, 1)
  betas <- c(1, 5, 10)
  taus <- c(0.1, 0.5, 0.99)
  z <- do.call(c, lapply(1:length(alphas), function(x) rep(x, users.by.cluster)))

  # parallel
  cl <- makeCluster(ncores)  
  clusterEvalQ(cl, {library(igraph); source('R/thread_generators.r')})
  clusterExport(cl, c("alphas", "betas", "taus", "z"))
  trees <- parLapply(cl, 1:ntrees, function(i) gen.thread.Lumbreras2016(n=25, z=z, alphas=alphas, betas=betas, taus=taus) )
  stopCluster(cl)
}


save(trees, file='trees.Rda')

##########################################################
# Create dataframe from a set of trees
############################################################
# Prepare all the information needed into a nice dataframe
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
  