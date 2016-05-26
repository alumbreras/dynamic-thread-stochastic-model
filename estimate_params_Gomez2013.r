# Check that we can properly recover the true parameters
# author: Alberto Lumbreras
#########################################################
library(parallel)
library(data.table)
library(neldermead)
library(dfoptim)
source('R/thread_generators.r')
source('R/likelihood.r')

###############
# generate N trees with fixed parameters
###############
N <- 50
alpha <- 1
beta <- 0.68
tau <- 0.75 
trees <- replicate(N, gen.thread.Gomez2013(n=25, alpha=alpha, beta=beta, tau=tau), simplify = FALSE)

# Create one dataframe from each trees
ncores <- detectCores() - 2
cl<-makeCluster(ncores, outfile="", port=11439)
clusterEvalQ(cl, {library(igraph); library(dplyr)})
data.parlapply <- parLapply(cl, trees, tree.to.data)
stopCluster(cl)

# join dataframes adding thread id
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


########################################
# Estimation
########################################
Qopt <- function(params, df.trees){
  sum(apply(df.trees, 1, function(x) likelihood.post(x, params[1], params[2], params[3])))
}

sol <- nmkb(c(0.5, 0.5, 0.5), Qopt, 
            lower = c(0,0,0), upper = c(Inf, Inf, 1), 
            control = list(maximize=TRUE),
            df.trees = df.trees)

cat('\n', sol$par)

