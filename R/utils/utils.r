# Some useful functions
#
# author: Alberto Lumbreras
###########################################################################


graph.reverse <- function (graph) {
  # Reverse graph edges
  # in graph 1.0 trees cannot be properly plotted if nodes point towards the root.
  # Use this function so that igraph 1.0 can plot it.
  if (!is.directed(graph))
    return(graph)
  
  e <- get.data.frame(graph, what="edges")
  
  ## swap "from" & "to"
  neworder <- 1:length(e)
  neworder[1:2] <- c(2,1)
  e <- e[neworder]
  names(e) <- names(e)[neworder]
  graph.data.frame(e, vertices = get.data.frame(graph, what="vertices"))
}



plot.hist <- function(values, name="values"){
  par(mfrow=c(1,3))
  
  # Histograms of power laws are usually useless
  # hist(values, plot=T, breaks=c(seq(0,max(values)+1, .1)), xlab=name, main=NULL)
  
  h <- hist(values, plot=F, breaks=c(seq(0,max(values)+1, .1)))
  probs <- h$counts/sum(h$counts)
  plot(probs, log="xy", pch=20, col="blue",
       xlab=paste0("log(",name,")"), ylab="log(prob)", yaxt="n")
  axis(2, las=1, at=10^c(0,-1,-2,-3, -4), labels=expression(1, 10^-1, 10^-2, 10^-3,  10^-4))
  
  plot(cumsum(sort(values, decreasing=T)), col="blue", pch=".", xlab="id", ylab=paste0("cumsum(", name, ")"))
  
  plot(1: length(values), sort(values, decreasing=T),
       log="y", pch=20, col="blue", xlab="id", ylab=name, las=1)
  
  title(name, outer=TRUE, line=-1)
}

plot.cluster.profiles <- function(df, z){
  # Plot bars with means and variances of features for every cluster
  # Args:
  #   df: dataframe with feature values (one individual per row)
  #   z: cluster assignment
  
  par(mfrow=c(1,1))
  k <- max(z)
  colors <- rainbow(k)
  par(mfrow=c(1,1))
  means <- NULL
  variances <- NULL
  for(i in 1:k){
    means <- rbind(means, colMeans(df[z == i, , drop = FALSE]))
    variances <- rbind(variances, apply(df[z==i,], 2, var))
  }

  x <- 1:ncol(means)
  i <- 1
  y <- as.numeric(means[i,])
  sd <-sqrt(as.numeric(variances[i,]))

  ymax = max(means+variances)
  #barx <- barplot(means, col=colors, names.arg = colnames(df), beside=T, ylim=c(0,ymax*1.25))
  barx <- barplot(means, col=colors, names.arg = colnames(df), beside=T, ylim=c(0,50))
  arrows(barx[1,], means[1,]+variances[1,], barx[1,], means[1,],  angle=90, code=1)
  arrows(barx, means+variances, barx, means,  angle=90, code=1)
  
  
  # Separated plots
  par(mfrow=c(2,1))
  epsilon <- 0.02
  for (i in 1:k){
    y <- as.numeric(means[i,])
    plot(x, y, type="l", ylim=c(-1,1))
    
    sd <-sqrt(as.numeric(variances[i,]))
    segments(x,y-sd,x,y+sd)
    segments(x-epsilon,y-sd,x+epsilon,y-sd)
    segments(x-epsilon,y+sd,x+epsilon,y+sd)
    title(paste("cluster", sum(z==i)))
  }

}

remove.outliers <- function(df, threshold=0.99){
  # Remove rows where some feature is an outlier
  #
  # Args:
  #    df: dataframe
  # 
  # Return:
  #    dataframe without the outlier rows
  outlier <- rep(FALSE, nrow(df))
  for(i in 1:ncol(df)){
    max <- as.numeric(quantile(df[,i], threshold))
    outlier <- outlier | (df[,i]>max)
  }
  df <- df[!outlier,]
  df
}
