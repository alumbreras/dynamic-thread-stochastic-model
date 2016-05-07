#
# Plotting functions
#


plot.tree <- function(gtree, labels=c('name', 'id')){
  # Plots a tree graph
  # Arguments:
  #   gtree: a igraph object graph with no cycles (tree)
  if (missing(labels)){
    labels <- NA
  }
  else{
  labels <- switch(labels, 
                   'name' = V(gtree)$name, 
                   'id' = as.numeric(V(gtree)))
  }
  par(mfrow=c(1,1))
  gtree.un <- as.undirected(gtree)
  la = layout_as_tree(gtree.un, mode='out', root=which.min(V(gtree.un)$date))

  plot(gtree.un,
       layout = la,
       vertex.label = labels,
       vertex.size=3,
       edge.arrow.size=0.6)
}