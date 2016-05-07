# Metrics and methods to compute metrics in a set of users, posts or threads
#
# author: Alberto Lumbreras
# created: October 9, 2015
#############################################################################

###############################################################
# Graph metrics
###############################################################

degree.depth.distribution <- function (g) 
{
  if (!is_igraph(g)) {
    stop("Not a graph object")
  }
  degrees <- degree(g)
  depths <- shortest.paths(g, to=1) 
  cs <- depths*degrees
  res <- hist(cs, -1:max(cs), plot = FALSE)$density
  res
}

depth.distribution <- function (g) 
{
  if (!is_igraph(g)) {
    stop("Not a graph object")
  }
  cs <- shortest.paths(g, to=1) 
  res <- hist(cs, -1:max(cs), plot = FALSE)$density
  res
}

hindex <- function(g){
  # h-index of a tree graph
  root <- V(g)[igraph::degree(g, mode="out")==0]
  levels <- shortest.paths(g)[,root]
  h <- 0
  for(i in 1:vcount(g)){
    if(sum(levels==i)<i){
      h <- i-1
      break
    }
  }
  return(h)
}

maxposts <- function(g){
  # Number of posts of the more prolific poster
  max(table(V(g)$user))
}

branching <- function(g){
  # Average branching factor
  sum(igraph::degree(g, mode="in")>1)/vcount(g)
}

 
###############################################################
# Bulk metrics that create dataframe with one row per case
###############################################################

threads.metrics <- function(threadids, dataset="reddit"){
  # Returns a dataframe with a set of metrics for every discussion thread
  # Args:
  #   dataset: name of dataset (it will look for a dataset.db file)
  #   threadids: id of threads to compute
  #
  # Returns:
  #   Dataframe with nthread rows and one column per metric, plus # of users, # of posts and thread id.
  nthreads <- length(threadids)
  res <- data.frame(matrix(NA, nrow=nthreads, ncol=23))
  names(res) <- c("gu.transitivity",  "gp.transitivity", 
                  "gu.diameter",      "gp.diameter",
                  "gu.avgdegree",     "gp.avgdegree",
                  "gu.avgpathlength", "gp.avgpathlength",
                  "gu.density",       "gp.density",
                  "gu.reciprocity",   "gp.reciprocity",
                  "gu.assortativity", "gp.assortativity",
                  "gu.modularity",    "gp.modularity",                  
                  "gp.hindex",
                  "gp.maxposts",
                  "gp.branching",
                  "nusers",
                  "nposts",
                  "duration",
                  "threadid")
  
  for(t in 1:nthreads){
    threadid <- threadids[t]
    graphs <- database.to.graph(threadid, con, dataset)
    if ('try-error' %in% class(res)){
      cat("\nERROR. Couldn't create graph from database. Skip thread\n")
      next
    } 
    cat("", t)
    gp <- graphs$gp
    gu <- graphs$gu
    plot(graphs)
    
    res[t,]$threadid <- threadid
    res[t,]$gu.transitivity <- transitivity(gu)
    res[t,]$gp.transitivity <- transitivity(gp)
    res[t,]$gu.diameter <- diameter(gu)
    res[t,]$gp.diameter <- diameter(gp)
    res[t,]$gu.avgdegree <- mean(igraph::degree(gu))
    res[t,]$gp.avgdegree <- mean(igraph::degree(gp))
    res[t,]$gu.avgpathlength <- average.path.length(gu)
    res[t,]$gp.avgpathlength <- average.path.length(gp)
    res[t,]$gu.density <- graph.density(gu)
    res[t,]$gp.density <- graph.density(gp)
    res[t,]$gu.reciprocity <- reciprocity(gu)
    res[t,]$gp.reciprocity <- reciprocity(gp)
    res[t,]$gu.assortativity <- assortativity.degree(gu) 
    res[t,]$gp.assortativity <- assortativity.degree(gp)    
    res[t,]$gu.modularity <- length(unique(multilevel.community(as.undirected(gu))$membership))
    res[t,]$gp.modularity <- length(unique(multilevel.community(as.undirected(gp))$membership))
    
    res[t,]$gp.hindex <- hindex(gp)
    res[t,]$gp.maxposts <- maxposts(gp)
    res[t,]$gp.branching <- branching(gp)
    
    res[t,]$nusers <- vcount(gu)
    res[t,]$nposts <- vcount(gp)
    res[t,]$duration <- max(as.numeric(V(gp)$date)) - min(as.numeric(V(gp)$date))
  }
  
  res
  
}

users.metrics <- function(userids, dataset="reddit"){
  # Returns a dataframe with a set of metrics for every discussion thread
  # Args:
  #   dataset: name of dataset (it will look for a dataset.db file)
  #   threadids: id of threads to compute
  #
  # Returns:
  #   Dataframe with nthread rows and one column per metric, plus # of users, # of posts and thread id.
  nusers <- length(userids)
  vars <- c("forums", "threads", 
            "posts", "words", "wordsperpost",
            "postsperthread", "sd.postsperthread", "med.postsperthread", 
            "indegree.ratio", "post.replied.ratio",
            "bidirectional.threads.ratio", "bidirectional.neighbors.ratio",
            "roots", "percroot", "depth",  "med.depth",
            "maxposts", "userid")
  res <- data.frame(matrix(NA, nrow=nusers, ncol=length(vars)))
  names(res) <- vars
  
  indegree.ratio <- vector()
  post.replied.ratio <- vector()
  bidirectional.threads.ratio <- vector()
  bidirectional.neighbors.ratio <- vector()
  
  for(u in 1:nusers){
    userid <- userids[u]
    cat("\n User:", u)
    
    query <- paste0("select * from posts where user =  '", userid, "'")
    posts <- dbGetQuery(con, query)
    nthreads <- length(unique(posts$thread))
    nposts <- nrow(posts)
    words <- unlist(strsplit(paste(posts$text, collapse=' '), ' ') )
    nwords <- length(words)
    
    res[u,]$userid <- userid
    res[u,]$threads <- nthreads
    res[u,]$posts <- nposts
    res[u,]$words <- nwords
    res[u,]$wordsperpost <- nwords/nposts
    res[u,]$postsperthread <- nposts/nthreads
    res[u,]$std.postsperthread <- sd(as.numeric(table(posts$thread)))
    res[u,]$med.postsperthread <- median(as.numeric(table(posts$thread)))
    res[u,]$maxposts <- max(as.numeric(table(posts$thread)))
    
    
    # metrics that depend on thread structure
    threads <- unique(posts$thread)
    depths <- vector()
    tnposts <- vector()
    indegree.ratio <- vector()
    posts.replied <- vector()
    bidirectional.threads.ratio <- vector()
    bidirectional.neighbors.ratio <- vector()
    roots <- vector()
    for (t in 1:nthreads){
      query <- paste0("select count(*) from posts where thread =  '", threadid, "'")
      size <- dbGetQuery(con, query)
      
      cat(paste0(" ", t, "/", nthreads, " (", size,")"))
      threadid <- threads[t]
      graphs <- database.to.graph(threadid, con, dataset)
      if ('try-error' %in% class(graphs)){
        cat("!") #cat("\nERROR. Couldn't create graph from database. Skip thread\n")
        next
      }
      gp <- graphs$gp
      gu <- graphs$gu
      
      # Root
      query <- paste0("select root from threads where threadid =  '", threadid, "'")
      root <- dbGetQuery(con, query)
      if(nrow(root)==0){
        cat("*") #cat("Skipping incomplete thread")
        next # thread beginning was not crawled
      }
      root <- as.character(root)
      tposts <- which(V(gp)$user==userid)
      troot <- which(V(gp)$name==root)
      
      # Depths of posts
      depths <- c(depths, as.numeric(distances(gp, v=tposts, to=troot, mode="out")))
      roots <- c(roots, troot %in% tposts)
      
      # Indegree ratio
      indegree.ratio <- c(indegree.ratio, degree(gu, v=userid, mode="in")/ecount(gu))
      
      # Posts replied ratio
      posts.replied <- c(posts.replied, sum(degree(gp, v=tposts, mode="in")>0))

      # Bi-directional threads ratio
#       bidir <- 0
#       for(p in 1:length(tposts)){
#         post <- tposts[p]
#         parent <- ego(gp, 1, nodes=post, mode="out")
#         children <- ego(gp, 1, nodes=post, mode="in")
#         if (parent %in% children){
#           bidir <- bidir + 1
#         }
#       }      
      # TODO
      #Bi-directional neighbors ratio
      
      
    }
    
    res[u,]$depth <- mean(depths)
    res[u,]$percroot <- sum(roots)/nthreads
    res[u,]$roots <- sum(roots)
    res[u,]$med.depth <- median(depths)
    
    # todo: nforums
    chunk <- paste0("'", paste0(threads, collapse = "','"), "'")
    query <- paste0("select distinct forum from threads where threadid in  (", chunk, ")")
    forums <- dbGetQuery(con, query)
    res[u,]$forums <- nrow(forums)
    
  }
  
  res
  
}

posts.metrics <- function(postids, dataset="reddit"){
  # Returns a dataframe with a set of metrics for every post
  # Args:
  #   dataset: name of dataset (it will look for a dataset.db file)
  #   threadids: id of threads to compute
  #
  # Returns:
  #   Dataframe with nthread rows and one column per metric, plus # of users, # of posts and thread id.
  library(data.table)
  nposts <- length(postids)  
  res <- data.table(postid = character(nposts*100),
                    thread = character(nposts*100),
                    user = character(nposts*100),
                    date = integer(nposts*100),
                    words = integer(nposts*100),
                    depth = integer(nposts*100), 
                    children = integer(nposts*100),
                    descendants = integer(nposts*100),
                    timetoparent = integer(nposts*100),
                    timetofirstchild = integer(nposts*100),
                    timetolastchild = integer(nposts*100),
                    timetolastdescendant = integer(nposts*100),
                    timetolastpost = integer(nposts*100))
  res[,] <- NA
  
  chunk <- paste0("'", paste0(postids, collapse = "','"), "'")
  threadids <- dbGetQuery(con, paste0("select distinct thread from posts where postid in  (", chunk, ")"))
  nthreads <- nrow(threadids)
  
  p <- 1 # count number of posts  
  # Compute by thread to be more efficient
  for (t in 1:nthreads){
    threadid <- threadids[t,]
    cat(paste0("\n\n\n**********threads: ", t, "/", nthreads, " - ", threadid))
    graphs <- database.to.graph(threadid, con, dataset)
    if ('try-error' %in% class(graphs)){
      cat("\nERROR. Couldn't create graph from database. Skip thread\n")
      next
    }
    plot(graphs, title=threadid)
    gp <- graphs$gp
    root <- which(degree(gp, mode="out")==0)
    init.time <- V(gp)[root]$date
    end.time <- max(V(gp)$date)
    
    cat("\nnposts: ", vcount(gp))
    for (i in 1:vcount(gp)){
      post <- V(gp)[i]
      parent <- neighbors(gp, v=i, mode="out")
      if(length(parent)==0){parent <- post} # if root, parent itself
      children <- neighbors(gp, v=i, mode="in")
      descendants <- ego(gp, 1000, nodes=i, mode="in", mindist=0)[[1]]
      nchildren <- length(children)
      ndescendants <- length(descendants)-1
      
      res[p, "postid":=post$name]
      res[p, "thread":=threadid]
      res[p, "user":=post$user]
      res[p, "date":=as.numeric(post$date)]
      res[p, "words":=length(unlist(strsplit(paste(post$text, collapse=' '), ' ') ))]
      res[p, "depth":=as.numeric(distances(gp, v=i, to=root, mode="out"))]
      res[p, "children":=nchildren]
      res[p, "descendants":= ndescendants] # exclude itself
      res[p, "timetoparent":=as.numeric(post$date) - as.numeric(parent$date)]
      
      #cat("\n ", post$name,"-",length(children))
      
      if(length(children)>0){
        val <- min(as.numeric(children$date)) - as.numeric(post$date)
        res[p, "timetofirstchild":=val]
        val <- max(as.numeric(children$date)) - as.numeric(post$date)
        res[p, "timetolastchild":=val]
        val <- max(as.numeric(descendants$date)) - as.numeric(post$date)
        res[p, "timetolastdescendant":=val]
      }else{
        res[p, "timetofirstchild":=0]
        res[p, "timetolastchild":=0]
        res[p, "timetolastdescendant":=0]
      }
      val <- max(as.numeric(V(gp)$date)) - as.numeric(post$date)
      res[p, "timetolastpost":=val] 
      
      p <- p+1
    }
  }
  res
}
